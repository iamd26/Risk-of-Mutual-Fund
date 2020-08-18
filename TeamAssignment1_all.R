# Team Assignment I


# Part I
# Question 1 ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(MASS)
library(metRology)
library(quantmod)
library(xts)

data <- read.csv("MQM530-TeamAssignment1-logret.csv")

#get mean and sd of daily log return
mu <- mean(data$logret)
sig<- sd(data$logret)
fmt <- "%s %9.6f"
sprintf (fmt,"Mean: ",mu)
sprintf (fmt,"SD:  ",sig)

# calculate VaR and ES with equations
alpha <- 0.95
VaR <- qnorm(1-alpha,mu,sig)
ES  <- mu - sig*dnorm(qnorm(1-alpha,0,1),0,1)/(1-alpha)
fmt <- "%s %9.6f"
sprintf (fmt,"VaR: ",VaR)
sprintf (fmt,"ES:  ",ES)

#"VaR:  -0.010575" meaning that 5% of future outcomes will be lower than -0.010575
#"ES:   -0.013315" meaning that if VaR happens, the expected outcome will be -0.013315
#given the mean = 0.000213, and sd = 0.006558, those results of VaR and ES are reasonable.

# End of Question 1----------------------------------------------------------

# Start of Q2 & Q3 ----------------------------------------------------------
#loading packages
library(data.table)
library(ggplot2)
library(moments)
library(quantmod)
library(rugarch)
library(xts)
library(nloptr)

# F is the price of the underlying asset 
# X is the strike price of the option
# t is the time to maturity (in years)
# r is the tbill rate (in decimal form)
# sigma is the volatility of the underlying asset 
# BFC is the price of the call option
# BFP is the price of the put option
# IVC is the implied volatility of the call option 
# IVP is the implied volatility of the put option

#Defining functions
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}

CalcVaRES <- function(r,alpha) {
  VaR <- quantile(r,1-alpha)
  ES  <- mean(r[r<VaR])
  VaR_ES <- c(VaR,ES)
  names(VaR_ES) <- c("VaR","ES")
  return(VaR_ES)
}

BFC <- function(F,X,t,r,sigma) {
  d1 <- log(F/X) + 0.5*sigma^2 *t
  d1 <- d1/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  N1 <- pnorm(d1)
  N2 <- pnorm(d2)
  C <- exp(-r*t) * (F * N1 - X * N2 )
  return(C)
}

BFP <- function(F,X,t,r,sigma) {
  d1 <- log(F/X) + 0.5*sigma^2 *t
  d1 <- d1/(sigma*sqrt(t))
  d2 <- d1 - sigma*sqrt(t)
  NM1 <- pnorm(-d1)
  NM2 <- pnorm(-d2)
  P <- exp(-r*t) * (X * NM2 - F * NM1 )
  return(P)
}

IVC <- function(F,X,t,r,Call) {
  eval_f_C <- function(sigma) {
    return ( (Call-BFC(F,X,t,r,sigma))^2 )
  }
  opts <- list("algorithm"="NLOPT_LN_COBYLA",
               "xtol_rel"=1.0e-8)
  xs <- 0.10
  es <- nloptr( x0=xs,
                eval_f=eval_f_C,
                opts=opts)
  return(es$solution)
}

IVP <- function(F,X,t,r,Put) {
  eval_f_P <- function(sigma) {
    return ( (Put-BFP(F,X,t,r,sigma))^2 )
  }
  opts <- list("algorithm"="NLOPT_LN_COBYLA",
               "xtol_rel"=1.0e-8)
  xs <- 0.10
  es <- nloptr( x0=xs,
                eval_f=eval_f_P,
                opts=opts)
  return(es$solution)
}

bill.price <- function(settle,mature,discount.rate,redemption=100) {
  P <- redemption*(1-discount.rate*(as.numeric(mature-settle)/360) )
  return(P)
}

# Question 2 ----------------------------------------------------------------
#Reading the data
holdings<-read.csv("MQM530-TeamAssignment1-fund_holdings.csv")
holdings$Date<-as.Date(holdings$Date,format="%Y-%m-%d")
holdings$Expiration<-as.Date(holdings$Expiration,format="%Y-%m-%d")

#Calculate the time to maturity
holdings$t<-as.numeric(holdings$Expiration-holdings$Date)/365

calls<-subset(holdings,Call.Put==1)
puts<-subset(holdings, Call.Put==2)

#IV for calls
calls$IV<-0
for(i in 1:nrow(calls)){
  calls$IV[i]<-IVC(calls$FutPrice[i],calls$Strike[i],calls$t[i],calls$r[i],calls$OptPrice[i])
}

#IV for Put
puts$IV<-0
for(i in 1:nrow(puts)){
  puts$IV[i]<-IVP(puts$FutPrice[i],puts$Strike[i],puts$t[i],puts$r[i],puts$OptPrice[i])
}

#combining and classifying
holdings<-rbind(calls,puts)
holdings$Type<-NA
for (i in 1:nrow(holdings)) {
  if(holdings$Call.Put[i]==1 && holdings$Contracts[i]>0){
    holdings$Type[i]="Purchased Call"
  }
  if(holdings$Call.Put[i]==1 && holdings$Contracts[i]<0){
    holdings$Type[i]="Written Call"
  }
  if(holdings$Call.Put[i]==2 && holdings$Contracts[i]>0){
    holdings$Type[i]="Purchased Put"
  }
  if(holdings$Call.Put[i]==2 && holdings$Contracts[i]<0){
    holdings$Type[i]="Written Put"
  }
}

ggplot(holdings,aes(holdings$Strike,holdings$IV,color=holdings$Type))+geom_line()

# End of Question 2----------------------------------------------------------

# Question 3 ----------------------------------------------------------------
holdings$MarketValue<-abs(holdings$Contracts)*250*holdings$OptPrice
cash<-628226078
Total_Value<-sum(holdings$MarketValue)+cash

#increasing IV by 50%
tmp50<-holdings
tmp50$IV<-tmp50$IV*1.5

for(i in 1:nrow(tmp50)){
  if(tmp50$Call.Put[i]==1){
    tmp50$OptPrice[i]=BFC(tmp50$FutPrice[i],tmp50$Strike[i],tmp50$t[i],tmp50$r[i],tmp50$IV[i])
  }
  if(tmp50$Call.Put[i]==2){
    tmp50$OptPrice[i]=BFP(tmp50$FutPrice[i],tmp50$Strike[i],tmp50$t[i],tmp50$r[i],tmp50$IV[i])
  }
}

tmp50$MarketValue<-abs(tmp50$Contracts)*250*tmp50$OptPrice
delta<-(sum(tmp50$MarketValue[which(tmp50$Type %like% "Purchased")])-sum(holdings$MarketValue[which(holdings$Type %like% "Purchased")]))+
  (-sum(tmp50$MarketValue[which(tmp50$Type %like% "Written")])-(-sum(holdings$MarketValue[which(holdings$Type %like% "Written")])))

FV50<-Total_Value + delta
(FV50-Total_Value)/Total_Value

#increasing IV by 100%
tmp100<-holdings
tmp100$IV<-tmp100$IV*2

for(i in 1:nrow(tmp100)){
  if(tmp100$Call.Put[i]==1){
    tmp100$OptPrice[i]=BFC(tmp100$FutPrice[i],tmp100$Strike[i],tmp100$t[i],tmp100$r[i],tmp100$IV[i])
  }
  if(tmp100$Call.Put[i]==2){
    tmp100$OptPrice[i]=BFP(tmp100$FutPrice[i],tmp100$Strike[i],tmp100$t[i],tmp100$r[i],tmp100$IV[i])
  }
}

tmp100$MarketValue<-abs(tmp100$Contracts)*250*tmp100$OptPrice
delta<-(sum(tmp100$MarketValue[which(tmp100$Type %like% "Purchased")])-sum(holdings$MarketValue[which(holdings$Type %like% "Purchased")]))+
  (-sum(tmp100$MarketValue[which(tmp100$Type %like% "Written")])-(-sum(holdings$MarketValue[which(holdings$Type %like% "Written")])))
FV100<-Total_Value + delta
(FV100-Total_Value)/Total_Value

#increasing by 200%
tmp200<-holdings
tmp200$IV<-tmp200$IV*3

for(i in 1:nrow(tmp200)){
  if(tmp200$Call.Put[i]==1){
    tmp200$OptPrice[i]=BFC(tmp200$FutPrice[i],tmp200$Strike[i],tmp200$t[i],tmp200$r[i],tmp200$IV[i])
  }
  if(tmp200$Call.Put[i]==2){
    tmp200$OptPrice[i]=BFP(tmp200$FutPrice[i],tmp200$Strike[i],tmp200$t[i],tmp200$r[i],tmp200$IV[i])
  }
}

tmp200$MarketValue<-abs(tmp200$Contracts)*250*tmp200$OptPrice
delta<-(sum(tmp200$MarketValue[which(tmp200$Type %like% "Purchased")])-sum(holdings$MarketValue[which(holdings$Type %like% "Purchased")]))+
  (-sum(tmp200$MarketValue[which(tmp200$Type %like% "Written")])-(-sum(holdings$MarketValue[which(holdings$Type %like% "Written")])))
FV200<-Total_Value + delta
(FV200-Total_Value)/Total_Value

#increasing IV by 300%
tmp300<-holdings
tmp300$IV<-tmp300$IV*4

for(i in 1:nrow(tmp300)){
  if(tmp300$Call.Put[i]==1){
    tmp300$OptPrice[i]=BFC(tmp300$FutPrice[i],tmp300$Strike[i],tmp300$t[i],tmp300$r[i],tmp300$IV[i])
  }
  if(tmp300$Call.Put[i]==2){
    tmp300$OptPrice[i]=BFP(tmp300$FutPrice[i],tmp300$Strike[i],tmp300$t[i],tmp300$r[i],tmp300$IV[i])
  }
}

tmp300$MarketValue<-abs(tmp300$Contracts)*250*tmp300$OptPrice
delta<-(sum(tmp300$MarketValue[which(tmp300$Type %like% "Purchased")])-sum(holdings$MarketValue[which(holdings$Type %like% "Purchased")]))+
  (-sum(tmp300$MarketValue[which(tmp300$Type %like% "Written")])-(-sum(holdings$MarketValue[which(holdings$Type %like% "Written")])))
FV300<-Total_Value + delta
(FV300-Total_Value)/Total_Value

#reducing fund value by 50%
d<--Total_Value*0.5
CalIV <- function(n) {
  tmp<-holdings
  tmp$IV<-tmp$IV*n
  for(i in 1:nrow(tmp300)){
    if(tmp$Call.Put[i]==1){
      tmp$OptPrice[i]=BFC(tmp$FutPrice[i],tmp$Strike[i],tmp$t[i],tmp$r[i],tmp$IV[i])
    }
    if(tmp$Call.Put[i]==2){
      tmp$OptPrice[i]=BFP(tmp$FutPrice[i],tmp$Strike[i],tmp$t[i],tmp$r[i],tmp$IV[i])
    }
  }
  tmp$MarketValue<-abs(tmp$Contracts)*250*tmp$OptPrice
  delta<-(sum(tmp$MarketValue[which(tmp$Type %like% "Purchased")])-sum(holdings$MarketValue[which(holdings$Type %like% "Purchased")]))+
    (-sum(tmp$MarketValue[which(tmp$Type %like% "Written")])-(-sum(holdings$MarketValue[which(holdings$Type %like% "Written")])))
  FVX<-Total_Value + delta
  reduce<-(FVX-Total_Value)/Total_Value
  n<-tmp$IV/holdings$IV
  return(reduce)
}
CalIV(2.307024)

# End of Question 3----------------------------------------------------------

# Start of Q4 & Q5 & Q6
# R code to generate pro forma returns for Fund L
library(quantmod)
library(ggplot2)
library(moments)
library(xts)

# portfolio tickers 
Tickers <- c("AAP","CCK","CPRT","CSCO","DECK","EBAY","GOOGL","JCP",
             "KO", "MSI","PFE", "PG",  "SWKS", "TAP", "WBA")
# portfolio weights
Weights <- c(0.04910,0.06043,0.06169,0.10462,0.02770,0.07957,0.10173,0.00627,
             0.07894,0.07918,0.08347,0.06169,0.05792,0.06081,0.08687)

start_date <- "2012-12-31"
end_date <- "2017-12-31"

# retrieve data from Yahoo!Finance
tkr <- Tickers[1]
dat <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
All <- dat[,6]
ntick <- length(Tickers)
for (tkr in Tickers[2:ntick]) {
  dat <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
  All <- merge(All,dat[,6],join="outer")
}
names(All) <- as.character(Tickers[1:ntick])

# create returns for each stock in the portfolio
returns <- diff(log(All))
returns <- returns[-1,]
returns <- exp(returns) - 1

# create the pro forma returns for dates with all stock returns
z <- matrix(returns,nrow=nrow(returns),ncol=ntick)
w <- matrix(Weights,ntick,1)
rp1 <- z %*% w                                     # rp1 is the pro forma return of the portfolio
returns$rp1 <- log(1+rp1)

ret <- returns$rp1
names(ret) <- "logret"

# Question 4 ----------------------------------------------------------------
rdat <- as.vector(ret$logret)
jarque.test(rdat)

#JB result: 
#JB = 265.78, p-value < 2.2e-16
#alternative hypothesis: greater
#  reject normality
#the unconditional distribution of the daily log return is not normal

# End of Question 4----------------------------------------------------------

# Question 5 ----------------------------------------------------------------
# Is there evidence of conditional volatility?
# acf of daily log returns (from Conditional Mean & ARMA)

acf <- acf(ret,main="ACF of daily log return")
acf$acf

aret <- abs(ret)
aacf <- acf(aret,main="ACF of |daily log return|")
aacf$acf

# Volatility clustering: an above average large return tends to be followed by another above average large return; Conditioning information is useful
# Conditional Volatility implies the autocorrelation coefficients of |logret| are positive.
# aacf$acf>0 This is strong evidence of conditional volatility 

# End of Question 5----------------------------------------------------------

# Question 6 ----------------------------------------------------------------
library(rugarch)
uspec.t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
                      distribution.model = "std") # Student t innovations
garch.t <- ugarchfit(spec = uspec.t, data = rdat)

# simulate 1-day ahead from AR(1)-GARCH(1,1) ~ t
set.seed(123789)
nper <- 1
nsim <- 100000
boot.garch <- ugarchboot(garch.t,
                         method=c("Partial","Full")[1],
                         sampling="raw",
                         n.ahead=nper,
                         n.bootpred=nsim,
                         solver="solnp")
sim <- boot.garch@fseries


alpha <- 0.95
VaR_ES <- CalcVaRES(sim,alpha)
round(VaR_ES,6)


# End of Question 6----------------------------------------------------------
