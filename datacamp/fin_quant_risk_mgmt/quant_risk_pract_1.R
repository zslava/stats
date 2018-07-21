#rm(list=ls()); setwd("~/Dropbox/cs/bigdata/datacamp/fin_quant_risk_mgmt")
#source("quant_risk_pract_1.R", echo=T)



require(qrmdata)
require(zoo)
require(xts)

################################
### chapter 1  risk factors
################################

data(SP500)

#head(SP500, n=3)
#tail(SP500, n=3)
## dow jones
data(DJ)

#head(DJ)
#tail(DJ)
#extracting dow jonies for two years 
dj0809 <- DJ["2008-01-01::2009-12-31"]


data(DJ_const)
# 30 stocks that make dow jones
str(DJ_const)

dim(DJ_const)
names(DJ_const)
## get data for 2 stocks
djstocks <- DJ_const["2008/2009",c("AAPL", "GS")]
plot.zoo(djstocks)

data("GBP_USD")
data("EUR_USD")

plot(GBP_USD)
plot(EUR_USD)

fx <- merge(GBP_USD, EUR_USD, all=TRUE)

fx0015 <- fx["2010/2015",]

plot.zoo(fx0015)

### log returns
sp500x <- diff(log(SP500))[-1]  # take off first el

plot.zoo(sp500x)

#log returns for dj2008-2009
dj0809_x <- diff(log(dj0809))[-1]

djstocks_x <- diff(log(djstocks))[-1]
plot.zoo(djstocks_x)

## commodities data

djstocks <- DJ_const["2008/2009",c("AAPL", "AXP", "BA", "CAT")]

plot.zoo(djstocks, plot.type="single", col=c(1,2,3,4))
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

djstocks_x <- diff(log(djstocks))
plot.zoo(djstocks_x)

##aggregate daily log returns to weeks or months, just a sum
sp500x_w <- apply.weekly(sp500x, sum) 
sp500x_m <- apply.monthly(sp500x, sum) 


##commodities
data("GOLD")
data("OIL_Brent")

gold<-GOLD["1990::",]
oil <- OIL_Brent["1990::",]

#plot.zoo(gold)
#plot.zoo(oil)
#log returns
goldx <-diff(log(gold))
oilx <- diff(log(oil))

goldx_m <- apply.monthly(goldx, sum)
oilx_m <- apply.monthly(oilx, sum)

coms <- merge(goldx_m, oilx_m)

plot.zoo(coms, type="h")
pairs(as.zoo(coms))

##interest data
data("ZCB_CA")
zcb <- ZCB_CA["2006::",]
yield_cols <- c("1.00y", "5.00y", "10.00y")
#log returns
zcb_x <- diff(log(zcb))[-1]

plot.zoo(zcb_x[,yield_cols])

#simple returns
zcb_x2 <- diff(zcb)[-1]
plot.zoo(zcb_x2[,yield_cols])

maturity <- (1:120)/4
# Plot the yield curve for the first day of zcb
plot(maturity, zcb[1,], ylim = range(zcb), type = "l",
     ylab = "yield (%)", col = "red")

lines(maturity, zcb[nrow(zcb),])

################################
### chapter 2 returns are riskier than normal
################################
require(moments)

djx <- as.numeric(dj0809_x)
mu <- mean(djx)
sigma <- sd(djx)
par(mfrow=c(1,2))
hist(djx, nclass=20, probability = TRUE)
points(djx, dnorm(djx,mean=mu,sd=sigma), col = "red", cex=0.25)
plot(density(djx),col="blue")  #non-parametric kernel density function
par(mfrow=c(1,1))

# Plot non-parametric KDE of djx

#### qqplots
n<-length(djx)
x1 <- rnorm(n)
x2 <- rt(n, df = 4)
x3 <- runif(n)

par(mfrow=c(2,2))
qqnorm(djx, main="qq for djx")
qqline(djx, col=2)

qqnorm(x1, main="qq for rnorm")
qqline(x1, col="red")

qqnorm(x2, main="qq for student 2 df = 4")
qqline(x2, col="red")

qqnorm(x3, main="qq for uniform")
qqline(x3, col="red")

dev.off()
## skewness and kurtosis
skewness(djx)
kurtosis(djx)

# Carry out a Jarque-Bera test for djx
z<-jarque.test(djx)

djreturns <- diff(log(DJ_const["2008::2011",]))[-1]
s <- apply(djreturns, FUN=skewness, MARGIN=2)
k <- apply(djreturns, FUN=kurtosis, MARGIN=2)

plot(s,k,type="n")
text(s, k, names(s), cex = 0.6)

all_jart <- apply(djreturns, FUN=jarque.test, MARGIN=2)

for (s in 1:dim(djreturns)[2]) {
  print(all_jart[s]$JB)
}

### testing normality for longer time horizons
tickers29 <- c("AAPL","AXP","BA","CAT","CSCO","CVX","DD","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","VZ","WMT","XOM")
djx_d <-  diff(log(DJ_const["1999-12-31::",tickers29]))[-1] # get daiy log returns

djx_w <- apply.weekly(djx_d, colSums) 
djx_m <- apply.monthly(djx_d, colSums) 


apply(djx_d, MARGIN=2, function(v){jarque.test(v)$p.value})
apply(djx_w, MARGIN=2, function(v){jarque.test(v)$p.value})
apply(djx_m, MARGIN=2, function(v){jarque.test(v)$p.value})


jd <- apply(djx_d, MARGIN=2, function(v){jarque.test(v)$statistic})
jw <- apply(djx_w, MARGIN=2, function(v){jarque.test(v)$statistic})
jm <- apply(djx_m, MARGIN=2, function(v){jarque.test(v)$statistic})

##moving winow returns
## prepare data
djx<- diff(log(DJ["2008-01-01::2011-12-31",]))[-1]

djx5 <- rollapplyr(djx, width = 5, FUN = sum)[-(1:4)] # 5-days return ~(weekly)

djx21 <- rollapplyr(djx, width = 21, FUN = sum)[-(1:20)] # 21-days return ~(monthly)

djx63 <- rollapplyr(djx, width = 63, FUN = sum)[-(1:62)] # 21-days return ~(trimestre)

djx2  <- merge(djx5, djx21, djx63, all=FALSE)
colnames(djx2) <- c("dji5", "dji21", "dji63")
plot.zoo(djx2)


s <- apply(djx2, MARGIN=2, function(v){skewness(v,na.rm=TRUE)})
k <- apply(djx2, MARGIN=2, function(v){kurtosis(v,na.rm=TRUE)})
jt <- apply(djx2, MARGIN=2, function(v){jarque.test(v)$statistic} )

### fitting t-distribution to data
# a student t distribution is generally a much better fit to daily, weekly 
# and monthly returns
library(QRM)
data("FTSE")

dj_x <- diff(log(DJ["2008-01-01::2011-12-31",]))[-1]
djx <- as.numeric(dj_x)

tfit <- fit.st(djx)
tpars <- tfit$par.ests
print(tpars)
nu<- tpars[1]
mu<- tpars[2]
sigma <- tpars[3]

hist(djx, nclass=40, probability = TRUE )
yvals <-  dt( (djx - mu)/sigma, df =nu )/sigma
points( djx, yvals, col="blue", cex=0.2)
points(djx, dnorm(djx,mean=mean(djx),sd=sd(djx)), col = "red", cex=0.2)


## test interest rates for normality
#prepare data
zcb <- ZCB_CA["2006::",]

yield_cols <- c("1.00y", "5.00y", "10.00y")
                
 #daily log returns 1,5,10 y
zcb_x <- diff(log(ZCB_CA["2006::",yield_cols]))[-1]
zcbx_m <- apply.monthly(zcb_x, colSums)
                
#daily simple returns 1,5,10
zcb_x2 <- diff(ZCB_CA["2006::",yield_cols])[-1]
zcbx2_m <- apply.monthly(zcb_x2, colSums)

plot.zoo(zcbx_m, type="h")
plot.zoo(zcbx2_m, type="h")


par(mfrow=c(1,2))
qqnorm(zcbx_m[,3], main="QQ 10y log rets")
qqnorm(zcbx2_m[,3],  main="QQ  10y simple rets")
par(mfrow=c(1,1))

# Compute the kurtosis of each series in zcbx_m and zcbx2_m
apply(zcbx_m, MARGIN=2, FUN=kurtosis)
apply(zcbx2_m, MARGIN=2, FUN=kurtosis)

# Conduct the Jarque-Bera test on each series in zcbx_m and zcbx2_m
apply(zcbx_m, MARGIN=2, FUN=jarque.test)
apply(zcbx2_m, MARGIN=2, FUN=jarque.test)


##### test gold for nomaility fit t-student
#prepare data
goldx<-diff(log(GOLD["1990::",]))[-1]
goldx_q <- apply.quarterly(goldx,sum)
goldxq <- as.numeric(goldx_q)
jarque.test(goldxq)

tfit <- fit.st(goldxq)
tpars <- tfit$par.ests
print(tpars) #estima
nu<- tpars[1]
mu<- tpars[2]
sigma <- tpars[3]
hist(goldxq, nclass=20, probability = TRUE, main="gold quarterly log returns" )
yvals <-  dt( (goldxq - mu)/sigma, df =nu )/sigma
points( goldxq, yvals, col="blue", cex=0.2) # t-student is blue


################################
### chapter 3 real world returns are volatile and correlated
################################

##how to generate a normal and t-distributed sample with specified means
# data is dowjones  log returns dj_x
n <- length(dj_x)
#for normal
npars <- list(mu=mean(dj_x), sigma=sd(dj_x))
#for t-student
tfit <- fit.st(dj_x)
tfpars <- tfit$par.ests
tpars <- list(nu=tfpars[1],mu=tfpars[2], sigma=tfpars[3])

#  Generate a normal sample of size n with parameters given by npars
ndata <- rnorm(n)*npars$sigma + npars$mu 
# Generate a t-distributed sample of size n with paramaters given by tpars
tdata <- rt(n, df = tpars$nu)*tpars$sigma + tpars$mu

ndatax <- xts(ndata, time(dj_x))
tdatax <- xts(tdata, time(dj_x))
alldata <- merge(dj_x, ndatax,tdatax)
plot.zoo(alldata, type="h", ylim = range(alldata))

### Apply Ljung-box to djx
# Apply the Ljung-Box test to djx
Box.test(dj_x, lag = 10, type="Ljung")
Box.test(abs(dj_x), lag = 10, type="Ljung")  # on dj_x abs values

print('on monthly data')
djx_m <- apply.monthly(dj_x, sum)  
Box.test(djx_m, lag = 10, type="Ljung")
Box.test(abs(djx_m), lag = 10, type="Ljung")  # on dj_x abs values

### Apply Ljung-box to longer periods
##prepare data
tickers29 <- c("AAPL","AXP","BA","CAT","CSCO","CVX","DD","DIS","GE","GS","HD","IBM","INTC","JNJ","JPM","KO","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","VZ","WMT","XOM")
djcx_d <-  diff(log(DJ_const["1999-12-31::",tickers29]))[-1] # get daiy log returns
djx_m <- apply.monthly(djcx_d, colSums) 


apply(djx_m, MARGIN=2,FUN=Box.test, lag = 10, type="Ljung")


### Extreme values in  log-return series 
ftse <- diff(log(FTSE))[-1]  # all available data daily
ftse_losses <- -ftse 
ftse_extremes <- ftse[ftse < -0.025]
#plot(ftse_extremes, type="h", auto.grid=FALSE)
plot.zoo(ftse_extremes, type = "h")                       
                       

### cross correlation of indexes
data("SMI")
dj_x <- diff(log(DJ["2005/2015",]))[-1]
ftse_x <- diff(log(FTSE["2005/2015",]))[-1]
smi_x <- diff(log(SMI["2005/2015",]))[-1]
indexes<-cbind(dj_x, ftse_x, smi_x)
colnames(indexes)<-c("dj", "ftse", "smi")
#cleaning   off NA values
indexes = na.omit(indexes)
##correlation plotting
pairs(as.zoo(indexes), cex=0.1, col=2)
## correlation matrix
cor(indexes)

acf(abs(indexes))
################################
### chapter 4 estimate portfolio value at risk
################################
##computing VAR and ES for a normal distribution

mu <- 0.000444099
sigma <- 0.02001809
#going from -4*sigma to 4*sigma
xvals <- seq(from = -4*sigma, to = 4*sigma, length.out = 100)

# Compute the density of a N(mu, sigma^2) distribution at xvals
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot ndens against xvals
plot(xvals,ndens,type="l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(0.99, mean=mu, sd=sigma)
ES99  <- ESnorm(0.99, mu=mu, sd=sigma)

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")
abline(v = ES99, col = "green")

##### UK portfolio  30% ftse, 40% S&P 30% SMI
data("USD_GBP")
data("CHF_GBP")
data("FTSE")
data("SMI")
riskfactors<-merge(FTSE,SP500,SMI,USD_GBP,CHF_GBP, all=FALSE) # rmoves NAs
riskfactors <- riskfactors["2000::2012",]
plot.zoo(riskfactors)
#empirical estimates of var and ES of normal distribution
losses <- rnorm(100)
losses_o <- sort(losses, decreasing = TRUE)
quantile(losses,0.95)
qnorm(0.95)

mean(losses[losses > quantile(losses,0.95)])
ESnorm(0.95) #we can see the expected shortfall is close to theoretical one

##### examining risk factors for international equity portfolio
returns <- diff(log(riskfactors))[-1,]
plot.zoo(returns)

## jarq.test
apply(returns, FUN=jarque.test, MARGIN=2)

##check the normality of returns
qqnorm(returns[,5])
qqline(returns[,5],col=2)

## picture of hte acfs of returns and abs returns
acf(returns)
acf(abs(returns))


###historical simulation

## loss operator for a portfolio of weights 0.3, 0.4 0.3
### normally custom written for any specific portfolio

lossop<-function(xseries,wts=c(0.3,0.4,0.3)){
  if (is.xts(xseries))
    x <- coredata(xseries)
  else if (is.matrix(xseries))
    x <- xseries
  else
    x <- matrix(xseries,nrow=1)
  ## apply a function on matrix rows
  ll <- apply(x,1,
              function(x,wts){
                1-(wts[1]*exp(x[1]) + wts[2]*exp(x[2]+x[4]) + wts[3]*exp(x[3]+x[5]))
              },wts=wts)
  
  if (is.xts(xseries))
    ll <- xts(ll,time(xseries))
  return(ll)
}

lossop(rep(-0.1,5))

##### estimating VaR and ES 

## non parametric estimations of quantiles from the data for 1% and 99% quantiles
quantile(hslosses, c(0.01,0.99))

# Estimate the 99% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.99)])

##compute means from the data
mu<-mean(hslosses)
sigma <- sd(hslosses)

#compute 0.99 var assuming  hslosses is normally distributed
qnorm(0.99, mean=mu, sd=sigma)
ESnorm(0.99,mu=mu, sd=sigma)

############################
### portfolio with options
#### Compute Black-Scholes price of an option
# r prog switch  construction
tf <- function(type=c("call", "put")){
  res <- "nan"
  switch(type, 
        call = { res <- "c"},
        put =  {res <-"p"},
        stop("wrong type"))  # or res <- "nan")  # default value
  return(res)
}

require(qrmtools)
K<-100 # strike
r<-0.01 #annualized free-risk interest rate
sigma<-0.20 #annualized vola

paste("Price a European call option that matures in one year if the current stock price is 80,
      T=1 i.e. option expiry is in 1 year")
Black_Scholes(t=0, S=80, r=r, sigma=sigma, K=K, T=1, type="call")
# Price a European call option that matures in one year if the current stock price is 120
Black_Scholes(0, 120, r, sigma, K, 1, "call")

paste("pricing puts")
# Price a European put option that matures in one year if the current stock price is 80
Black_Scholes(0, 80, r, sigma, K, 1, "put")

# Price a European put option that matures in one year if the current stock price is 120
Black_Scholes(0, 120, r, sigma, K, 1, "put")
         
####### equity and implied volatility risk factors  
data("SP500")
data("VIX")

riskfactors <- merge(SP500,VIX, all=FALSE)["1990::2010"]
returns<-diff(log(riskfactors))[-1,]  
plot.zoo(riskfactors)

## basic form of pairs plot
plot(as.matrix(returns), cex=0.1)

## jarq.test
apply(returns, FUN=jarque.test, MARGIN=2)
qqnorm(returns[,2], main="QQ-plot for VIX")
qqline(returns[,2],col=2)


acf(returns)
acf(abs(returns))

cor(returns)
         
####### Historical simulation of a single option portfolio ####

##lossop op for option portfolio
lossop <- function(xseries,r=0.01, K=100, T=1, sigma=0.2,S=100){
  if (is.xts(xseries))
    x <- coredata(xseries)
  else if (is.matrix(xseries))
    x <- xseries
  else
    x <- matrix(xseries,nrow=1)
  ll <- apply(x,MARGIN=1,
            function(x,r,K,T,sigma,S){
              deltat <- 1/250
              V_t0 <- Black_Scholes(t=0, S=S, r=r, sigma=sigma, K=K, T=T, type="call")
              V_t1 = Black_Scholes(t=deltat, S=exp(log(S)+x[1]), r=r, sigma=exp(log(sigma)+x[2]), K=K, T=T, type="call")
              return(- (V_t1 - V_t0)/V_t0)
            },
            r=r,K=K,T=T,sigma=sigma,S=S)
  if (is.xts(xseries))
    ll <- xts(ll,time(xseries))
  return(ll)
}

# Calculate the first loss
lossop(c(-0.1,-0.1),S=80, sigma=0.2)

# Calculate the second loss
lossop(c(-0.1, 0.1), S=100, sigma=0.2)

# Create and plot hslosses
hslosses <- lossop(returns,sigma=0.2,S=100)

plot(hslosses)


# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)
qqline(hslosses)
# Plot the sample acf of raw data and absolute values in hslosses
acf(hslosses)
acf(abs(hslosses))

### Estimating VaR and ES for option portfolio
paste("non-parametric calculations from loss simulated data")
# Estimate the 99.5% percentile of the distribution
paste("99.5% VAR",quantile(hslosses, 0.995))

# Estimate the 99.5% ES
paste("99.5% ES from data",mean(hslosses[hslosses >= quantile(hslosses, 0.995)]))

paste("var and ES if losses are thought to be normally distributed")
# Estimate the mean and standard deviation of hslosses
mu<-mean(hslosses)
sigma<-sd(hslosses)

# Compute the 99.5% quantile of a normal distribution
paste("99.5% VAR from normal distribution",qnorm(0.995, mean=mu, sd=sigma))

# Compute the 99.5% ES of a normal distribution
paste("99.5 % ES from normal distribution",ESnorm(0.995, mu=mu, sd=sigma))


#### computing weekly option portflio var 
## convert daily returns to weekly
returns_w <- apply.weekly(returns, colSums) 
hslosses_w <- lossop(returns_w, sigma=0.25, S=120)
paste("99 % var for weekly portfolio returns for S=120 and sigma=0.25")
quantile(hslosses_w, 0.99)

