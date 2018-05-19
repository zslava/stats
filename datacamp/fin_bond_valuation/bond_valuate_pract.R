#rm(list=ls()); setwd('C:/Users/Zimine/Dropbox/cs/bigdata/datacamp/fin_bond_valuation'); source("bond_valuate_pract.R")
#rm(list=ls()); setwd('~/Dropbox/cs/bigdata/datacamp/fin_bond_valuation'); source("bond_valuate_pract.R")

require(Quandl)


#define pronce pricing func
bondprc <- function(p, r, ttm, y) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  return(sum(cf$pv))
}

## quandl my api 
##my quandl api key
Quandl.api_key("dCbRAxPkcnxxxzTtDFW5")
baa<-Quandl("MOODY/DBAAYLD")

# Identify 9/30/16 yield
baa_yield <- subset(baa, baa$DATE == "2016-09-30")

# Convert yield to decimals and view
baa_yield <- baa_yield$VALUE * 0.01
print(baa_yield)
print(bondprc(p = 100, r = 0.05, ttm = 5, y = 0.0429))


##treasury data
library(quantmod)

t10yr <- getSymbols("DGS10", src="FRED", auto.assign = FALSE)
#head(t10yr)S

####use uniroot func to find ytm
bprice<-95.79
coupon<-5
par<-100
ttm<-5
cf<-c(-1*bprice, rep(coupon,ttm-1), coupon+par)

bval <- function(x, cf, t=seq(along = cf)){ 
  sum(cf / (1 + x)^t)
}

# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

ytm(cf)

##approximate duration

apx_duration <-function(par,r,ttm,y){
  dy<-0.01 # 100 bp
  duration <- (bondprc(par,r,ttm, y-dy) -bondprc(par,r,ttm, y+dy) ) / (2 * bondprc(par,r,ttm, y) * dy)
  return(duration)
}


par<-100
r<-0.1
y<-0.1
ttm<-20

apx_duration(par,r,ttm,y)



apx_convexity<-function(par,r,ttm,y){
  dy<-0.01 #100 bp
  conv<- ( bondprc(par,r,ttm, y-dy) + bondprc(par,r,ttm, y+dy) - 2 * bondprc(par,r,ttm, y) ) /
         (bondprc(par,r,ttm, y) * dy^2)
  return(conv)
}


apx_convexity(par,r,ttm,y)

## compute dollar change

apx_price <- function(par,r,ttm,y, dy){
  
  bpx<-bondprc(par,r,ttm, y) # price of bond
  dur_dollar_delta<- -apx_duration(par,r,ttm,y) * dy * bpx
  conv_dollar_delta<- 0.5 * apx_convexity(par,r,ttm,y) * dy^2 * bpx
  
  return(bpx + dur_dollar_delta + conv_dollar_delta)
}

apx_price(par,r,ttm,y, 0)
dy<-0.01
apx_price(par,r,ttm,y, dy)
apx_price(par,r,ttm,y, -dy)

### chap 4 comprehensive example
aaa <- Quandl("MOODY/DBAAYLD")
aaa_yield <- subset(aaa, aaa$DATE == as.Date("2016-09-30"))
aaa_yield <- 0.01 * as.numeric(aaa_yield$VALUE)                    
                    
cttm<-8
par<-100
cr<-0.03
#cash flows
cf <- c( rep(par*cr,cttm-1), par*(1+cr)  )
cf <- data.frame(cf)
cf$t <- seq(1,nrow(cf),1)
cf$pv_factor <- 1 / (1+aaa_yield)^cf$t
cf$pv <- cf$cf * cf$pv_factor

