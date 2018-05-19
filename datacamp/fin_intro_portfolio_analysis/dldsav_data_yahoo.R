#rm(list=ls()); setwd("~/Dropbox/cs/bigdata/datacamp/fin_intro_portfolio_analysis");source("dldsav_data_yahoo.R")
#setwd("~/Dropbox/cs/bigdata/datacamp/fin_intro_portfolio_analysis");source("dldsav_data_yahoo.R")

require(xts)
require(quantmod)


dt_start<-"1990-12-01"
dt_end <-"2016-12-31"

## current contents of dow jones index source http://money.cnn.com/data/dow30/
djii<-c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DD", "XOM", "GE", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT")


is_load<-TRUE
is_load<-FALSE

if (is_load){
    tkr<-new.env()
    for (i in 1:length(djii)){
    print(paste("loading", djii[i]))
    getSymbols(djii[i], env=tkr, src="yahoo", from=as.Date(dt_start), to=as.Date(dt_end)  )
  }
}
#create an xts of monthly ajdusted closes 

prmo<-c()
for (i in 1:length(djii)){
  cdly<-get(djii[i], env=tkr)
  cmly<-to.monthly(cdly, indexAt="endof")
  prmo<-cbind(prmo, Ad(cmly) ) 
}
colnames(prmo)<-djii

## we have two tickers which had NA values at the first row, lets remove it
na_idx<-which(is.na(prmo[1,])) #prmo[1,] first row

prmo <- prmo[, setdiff(colnames(prmo), colnames(prmo)[na_idx] ) ]  #this removes xts columns colnames(prmo)[na_idx]

write.zoo(prmo, "djii_prices.csv" )

#reading from file is 
#prices <- read.zoo("djii_prices.csv", format= "%Y-%m-%d", header=TRUE)
