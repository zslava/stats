#rm(list=ls()); setwd("~/Dropbox/cs/bigdata/datacamp/fin_intro_portfolio_analysis");source("min_variance_pract.R")

require(xts)
require(PerformanceAnalytics)
require(tseries)
require(ggplot2)
require(dygraphs)

#reading monthly prices of djii  from 1990-12  to 2016-12
# two tickers GS and V  did not have a value at 1990-12 so they are purged  so

djii<-c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DD", "XOM", "GE"
        ,"HD", "IBM", "INTC","JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG"
        ,"TRV", "UTX", "UNH", "VZ", "WMT")


# check R script dldsav_data_yahoo.R  which downloads data from yahoo converts it to monthly 
#and writes csv file djii_prices.csv
dj_prices <- read.zoo("djii_prices.csv", format= "%Y-%m-%d", header=TRUE)

#returns from prices
dj_returns<-Return.calculate(dj_prices) #performanceAnalytics
dj_returns<-dj_returns[-1,] #remove first NA line

#Create a vector of mean returns for all tickers
dj_means <- apply(dj_returns, MARGIN=2, FUN="mean")

# Create a vector of mean standard deviation
dj_sds <- apply(dj_returns, MARGIN=2, FUN="sd")

df<-data.frame(dj_sds, dj_means)

print(
ggplot(df, aes(x=dj_sds, y=dj_means)) + 
  geom_text(aes(label=djii),size=3) +
 ggtitle("DJII stocks mean returns between 1991 and 20016") +
 labs(y="mean return", x="mean standard deviation")
)

###
dim(dj_returns)
ew_preturns<-rowMeans(dj_returns,1) #same as apply(returns,1 "mean")
ew_preturns <- xts(ew_preturns, order.by = time(dj_returns))


### loopig to find an efficint frontier
stockmu<-dj_means
grid_size<-50
prtf_size<-length(dj_returns[1,]) #28
grid <- seq(from = 0.01, to = max(stockmu)-0.0001, length.out = grid_size)

vpm <- vpsd <- rep(NA, length=grid_size )
mweights <- matrix(NA, grid_size, prtf_size)

for(i in 1:length(grid)) {
  print(paste("iterating  ith step:", i))
  opt <- portfolio.optim(x = dj_returns, pm = grid[i]) # return target grows monotonically
  vpm[i] <- opt$pm
  vpsd[i] <- opt$ps
  mweights[i, ] <- opt$pw
}

#plot(vpsd, vpm)
df<-data.frame(vpsd, vpm)

print(
  ggplot(df, aes(x=vpsd, y=vpm)) + 
    geom_point() +
    ggtitle("Efficient frontier") +
    labs(y="return", x="vola")
)

##The minimum variance and maximum Sharpe ratio portfolio
weights_minvar <- mweights[vpsd == min(vpsd), ]

# Calculate the Sharpe ratio
riskfree_rt<-0.0075
vsr <- (vpm - riskfree_rt) / vpsd

# Create weights_max_sr as the portfolio with the maximum Sharpe ratio
weights_max_sr <- mweights[vsr == max(vsr),]

# Create barplot of weights_minvar and weights_max_sr
par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
barplot(weights_minvar[weights_minvar > 0.01])
barplot(weights_max_sr[weights_max_sr > 0.01])

