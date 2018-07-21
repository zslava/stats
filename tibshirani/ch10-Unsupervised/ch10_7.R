##ch10 Applied


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch10-Unsupervised/");source("ch10_7.R")

require(ISLR)
data(USArrests)

dsc = scale(USArrests)

a = dist(dsc)^2

b = as.dist(1 - cor(t(dsc)))

hc_a <- hclust(a) #method="complete"
hc_b <- hclust(b)

par(mfrow=c(1,2))
plot(hc_a, main="euclidian")
plot(hc_b, main="correlation distance")

par(mfrow=c(1,1))