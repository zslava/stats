##ch10 Applied


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch10-Unsupervised/");source("ch10_11.R")


### a
x <- read.csv(file="Ch10Ex11.csv", header=F)


### b
distcor <- as.dist(1-cor(x))
hc.comp <- hclust(dcor, method="complete")
plot(hc.comp)

## we do not obtain 2 groups 

### c
pr.out = prcomp(t(x))
summary(pr.out)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]