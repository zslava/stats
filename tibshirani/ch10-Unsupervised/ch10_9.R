##ch10 Applied


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch10-Unsupervised/");source("ch10_9.R")

### ex 9
data(USArrests)
x<- USArrests
states <- row.names(USArrests)


###a)

hc_c <- hclust(dist(x), method="complete")

plot(hc_c)


###b )
ct <- cutree(hc_c, 3)

print( states[which(ct==1)] )
print( states[which(ct==2)] )
print( states[which(ct==3)] )

###c)
xs <- scale(x)
hc_sc <- hclust(dist(xs), method="complete")

##d
plot(hc_sc)
## we have now 4 clusters 
## variables should be scaled before 
## justifications ? 
