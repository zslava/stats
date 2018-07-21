##ch10 Applied


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch10-Unsupervised/");source("ch10_10.R")


#a

set.seed(2)

x <- matrix(rnorm(20*3*50), ncol=50)


x[1:20,] <- x[1:20,] + 3 
x[41:60,] <- x[41:60,] - 3

y <- c(rep(2,20), rep(3,20), rep(4,20))
### b

pca.out <- prcomp(x)

print(summary(pca.out))


plot(pca.out$x[,1:2], col=y, cex=0.3, xlab="Z1", ylab="Z2")


###c
km.out <- kmeans(x, 3, nstart=20)

print(km.out$cluster)
print( table(km.out$cluster, c(rep(3,20), rep(1,20), rep(2,20) )) )

##perfect match

#d)
km2 <- kmeans(x, 2, nstart=20)
print(km2$cluster)
#1 new class is composed of two other classes, and the 2nc class corresponds to a previous class 

#e)
km4 <- kmeans(x, 4, nstart=20)
print(km4$cluster)

#one class is split in 2 new classes, the other two classes stay the same

#f)
px <- pca.out$x[,1:2]
km_pc <- kmeans(px, 3,nstart=20)
print(km_pc$cluster)
# good separation in 3 clusters 
print( table(km_pc$cluster, c( rep(3,20), rep(2,20), rep(1,20)  )) )
#perfect match 


##g)
km.s <- kmeans(scale(x), 3, nstart=20)
print( table(km.s$cluster, c( rep(1,20), rep(3,20), rep(2,20)  )) )

##still a perfect match
