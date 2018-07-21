##ch10 Lab


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch10-Unsupervised/");source("ch10_lab.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}


pprint("lab 1 PCA")

data(USArrests)
states <- row.names(USArrests)
print(states)

## summary stats with apply on columns
print ( apply(USArrests, MARGIN=2,mean) )
print ( apply(USArrests, MARGIN=2,sd) )


#do pca 
pr.out <- prcomp(USArrests, scale=TRUE) #scaling normalize variables. 

print( names(pr.out) )
print ( pr.out$center)
print ( pr.out$scale)
print ( pr.out ) # PCA details  Rotains shows the loadings of feature to components


biplot(pr.out,scale=0, cex=0.6) ## scale=0 arrows are scaled to represent loadings


#to make a 180 flip of the plot 
pr.out$rotation <- -pr.out$rotation 
pr.out$x <- - pr.out$x 
biplot(pr.out,scale=0, cex=0.6)

print(pr.out$sdev )  #SDevs
pr.var <- pr.out$sdev^2

#compute a proportion of variance explained
pve <- pr.var / sum(pr.var)
print(paste("proportion of variance explained:",paste(pve,collapse=" ") ))


par(mfrow=c(1,2))
plot(pve, xlab="PC", ylab="Var Explained", ylim=c(0,1), type="b" )
plot(cumsum(pve), xlab="PC", ylab="Cumul Var Explained", col="blue", ylim=c(0,1), type="b" )
par(mfrow=c(1,1))


pprint("lab 2 Clustering")

##generate data
set.seed(2)
n <- 50
nh <- n/2
x <- matrix(rnorm(n*2),ncol=2)
x[1:nh,1] =x[1:nh,1] + 3
x[1:nh,2] =x[1:nh,2] - 4

y <- c( rep(3,nh), rep(2,nh))
plot(x,col=y, pch=19)

### perform k-means clust with K = 2
km.out <- kmeans(x, centers=2,nstart=20)

print( km.out)

plot(x,col=(km.out$cluster+1) , main="k-means with k=2", pch=1, cex=2, lwd=2 )
#check the clastered classification with true label
points(x,col=y, pch=19)

#in real world K is unknown   lets try kmeans with k=3
set.seed(4)
km.out <- kmeans(x,centers=3,nstart=20) #recommended value for nstart 20 or 50
print(km.out)

plot(x,col=(km.out$cluster+1) )




pprint("Hierarchical clustering")

hc.complete <- hclust(dist(x), method="complete")
hc.single <- hclust(dist(x), method="single")
hc.avg <- hclust(dist(x), method="average")
par(mfrow=c(1,3))
plot(hc.complete, main="complete linkage", cex=.9)
plot(hc.single, main="single linkage", cex=.9)
plot(hc.avg, main="average linkage", cex=.9)
par(mfrow=c(1,1))

#To determine the cluster labels for each observation associated 
#with a given cut of the dendrogram

print( cutree(hc.complete, 2))
print( cutree(hc.single, 2))
print( cutree(hc.avg, 2))


#lets check the hi-clustering with variables scaled before clustering
xsc <- scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical clustering with scaled features" )


##clustering of correlation based distance. Need to have at least 3 features
x<-matrix(rnorm(30*3),ncol=3)
dd<-as.dist(1-cor(t(x)))  # as.dist
plot(hclust(dd, method="complete"), main="complete linkage with correlation-based distance", xlab="",sub="")


pprint("Lab 3 NC160 Data example")
require(ISLR)
data(NCI60) #genomic data
nci.labs <- NCI60$labs
nci.data <- NCI60$data 
print( dim(nci.data))

print( table(nci.labs) )

#perform pca
pr.out <- prcomp(nci.data, scale=TRUE)

#func to assing colors to each of 64 lines
Cols <-function(vec){
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}


par(mfrow=c(1,2))
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch=19, xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)],col=Cols(nci.labs),pch=19, xlab="Z1", ylab="Z3")
par(mfrow=c(1,1))

print( summary(pr.out) )

plot(pr.out)


pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="PC", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulatve PVE", xlab="PC", col="brown3")
par(mfrow=c(1,1))


pprint("Clustering the observations of NCI60 Data")

sd.data <- scale(nci.data)
data.dist <- dist(sd.data)
par(mfrow=c(3,1))
plot(hclust(data.dist), labels=nci.labs, main="complete linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="average linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="single linkage", xlab="", sub="",ylab="")

par(mfrow=c(1,1))


#We will use complete linkage hierarchical clustering for the further analysis

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out,4)
cm <- table(hc.clusters, nci.labs)
print(cm)


plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

print(hc.out)

#lets compare it with k-means clustering with 4 clusters
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
cm <- table(kmeans=km.clusters, hierarch=hc.clusters)
print(cm)


#lets perform hierarchical clustring on several principal components
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier Clust on First Fir score vectors" )
cm <- table(hcpc=cutree(hc.out,4),truth=nci.labs)
print(cm)

