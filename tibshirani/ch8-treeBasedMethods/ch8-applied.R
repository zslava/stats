##ch8-lecture


# rm(list=ls()); setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch8-treeBasedMethods/");source("ch8-applied.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}


ex7<-function(){
pprint("EX 7")
require(ISLR)
require(randomForest)

nfeats <- dim(Boston)[2] -1
nsamp <-  dim(Boston)[1]

mtrys <- c(nfeats, nfeats/2, sqrt(nfeats))
ntrees <- seq(from=1, to=500)


set.seed(1101)
#split into train and test
train <- sample( 1:nsamp, floor(4/5 * nsamp) )
#train <- sample( 1:nsamp, nsamp/2 )
test <- (1:nsamp)[-train]

X.train <- Boston[train, -14] # all features minus response
X.test <-  Boston[test, -14]

Y.train <- Boston[train, 14] #response vector
Y.test  <- Boston[test, 14]

p <- nfeats
p.2 <- p / 2
p.sq <- sqrt(p)

#fit models for each  p (rf.boston.p$mse will contain values for each tree between 1 and 500)
rf.boston.p <- randomForest(X.train, Y.train, xtest=X.test, ytest=Y.test, mtry=p, ntree=500) 
rf.boston.p2 <- randomForest(X.train, Y.train, xtest=X.test, ytest=Y.test, mtry=p.2, ntree=500) 
rf.boston.psq <- randomForest(X.train, Y.train, xtest=X.test, ytest=Y.test, mtry=p.sq, ntree=500) 

# find ylims
y_min <- 0.9* min(c(rf.boston.p$test$mse, rf.boston.p2$test$mse, rf.boston.psq$test$mse))
y_max <- 1.1* max(c(rf.boston.p$test$mse, rf.boston.p2$test$mse, rf.boston.psq$test$mse))

plot(1:500, rf.boston.p$test$mse, col="green", type="l", xlab="# trees", ylab="test mse", ylim=c(y_min, 20))
lines(1:500, rf.boston.p2$test$mse, col="red", type="l")
lines(1:500, rf.boston.psq$test$mse, col="blue", type="l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col=c("green", "red", "blue"), cex=1, lty=1)
}

pprint("ex 8")

require(ISLR)
require(tree)
data(Carseats)

#a)
set.seed(1011)
nfeats <- dim(Carseats)[2] -1
nsamp <-  dim(Carseats)[1]
train <- sample( 1:nsamp, size=250) #250 / 150 split
test <- (1:nsamp)[-train]

#b)
tree.carseats <- tree(Sales~., data=Carseats[train,])
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.pred <- predict(tree.carseats, Carseats[test,])
test.mse <- mean( (Carseats[test,]$Sales - tree.pred)^2 )
print(paste("full tree test mse", test.mse))



#c)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.tree)
plot(cv.carseats$size, cv.carseats$dev, type="b")
prune.carseats <- prune.tree(tree.carseats, best=16)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred <- predict(prune.carseats, Carseats[test,])
test.mse <- mean( (Carseats[test,]$Sales - tree.pred)^2 )
print(paste("pruned tree test mse", test.mse))

#prunning did a tiny improvement of the test mse


#d)

require(randomForest)
bag.carseats <- randomForest(Sales~.,data=Carseats[train,], mtry=nfeats, tree=500, importance=T) #m=p is a bagging case of RandomForest
bag.pred <- predict(bag.carseats, Carseats[test,])
test.mse <- mean( (Carseats[test,]$Sales - bag.pred)^2 )
print(paste("bagging test mse", test.mse))

print( importance(bag.carseats))
#bagging reduces significantly the test mse

#e)
rf.carseats = randomForest(Sales~., data=Carseats[train,], mtry=5, ntree=500, importance=T)
rf.pred = predict(rf.carseats, Carseats[test,])
test.mse  = mean((Carseats[test,]$Sales - rf.pred)^2)
print( importance(rf.carseats) )
print(paste("random forest test mse", test.mse))

#in this data set RF slightly increased the test mse




#### ex11
pprint("EX 11")
require(ISLR)
require(class)
data(Caravan)

##a)
set.seed(342)
nfeats <- dim(Caravan)[2] -1 
nsam  <- dim(Caravan)[1]
train <- 1:1000
test <- (1:nsam)[-train]
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
X.train <- Caravan[train,-86]
X.test <- Caravan[test,-86]
Y.train <- Caravan[train,86]
Y.test <- Caravan[test,86]

