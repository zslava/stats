##ch8-lecture


# rm(list=ls()); setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch8-treeBasedMethods/");source("ch8-lecture.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}



pprint("decision trees")


require(ISLR)
require(tree)
attach(Carseats) #in ISLR
hist(Sales)
High<- ifelse(Sales<=8, "No", "Yes")
Carseats <- data.frame(Carseats,High)

tree.carseats <- tree(High~.-Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)


set.seed(1011)
nspl <- nrow(Carseats)
train <- sample(1:nspl, 250  )
test <- (1:nspl)[-train]
tree.carseats <- tree(High~.-Sales, data=Carseats, subset=train)
tree.pred <- predict( tree.carseats, Carseats[test,], type="class") #classification
ctbl <- table(tree.pred, Carseats[test,]$High  )
print(ctbl)

accuracy <- sum(diag(ctbl)) / sum(ctbl)
print(accuracy)

## lets prune this tree 
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
print(cv.carseats)
plot(cv.carseats)

#we prune the tree by taking a size at a minimum of cv curve ie. picking 13
prune.carseats <- prune.misclass(tree.carseats, best=13)
plot(prune.carseats)
text(prune.carseats,pretty=0)

#no lets evaluate the pruned tree on a test dataset
tree.pred <- predict( prune.carseats, Carseats[test,], type="class") #classification
ctbl <- table(tree.pred, Carseats[test,]$High  )
print(ctbl)

accuracy <- sum(diag(ctbl)) / sum(ctbl)
print(accuracy)

#we observe that accuracy of the pruned tree is the same,
# but pruned tree is easier to interpret

pprint("Random Forests and boosting")

#This moethods use trees as building blocks to build more complex models. Here we will use the Boston housing data to explore random forests  and boosting from the MASS package

#Random forests build lots of bushy trees, and then average them to reduce the variance

require(randomForest)
require(MASS)
set.seed(101)
print(dim(Boston))
nsamb <- nrow(Boston)
tsize <- 300
train <- sample(1:nsamb, tsize)
test <- (1:nsamb)[-train]

#fitting a ranom forest. WE will use the response `medv` (median house value)

rf.boston <- randomForest(medv~.,data=Boston, subset=train)
print(rf.boston)

# The MSR (mean of squared residuals) and variance explained are based on OOB (out of bag) estimates, a clever trick to get honets error estimates

#the mtry reported param has a value 4 . As we have 13 predictor we can vary thiss param between 1 and 13 to check the test MSE

nfeats <- dim(Boston)[2] - 1
oob.err <- rep(NA,nfeats)
test.err <- rep(NA,nfeats)
for( m in 1:nfeats){
    fit <- randomForest(medv~.,data=Boston, subset=train, mtry=m, tree=400)
    oob.err[m] <-fit$mse[400]
    pred <- predict(fit, Boston[test,])
    test.err[m] <- mean( (pred - Boston[test,]$medv)^2  )
    cat(m, " ")
}
#now plotting MSE
data_mat <- cbind(test.err, oob.err) 
matplot(1:m, data_mat, pch=19, col=c("red", "blue"), type="b", ylab="test MSE", xlab="mtry" )
legend("topright", legend=c("OOB", "Test"), pch=19, col=c("red","blue"))


#not too difficulat.  `mtry=13` correspoind to bagging
pprint("Boosting")

#Boosting builds lots of smaller trees. Unlike random forests, each new tee in boosting tries to patch up the deficiencies of the current ensemble.

require(gbm)
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=1000, shrinkage=0.01, interaction.depth=4) #interaction.depth = # of splits
print( summary(boost.boston) )1 #this produces the plot of relative importance of features to reduce variance
par(mfrow=c(1,2))
plot(boost.boston, i="lstat")
plot(boost.boston, i="rm")
par(mfrow=c(1,1))
# lets make a prediction of the test set. with boosting, the number of trees is a tuning parameter. we need to use validaton to select the number of trees. 

n.trees <- seq(from=100, to=10000, by=100)
predmat <- predict(boost.boston, newdata=Boston[test,], n.trees=n.trees)
print(dim(predmat))
berr <- apply( (predmat - Boston[test,]$medv)^2, MARGIN=2, FUN=mean )
plot(n.trees, berr, pch=19, cex=0.5, ylab="MSE", xlab="# trees", main="Boosting Test Error")

## boosting if tweeked often outpeforms of random forests. 

# for boosting you have genuine tuning params: the number of trees, the shrinkage param, and the depth
# both are very powerful methods  
