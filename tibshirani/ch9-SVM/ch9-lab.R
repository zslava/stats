##ch9-lab


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch9-SVM/");source("ch9-lab.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}

pprint("support vector classifier")

#cost param is fixed
#construct a 2 dimensional example 
set.seed(1)
x<-matrix(rnorm(20*2),ncol=2) # 20x2 matrix of N(0,1) points
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1  # move mean to  1 to 2nd half of lines in the matrix
plot(x, col=(3-y), pch=19) #first half of matrix lines is blue, 2nd half is red

#prepare a dataset data frame
dat <- data.frame(x=x, y=as.factor(y))
require(e1071)
cst <- 10
svmfit <- svm(y~., data=dat, kernel="linear", cost=cst, scale=FALSE) #x1,x2 are on the same scale
plot(svmfit,dat)
print(summary(svmfit))

print("support vectors")
print( svmfit$index)

# for larger bands
cst <- 0.1
svmfit <- svm(y~., data=dat, kernel="linear", cost=cst, scale=FALSE) #x1,x2 are on the same scale
plot(svmfit,dat)
print( svmfit$index)

##

#for cross-validation
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1.5, 10,100)))
print(summary(tune.out))

#we see that cost=0.1 has the lowest error rate

bestmod = tune.out$best.model
print( summary(bestmod) )

#for prediction
xtest <- matrix(rnorm(20*2),ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest,y=as.factor(ytest))
ypred <- predict(bestmod,testdat)
cm <- table(predict=ypred, truth=testdat$y)
print(cm)
print(paste("bestmod svm accuracy:", sum(diag(cm))/sum(cm) ))
# only 1 variable is misclassified

#lets run predictions with cost=0.01
cst <- 0.01
svmfit <- svm(y~., data=dat, kernel="linear", cost=cst, scale=FALSE) #x1,x2 are on the same scale
ypred <- predict(svmfit,testdat)
cm <- table(predict=ypred, truth=testdat$y)
print(cm)
print(paste("cost 0.01 svm accuracy:", sum(diag(cm))/sum(cm) ))

# now consider the case where two classes are linearly separable
# separate two classes further 

x[y==1,] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19) #first half of matrix lines is blue, 2nd half is red
dat <- data.frame(x=x, y=as.factor(y))

cst <- 1e5 #high cost small margins
svmfit <- svm(y~., data=dat, kernel="linear", cost=cst, scale=FALSE) #x1,x2 are on the same scale
plot(svmfit,dat)
print(summary(svmfit))

#lets check the accuracy of this model on test data also more separated
#prepare test data
xtest[ytest==1,] <- xtest[ytest==1,] + 0.5
plot(xtest,col=(ytest+5)/2,pch=19)
testdat <- data.frame(x=xtest,y=as.factor(ytest))

ypred <- predict(svmfit, testdat)
cm <- table(predict=ypred, truth=testdat$y)
print(cm)
print(paste("cost 1e5 svm accuracy on separable classes :", sum(diag(cm))/sum(cm) ))

#lets widen the margins and check accuracy
cst <- 1 #to widen margins
svmfit <- svm(y~., data=dat, kernel="linear", cost=cst, scale=FALSE) #x1,x2 are on the same scale
plot(svmfit,dat)
ypred <- predict(svmfit, testdat)
cm <- table(predict=ypred, truth=testdat$y)
print(cm)
print(paste("cost 1 svm accuracy on separable classes :", sum(diag(cm))/sum(cm) ))


################################
pprint("Support Vector Machine")

#We study non-linear kernels

set.seed(1)
# 200x2 matrix
x<-matrix(rnorm(200*2),ncol=2)
x[1:100,] <- x[1:100,] + 2 #first half of lines strongly separated
x[101:150,] <- x[101:150,] -2 #next 50 lines also separated but in opposite direction
y<-c(rep(1,150), rep(2,50))
dat<-data.frame(x=x,y=as.factor(y))

plot(x,col=y, pch=19)

#lets see the svm fit with radial kernel
train <- sample(1:nrow(dat),nrow(dat)/2) #half half
cst <- 1
svmfit <- svm(y~., data=dat[train,], kernel="radial", cost=cst, gamma=1) 

plot(svmfit,dat[train,])
print( summary(svmfit))

#if you reduce margin, the decision boundary becomes more irregular
cst <- 1e5
svmfit <- svm(y~., data=dat[train,], kernel="radial", cost=cst, gamma=1) 
plot(svmfit,dat[train,])


## lets do the cross-validation with  radial kernel
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial",
             ranges=list(cost=c(0.1, 1,10,100,1000),
             gamma=c(0.5,1,2,3,4)) )
print(summary(tune.out))

#best params choice cost=1 gamma=2

cm <- table(true=dat[-train,"y"], pred=predict(tune.out$best.model ,newdata=dat[-train,]))
print(cm)
print(paste("radial cost=1 gamma=2 svm accuracy:", sum(diag(cm))/sum(cm) ))

#very high accurancy

#########################
pprint("ROC curves")

#ROC = receiver operating characteristic curve 

require(ROCR)
#define a func to make roc plot

rocplot <- function(pred,truth, ...){
    predob <- prediction(pred,truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf,...)
}

svmfit.opt <- svm(y ~ . ,data=dat[train,], kernel="radial", gamma=2, cost=1  )
pred <- predict(svmfit.opt, dat[train,], decision.values = TRUE) 
fitted <- attributes(pred)$decision.values 

par(mfrow=c(1,2))

rocplot(fitted, dat[train,"y"], main="Training Data")

#lets increase gamma to 50
svmfit.flex <- svm(y ~ . ,data=dat[train,], kernel="radial", gamma=50, cost=1  )
pred <- predict(svmfit.flex, dat[train,], decision.values = TRUE) 
fitted <- attributes(pred)$decision.values 
rocplot(fitted, dat[train,"y"], add=T, col="red")

#lets check roc on test data 
tpred <- predict(svmfit.opt, dat[-train,], decision.values = TRUE) 
fitted <- attributes(tpred)$decision.values 
rocplot(fitted, dat[-train,"y"], main="Test Data")

tpred <- predict(svmfit.flex, dat[-train,], decision.values = TRUE) 
fitted <- attributes(tpred)$decision.values 
rocplot(fitted, dat[-train,"y"], add=T, col="red")

#we see that on test data the best results are with gamma=2

###################################
pprint("SVM with Multiple Classes")

#lets add a 3rd class
set.seed(1)
x<-rbind(x,matrix(rnorm(50*2),ncol=2)) #more 50 lines
y<-c(y,rep(0,50))
x[y==0,2] <- x[y==0,2] + 2

dat <- data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

#now lets fit

svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit,dat)


#############################################
pprint("Application to Gene Expression Data")


require(ISLR)
data(Khan)  #  63 rows, 2308 features
print(names(Khan))
print(dim(Khan))
print(dim(Khan$xtrain))
print(dim(Khan$xtest))

#ytrain, ytest contain cancer subtype
print(table(Khan$ytrain))
print(table(Khan$ytest))

dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", const=10)
print(summary(out))

cm <- table(out$fitted, dat$y)
print( cm ) #no training errors
print(paste("gene train data accuracy:", sum(diag(cm))/sum(cm) ))

#lets check prediction on test data
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, dat.te)
cm <- table(pred.te, dat.te$y)
print( cm )
print(paste("gene test data accuracy:", sum(diag(cm))/sum(cm) ))









