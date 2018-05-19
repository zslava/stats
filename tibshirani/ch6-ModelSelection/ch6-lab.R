##ch6-lab


# rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch6-ModelSelection/");source("ch6-lab.R")


require(ISLR)
require(leaps)

data(Hitters)
names(Hitters)

##clean data
sum(is.na(Hitters))
Hitters <- na.omit(Hitters)
sum(is.na(Hitters))


## chose model using validation set and cross-validation

## 1.validation set approach 

set.seed(1) 
train <- sample( c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test <- !train
#sum(train)

p_features<- dim(Hitters)[2] -1

#fit on training set
regfit.best<-regsubsets(Salary~.,data=Hitters[train,], nvmax =p_features)

#compute validation set error

test.mat<-model.matrix(Salary~. ,data=Hitters[test,])

val.errors <- rep(NA,p_features)

for (i in 1:p_features){
	coefi <- coef(regfit.best,id=i) #best model of size k=i
    pred  <- test.mat[,names(coefi)]%*%coefi
    val.errors[i] <- mean( (Hitters$Salary[test]-pred)^2 )
}
plot(val.errors, ylab="test error", xlab="number of parametrs", type="l")
ve_min <- which.min(val.errors)
points(ve_min,val.errors[ve_min],col="red", cex=2, pch=20)

print(paste("test err min:",ve_min))
print(coef(regfit.best,id=ve_min))

## lets write the function

predict.regsubsets<-function(object,newdata,id,...){
    form<-as.formula(object$call[[2]])
    mat<-model.matrix(form,newdata)
    coefi<-coef(object,id=id)
    mat[,names(coefi)]%*%coefi
}

#lets use this function 
regfit.best<-regsubsets(Salary~. ,data=Hitters, nvmax =p_features)
print("coefs of best model of k=10 after a fit on all data")
print(coef(regfit.best ,ve_min))

##We now try to choose among the models of different sizes using cross- validation.
#for k-10 folds
k<-10
set.seed(1)
folds<-sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors <- matrix(NA,k,p_features, dimnames=list(NULL,paste(1:19)))
#lets write a loop that performance cross-validation
#looping on folds
for(j in 1:k ){

	best.fit <- regsubsets(Salary~.,data=Hitters[folds!=j,] ,nvmax=p_features)
   	for(i in 1:19){
   		pred<-predict.regsubsets(best.fit, Hitters[folds==j,], id=i)
   		cv.errors[j,i]<-mean( (Hitters$Salary[folds==j]-pred)^2)
   }	
}

mean.cv.errors <- apply(cv.errors, 2, mean)
print(paste("mean cv error:", paste(mean.cv.errors, collapse=",")))
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b', main="cross-validation curve for best subset selection")

min_cve_idx <- which.min(as.numeric(mean.cv.errors))
points(min_cve_idx, mean.cv.errors[min_cve_idx],col="red", cex=2, pch=20)


### lab 2 Ridge and Lasso 
library(glmnet)
x <- model.matrix(Salary~. ,data=Hitters)[,-1] #syntax different in lecture
#x <- model.matrix(Salary~.-1 ,data=Hitters)
y <- Hitters$Salary 

#ridge regression
grid <- 10^seq(10,-2,length=100) # array of 100 values from 10^10 to 10^-2 for lambda
ridge.mod <- glmnet(x,y, alpha=0, lambda=grid) # alpha = 0  is ridge
plot(ridge.mod)

print("model coeff for the lambda in the middle of the grid")
print(coef(ridge.mod)[,50])

print("to obtain ridge regression coefficien for lambda =50")
print(predict(ridge.mod, s=50,type="coefficients")[1:20,])

##split data
set.seed(1)
#split in half
full_idx <- 1:nrow(x)
train <- sample(full_idx, length(full_idx)/2 )
test <- full_idx[-train] 
y.test <- y[test]

#fit ridge on train
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda = grid, thresh=1e-12 ) #alpha=0 is ridge
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,]) #make predictions for lambda = 4
print(mean((ridge.pred-y.test)^2)) #mse on test for lambda = 4

#compute mse of the null model (only intercept as param)
print( mean( (mean(y[train])-y.test)^2 ))

#compute mse of the  for a very large lambda
ridge.pred <- predict(ridge.mod, s=1e10, newx=x[test,]) #make predictions for lambda  large
print(mean((ridge.pred-y.test)^2)) #mse on test for lambda = 1e10

#compute mse of the  for lambda = 0 , i.e. plain linear regression
ridge.pred <- predict(ridge.mod, s=0, newx=x[test,]) #make predictions for lambda = 4
print(mean((ridge.pred-y.test)^2)) #mse on test for lambda = 1e10


lm.reg<-lm(y~x, subset=train)
print(lm.reg)

print(predict(ridge.mod, s=0, exact=TRUE, type="coefficients")[1:20,] )



#lets find the lambda with lowest cv 
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam<-cv.out$lambda.min
print(bestlam)

#check test MSE for min lambda
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,]) #make predictions for lambda  large
print(mean((ridge.pred-y.test)^2))

##see coeefs for best lamada from the model refit on the all data
out=glmnet(x,y,alpha=0)
coefs<-predict(out,type="coefficients",s=bestlam)[1:20,]
print(coefs)

#### LASSo
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed (1)
cv.out <- cv.glmnet(x[train,], y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
print(bestlam)
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mse_bestlam_lasso <- mean((lasso.pred-y.test)^2)
print(mse_bestlam_lasso)

## see how lasso shrinks the model 
out <- glmnet(x,y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
print(lasso.coef)
print(lasso.coef[lasso.coef != 0])

##LAB 3 PCR  and PLS

#PCR
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE,validation ="CV")
print(summary(pcr.fit))
validationplot(pcr.fit, val.type="MSEP") #MSEP - cross-validation MSE to be plotted

#perform pcr on the training data and evaluate its test MSE 
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE,validation ="CV")
validationplot(pcr.fit,val.type="MSEP")
#from plot the min of v is when n of componenets =7
#find lowes CVE
pcr.pred <- predict(pcr.fit, x[test,], ncomp=7) 
mse_test_min <- mean((pcr.pred-y.test)^2)
print(mse_test_min)

#compare PCR test mse with those of Ridge and LASSO

pcr.fit <- pcr(y~x, scale=TRUE, ncomp=7) #on all dataset on 7 componentts
print(summary(pcr.fit))

### PLS
set.seed (1)
pls.fit=plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
print(summary(pls.fit))
validationplot(pls.fit,val.type="MSEP")

pls.pred <- predict(pls.fit, x[test,], ncomp=2) 
mse_test_min <- mean((pls.pred-y.test)^2)
print(mse_test_min)

# a pls fit on all data using ncomponents = 2
pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
print(summary(pls.fit))

