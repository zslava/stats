##ch6-pract


# rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch6-ModelSelection/");source("ch6-lecture.R")

# 
# ..
#### Model selection using a validation set

require(ISLR)
require(leaps)

data(Hitters)
Hitters <- na.omit(Hitters)


set.seed(1)
nrows <- dim(Hitters)[1] #263
sample_size <- floor(2/3 * nrows) + 5  # training sample of size 180
train<-sample(seq(nrows),sample_size,replace=FALSE)
regfit.fwd <- regsubsets(Salary~. ,data=Hitters[train,]
						,nvmax=19,method="forward")


val.errors<-rep(NA,19) # 19 is thte max numb of predictors in the Hitters dataset
x.test <- model.matrix(Salary~. , data=Hitters[-train,]) #

for(i in 1:19){
   coefi <- coef(regfit.fwd,id=i)
   pred  <- x.test[,names(coefi)]%*%coefi #varieables * model coefficients
   val.errors[i] <- mean((Hitters$Salary[-train]-pred)^2)
}
plot(sqrt(val.errors), ylab="Root MSE", ylim=c(300,400), pch=19,type="b")
points(sqrt(regfit.fwd$rss[-1]/sample_size), col="blue", pch=19,type="b")
legend("topright", legend=c("Training", "Validation"), col=c("blue","black"), pch=19)

#lets rewrite the code above in the function
predict.regsubsets<-function(object,newdata,id,...){
	form<-as.formula(object$call[[2]])
	mat<-model.matrix(form,newdata)
	coefi<-coef(object,id=id)
	mat[,names(coefi)]%*%coefi
}


#### model selection by cross validation
print(paste("##### model selection by cross validation #####"))
set.seed(11)
#index for sample
folds <- sample(rep(1:10, length=nrow(Hitters)))
print("folds: counts of elemnts")
print(table(folds))
cv.errors<-matrix(NA,10,19)  #matrix 10 rows 19 columns
## loop on k folds
for (k in 1:10){
	best.fit<-regsubsets(Salary~. ,data=Hitters[folds!=k,]
						,nvmax=19 ,method="forward")

	#browser()
	## loop on p predictors
	for (i in 1:19){
		pred<-predict.regsubsets(best.fit,Hitters[folds==k,],id=i)
		cv.errors[k,i] <- mean( (Hitters$Salary[folds==k] - pred)^2)
	}
 
}
#print(cv.errors)
rmse.cv<-sqrt(apply(cv.errors, 2,mean))
plot(rmse.cv, pch=19,type="b", main="cross-validaton curve")

#### Ridge regression and the Lasso
print(paste("##### Ridge Regression and the Lasso #####"))

library(glmnet)
x<-model.matrix(Salary~.-1,data=Hitters)
y<-Hitters$Salary

## first fwe fit a ridge-regression model
fit.ridge<-glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge<-cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

## now we fit a lasso model: for this we use the default alpha=1
fit.lasso<-glmnet(x,y)
plot(fit.lasso,xvar="lambda", label=TRUE)
cv.lasso <- cv.glmnet(x,y)
plot(cv.lasso)
print(coef(cv.lasso))

##suppose we want to use our ealier  train/validation divistion to select the lambda for the lasso

lasso.tr<- glmnet(x[train,],y[train])
print(lasso.tr)
pred<-predict(lasso.tr,x[-train,])
print(dim(pred))
rmse <- sqrt(apply((y[-train]-pred)^2,2,mean ))

plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best <- lasso.tr$lambda[order(rmse)[1]]
print(lam.best)
print(coef(lasso.tr,s=lam.best))



