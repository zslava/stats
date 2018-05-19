##ch6-exercises-applied


# rm(list=ls());  if (length(dev.list())>0){graphics.off()} ;setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch6-ModelSelection/");source("ch6-ex-applied.R")


require(leaps)


### applied exercies
print("######################################")
print("##")
print("## ex 8")
print("##")
print("######################################")

set.seed(1)
##a)
n<-100
x<-rnorm(n)
epsilon <-rnorm(n) 

##b)

beta0<- 3
beta1<- 2
beta2<- -3
beta3<- 0.3

y<-beta0 + beta1*x + beta2*x^2 + beta3*x^3 + epsilon

#c)
dataset <- data.frame(y,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10)
colnames(dataset) <- c("response", "x1", "x2", "x3", "x4", "x5"
					  ,"x6", "x7", "x8", "x9", "x10")
p_feats<-10
regfit.full<-regsubsets(response~., data=dataset, nvmax=p_feats)
reg.summary<-summary(regfit.full)
print(reg.summary)


plot_regularization_measures<-function(model, label="best subset") {
	model.summary <- summary(model)
	xlbl <- paste(label, ": #params")
	if (length(dev.list())>0) { dev.new() }
	par(mfrow=c(2,2))
	plot(model.summary$rss ,xlab=xlbl,ylab="RSS",type="l")
    #adjusted r squared
	plot(model.summary$adjr2, xlab=xlbl,ylab="Adjusted RSq", type="l")
	idx1<-which.max(model.summary$adjr2)
	points(idx1, model.summary$adjr2[idx1],col="red", cex=1, pch=20)
    #CP
	plot(model.summary$cp, xlab=xlbl,ylab="Cp", type="l")
	idx2<-which.min(model.summary$cp)
	points(idx2, model.summary$cp[idx2],col="red", cex=1, pch=20)
    #BIC
	plot(model.summary$bic, xlab=xlbl,ylab="BIC", type="l")
	idx3<-which.min(model.summary$bic)
	points(idx3, model.summary$bic[idx3],col="red", cex=1, pch=20)
    par(mfrow=c(1,1)) 
}

plot_regularization_measures(model=regfit.full)

#d) forward selection

regfit.fwd<-regsubsets(response~., data=dataset, nvmax=p_feats, method="forward")
reg.summary<-summary(regfit.fwd)

plot_regularization_measures(model=regfit.fwd, label="forward selection")

print("coefficients for the forward selection best model")
print(coefficients(regfit.fwd , id=3))


#backard selection

regfit.bwd<-regsubsets(response~., data=dataset, nvmax=p_feats, method="backward")
reg.summary<-summary(regfit.bwd)

plot_regularization_measures(model=regfit.bwd, label="backward selection")

print("for backward and forward selection results are identical as for best subset selection")


##e) lasso)
require(glmnet)

xv <- model.matrix(response~., data=dataset)[,-1]
yv <- dataset$response
set.seed(1)
grid <- 10^seq(10,-2,length=100)


cv.out <- cv.glmnet(xv, yv, alpha=1 ) #alpha=1 - lasso
bestlam <- cv.out$lambda.min 
print(paste("lasso cv best lambda:",bestlam))

par(mfrow=c(1,1))
plot(cv.out)

lasso.mod <- glmnet(xv,yv, alpha=1, lambda=grid, thresh=1e-12)
lasso.pred <- predict(lasso.mod, s=bestlam, type="coefficients")
print(lasso.pred)
print("we observe that coefficents x0, x1 and x2 match closely the betas of the real Y function")


##f)
#new model 


beta7 <- 7

y <- beta0 + beta7 * x^7 + epsilon

dataset2 <- data.frame("y" = y, "x" = x)
#colnames(ds2) <- c("response", "x7")

regfit.full<-regsubsets(y~poly(x,7,raw=TRUE), data=dataset2, nvmax=7)

reg.summary<-summary(regfit.full)
print(reg.summary)

plot_regularization_measures(model=regfit.full, label="best subset dataset2")


print(coefficients(regfit.full, id=1))
print(coefficients(regfit.full, id=2))
print(coefficients(regfit.full, id=4))

print("we observe that model with 1 predictor picked by BIC is the most accurate")

#lasso
xmat = model.matrix(y~poly(x, 7, raw=T), data=dataset2)[, -1]
cv.lasso <- cv.glmnet(xmat, y, alpha=1)
plot(cv.lasso)
bestlam <- cv.lasso$lambda.min 
print(paste("best lambda:", bestlam))
lasso.mod <- glmnet(xmat, y, alpha=1, lambda=grid, thresh=1e-12)
lasso.pred <- predict(lasso.mod, s=bestlam, type="coefficients")
print(lasso.pred)

print("we observe that lasso's coefficients are close the betas of real model, interecept is quite off")

print("######################################")
print("##")
print("## ex 9")
print("##")
print("######################################")

rm(list=ls())

require(ISLR)
data(College)

set.seed(11)
## a)
#split in half
full_idx <- 1:nrow(College)
train <- sample(full_idx, length(full_idx)/2 )
test <- full_idx[-train] 

#b)

lm.mod <- lm(Apps~., data=College, subset=train)
lm.mod.sum <- summary(lm.mod)
print(lm.mod.sum)

y<-College$Apps

lm.pred <- predict(lm.mod, College[test,])
mse_lm <- mean( (y[test] -lm.pred)^2 )
print(paste("test MSE linear model: ",mse_lm))

#c)

require(glmnet)

full.mat <- model.matrix(Apps~., data=College)

train.mat <- model.matrix(Apps~., data=College[train,])
test.mat <- model.matrix(Apps~., data=College[test,])

y <- College$Apps

y.train <- y[train]
y.test <- y[test]

grid <- 10^seq(10,-2,length=100)


#cv.out <- cv.glmnet(x[train,], y[train], alpha=0 ) #ridge on train
#bestlam <- cv.out$lambda.min 

ridge.mod <- cv.glmnet(train.mat, y.train, alpha=0, lambda=grid, thresh=1e-12)
bestlam <- ridge.mod$lambda.min
print(paste("cv Ridge best lambda:",bestlam))

ridge.pred <- predict(ridge.mod, s=bestlam, newx=test.mat)
mse_ridge_best <- mean((ridge.pred-y.test)^2)
print(paste("test MSE  ridge model: ",mse_ridge_best))


#d) lasso
lasso.mod <- cv.glmnet(train.mat, y.train, alpha=1 , lambda=grid, thresh=1e-12) #lasso on train
bestlam <- lasso.mod$lambda.min 
print(paste("cv Lasso best lambda:",bestlam))

lasso.pred <- predict(lasso.mod, s=bestlam, newx=test.mat)
mse_lasso_best <- mean((lasso.pred-y.test)^2)
print(paste("test MSE  lasso model: ",mse_lasso_best))

lasso.mod <- glmnet(full.mat, y, alpha=1)
lasso.coefs <-predict(lasso.mod,s=bestlam, type="coefficients")
print(lasso.coefs)

#e)  PCR 
library(pls)

pcr.fit <- pcr(Apps~. ,data=College,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred <- predict(pcr.fit, College[test,], ncomp=9)
mse_pcr <- mean( (pcr.pred - y.test)^2 )
print(paste("test MSE  PCR ncomp=9: ",mse_pcr))

#f)  PLS
pls.fit <- plsr(Apps~. ,data=College,subset=train
	           ,scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred <- predict(pls.fit, College[test,], ncomp=7)
mse_pls <- mean( (pls.pred - y.test)^2 )
print(paste("test MSE  PLS ncomp=7: ",mse_pls))

#g)
print("from the values of test MSEs, all models have comparable predictive power")

#from asad solution lets compare R squared of all 5 models 
test.avg <- mean(College[test,]$Apps)
tss <- mean( (y.test  - test.avg)^2)
lm.test.r2 <-    1 - mse_lm / tss 
ridge.test.r2 <- 1 - mse_ridge_best / tss 
lasso.test.r2 <- 1 - mse_lasso_best / tss 
pcr.test.r2 <-   1 - mse_pcr /tss 
pls.test.r2  <-  1 - mse_pls / tss 

barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2)
		,col=2, names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS")
		,main="Test R-squared")


print("######################################")
print("##")
print("## ex 10")
print("##")
print("######################################")

rm(list=ls())
graphics.off()

#a)
set.seed(1)
p<-20
n<-1000
X = matrix(rnorm(n*p), n, p)
B = rnorm(p)
#set some values of B to zero
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
eps = rnorm(p)

Y <- X%*% B + eps 

#b)
full_idx <- 1:nrow(X)
train <- sample(full_idx, length(full_idx) * 1/10 )
test <- full_idx[-train] 

#c)
library(leaps)
datas <- data.frame(Y,X)
p_feats <- dim(datas)[2]-1 ## all features
regbsub <- regsubsets(Y~., data=datas[train,], nvmax=p)
regbsum <- summary(regbsub)
#print(reg.best.sum)
plot( regbsum$rss/ length(train),xlab="Number of Variables ",ylab="training MSE",
type="b", pch=19,main="Best subset select")

#sol asad compute manually train MSE  for all models
x.train <- datas[train,-1]
y.train <- datas[train,"Y"]
x.test <- datas[test,-1]
y.test <- datas[test,"Y"]

val.errors <- rep(NA,p)

x_cols <- colnames(datas)[-1]
for(i in 1:p){
	coefi <- coef(regbsub, id=i)
	pred <- as.matrix( x.train[,x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols ]
	val.errors[i] <- mean((y.train - pred)^2)
}

# val.errors == regbsum$rss / length(train)

#d)  test mse
val.t.errors <- rep(NA,p)

for(i in 1:p){
	coefi <- coef(regbsub, id=i)
	pred <- as.matrix( x.test[,x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols ]
	val.t.errors[i] <- mean((y.test - pred)^2)
}
plot(val.t.errors,ylab="test MSE", xlab="Number of variables", type="b", pch=19, main="Best subset select")

#e)
idx <- which.min(val.t.errors)
points(idx, val.t.errors[idx], col="red", pch=20)

#f)
print("true betas")
print(B)
print( coef(regbsub,id=16) )
print("coefs of model with 16 params match quite well the original betas")


## g)

print("######################################")
print("##")
print("## ex 11")
print("##")
print("######################################")

rm(list=ls())
graphics.off()
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)

data(Boston)

## a)
## best subset selection with k-fold validation

## define predict for bset subset selection function (to compute test mse)
predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id = id)
    mat[, names(coefi)] %*% coefi
}

k<-10  #k-fold validation
p <- ncol(Boston)-1

folds<-sample(rep(1:k, length=nrow(Boston)) )
#print(table(folds))

cv.errors <- matrix(NA,k,p)

#loop on k folds
for (j in 1:k){
	best.fit <- regsubsets(crim~. ,data=Boston[folds!=j,], nvmax =p) #on train
	for (i in 1:p){
		pred <- predict.regsubsets(best.fit, Boston[folds==j,], id=i  ) #predictions of ith model
		cv.errors[j,i] <- mean( (pred - Boston$crim[folds==j] )^2 )
	}
}
rmse.cv <- sqrt(apply(cv.errors, 2, mean))

plot(rmse.cv, type="b", pch=19,main="X-validation curve for best subset", xlab="# params"
	 ,ylab="RMSE")


min_pars <- which.min(rmse.cv)

bestsub_coefs <- coefficients(best.fit,id=min_pars)
print(bestsub_coefs)

rmse.cv.bestsubset <- rmse.cv[min_pars]
print(paste("best subset min rmse:",rmse.cv.bestsubset))

# lasso

x <- model.matrix(crim~.-1,data=Boston)
y <- Boston$crim 
cv.lasso <- cv.glmnet(x,y,type.measure="mse",alpha=1) 
plot(cv.lasso)
print(coef(cv.lasso))
rmse.cv.lasso <- sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])
print(paste("lasso min rmse:",rmse.cv.lasso))


#ridge 

cv.ridge <- cv.glmnet(x,y,type.measure="mse",alpha=0)
plot(cv.ridge)
print(coef(cv.ridge))
rmse.cv.ridge <- sqrt(cv.ridge$cvm[ cv.ridge$lambda == cv.ridge$lambda.1se ] )
print(paste("ridge min rmse:",rmse.cv.ridge))

#pcr 
library(pls)
pcr.fit <- pcr(crim~., data=Boston,scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="RMSEP")
print(summary(pcr.fit))



##### chosen best subset selectio wn with 9 params
## lets see cofs of linear model with this params trained on all data

sel.lm.fit <- lm(crim~zn+indus+nox+dis+rad+ptratio+black+lstat+medv, data=Boston)
print(summary(sel.lm.fit))
sel.pred <- predict(sel.lm.fit)
sel.rmse <- sqrt(mean ( (y - sel.pred)^2))
