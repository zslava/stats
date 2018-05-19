# rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch5-Resampling/");source("ch5-ex_appl.R")


print("#####################################################")
print ("# ex 5.5")
print("####################################################")


#a) fit a logistic regression model 

require(ISLR)
print("summary of Default data set")
#print(summary(Default))
#pairs(Default, cex=0.3, col=Default$default)

#logistic regresion  trained on full data set
glm.fit <- glm(default~income+balance, data=Default, family=binomial)
#print(summary(glm.fit))

glm.probs<-predict(glm.fit,type="response")
glm.pred<-rep("No", length(glm.probs))
glm.pred[glm.probs > 0.5 ] <- "Yes"

#confusion matrix
print( table(glm.pred, Default$default))
print(paste("logreg income+balance error:", mean(glm.pred!=Default$default)))


#b) use validation apparoach estimate the test erro of this model
#i  lets put code which computes model errror in a funciton 
set.seed(1)

logreg_vsrun_tsterror<-function(Default){
	#make a new random smaple
	tsmpl <- sample(1:nrow(Default), nrow(Default)/2)  #training size is half of the dataset
	train_idx <- 1:nrow(Default) %in% tsmpl

	glm.fit <- glm(default~income+balance, data=Default, family=binomial, subset=train_idx)

	glm.probs<-predict(glm.fit,Default[!train_idx,], type="response")
	glm.pred<-rep("No", length(glm.probs))
	glm.pred[glm.probs > 0.5 ] <- "Yes"
    model_tst_err <- mean(glm.pred != Default[!train_idx,]$default)
    return (model_tst_err)
}

#c)
print(paste("default mode vs run  test error:", logreg_vsrun_tsterror(Default)))
print(paste("default mode vs run  test error:", logreg_vsrun_tsterror(Default)))
print(paste("default mode vs run  test error:", logreg_vsrun_tsterror(Default)))

runlng <- 3 # 10
tse <- rep(0,runlng)
for (i in 1:runlng){
	tse[i] <- logreg_vsrun_tsterror(Default)
}
print(paste("lgoreg income+balance mean:", mean(tse), "sd:", sd(tse)))
## we observe the  mean and sdandard devition of the model error rate

#d)
#redifined the function with a changed model
logreg_vsrun_tsterror_m1<-function(Default){
	#make a new random smaple
	tsmpl <- sample(1:nrow(Default), nrow(Default)/2)  #training size is half of the dataset
	train_idx <- 1:nrow(Default) %in% tsmpl

	glm.fit <- glm(default~income+balance+student, data=Default, family=binomial, subset=train_idx)

	glm.probs<-predict(glm.fit,Default[!train_idx,], type="response")
	glm.pred<-rep("No", length(glm.probs))
	glm.pred[glm.probs > 0.5 ] <- "Yes"
    model_tst_err <- mean(glm.pred != Default[!train_idx,]$default)
    return (model_tst_err)
}
runlng <- 13 #10
tse <- rep(0,runlng)
for (i in 1:runlng){
	tse[i] <- logreg_vsrun_tsterror(Default)
}
print(paste("lgoreg income+balance+student mean:", mean(tse), "sd:", sd(tse)))

## we observe that adding student to the model does not matter. i.e. a new  tst error and standard deviation
# lie  within the previous tst error and standard deviation. 

#z)  lets verify these results with k-fold validation
require(boot)
glm.fit<-glm(default~income+balance, data=Default, family=binomial)
cvk<-cv.glm(Default,glm.fit,K=5)
print(paste("logreg income+balance cv k-5 fold error:",cvk$delta[1]))

#the result 0.021 is lower than 0.026

print("#####################################################")
print ("# ex 5.6")
print("####################################################")

#a)
glm.fit<-glm(default~income+balance, data=Default, family=binomial)

print(coef(summary(glm.fit)))

#b)
boot.fn<-function(data,index){
	return ( coef(glm(default~income+balance, data=data, subset=index,family=binomial)) )
}

print(paste("coefs:",boot.fn(Default, 1:nrow(Default))))

#c)
print("applying bootstrap to for log reg model coefficients 50 times")
set.seed(1)
print(boot(Default,boot.fn,R=3)) #50 times expected value  3 for rapidity of exeuction

#we can observe standard errors  are similir using glm() and bootstrap method

print("#####################################################")
print ("# ex 5.7")
print("####################################################")
require(ISLR)
str(Weekly)
set.seed(1)
#a)
glm.fit<-glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)

#b)  log reg with all but the first obs
glm.fit<-glm(Direction~Lag1+Lag2, data=Weekly[-1,], family=binomial)

#c)
glm.probs <- predict(glm.fit, Weekly[1,], type="response")
glm.pred <- rep("Down",length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
print(paste("prediction fist observation of Weekly set:", glm.pred))
print("first observation of dataset")
print(Weekly[1,])
#we see the prediction is wrong

#d) 
## loocv using for loop
#lng <- nrow(Weekly)      ## uncomment this for true loocv
lng <- 10
iscorrect<-rep(0,lng)
for (i in 1:lng) {
	glm.fit<-glm(Direction~Lag1+Lag2, data=Weekly[-i,], family=binomial)
	glm.prob <- predict(glm.fit, Weekly[i,], type="response")
	glm.pred <- ifelse( glm.prob > 0.5, "Up", "Down")
	iscorrect[i] <- ifelse (glm.pred == Weekly[i,]$Direction, 1, 0 )
}

#e)
print(paste("loocv by for estimate:",mean(iscorrect)))

print("#####################################################")
print ("# ex 5.8")
print("####################################################")
##cross validation on  a simulated data set
#a)
set.seed(1)
nobs<-100

x<-rnorm(nobs)
y<-x-2*x^2+rnorm(100)

#n - 100  (obs)
#p = 2  x and another rnorm(100)

#y=f(x) = x- 2 * x^2 + \epsilon
#b)
plot(x,y)

#c)
require(boot)
set.seed(1)
dataset <- data.frame(x,y)


for (i in (1:4)){
	glm.fit<-glm(y~poly(x,i),data=dataset)
	cv.err<-cv.glm(dataset,glm.fit)
	print(paste("linear model degree",i,"loocv error:",cv.err$delta[1]))
}

#d)
set.seed(2)
print("using another seed")
for (i in (1:4)){
	glm.fit<-glm(y~poly(x,i),data=dataset)
	cv.err<-cv.glm(dataset,glm.fit,K=10)
	print(paste("linear model degree",i,"loocv error:",cv.err$delta[1]))
}
#we see results are the same

#e) polinomial model with degree 2 has the lowest loocv

#f)
print( summary(glm(y~poly(x,4),data=dataset)))

#we observer that p-values are significant only up to degree 2 of the  x. this agrees with loocv result


print("#####################################################")
print ("# ex 5.9")
print("####################################################")
#Boston data set 
require(MASS)
#a)
mu <- mean(Boston$medv)
print(paste("medv mu estimate:",mu))
#b)
mu_stderr <- sd(Boston$medv) / sqrt(nrow(Boston))
print(paste("1 estimate of medv std error:",mu_stderr))

#c 
medv_mu.fn <-function(data,index){
	medv_mu <- mean(data[index,]$medv) 
	return(medv_mu)
}
#print(  medv_mu.fn(Boston, 1:nrow(Boston)) )

btstrap <- boot(Boston,medv_mu.fn,1000)
print(btstrap)


#we observe tat mu standard error is close  to the previous estimate

#d)  95% confidence level
conf_intval95 <-c( btstrap$t0 - 2 * 0.4110794 , btstrap$t0 + 2 * 0.4110794 )
print("confidence 95% interval")
print(conf_intval95)
print(t.test(Boston$medv))
## bootstrap confidence interval is very close to the t.test 95%  confidence interval

#e)
med <- median(Boston$medv)
print(paste("median estimate:",med))
#f)
medv_median.fn <-function(data,index){
	med <- median(data[index,]$medv) 
	return(med)
}
#print(  medv_median.fn(Boston, 1:nrow(Boston)) ) #for debugging

btstrap <- boot(Boston,medv_median.fn,1000)
print("median medv by bootstrap")
print(btstrap)
# we observe that standard error of median is similar of that of the mean

#g)
mu_01<-quantile(Boston$medv, 0.1)
print(paste("10% quantile of medv:",mu_01))

medv_quant10.fn <-function(data,index){
	qmu <- quantile( data[index,]$medv, c(0.1) ) 
	return(qmu)
}
#print(  medv_quant10.fn(Boston, 1:nrow(Boston)) ) #for debugging

btstrap <- boot(Boston,medv_quant10.fn,1000)
print("10% quantile of medv by bootstrap")
print(btstrap)

#we obsert the estmate by the boostrap mathes the sample 10% quantile.  
#the standard error of the 10% quantile is of the same order of magnitude compared to stderr of mean, meddian
# the standard error is small relative to the estimate of 10% quantile


