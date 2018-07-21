##ch9-applied


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch9-SVM/");source("ch9-applied.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}

pprint("ex 1")

x1 <- -10:10
x2 <- 1 + 3*x1
plot(x1,x2,type="l", col="red", ylim=range(x1), main="hyperplanes as lines in 2D")
abline(v=0,  lty="dotted")
abline(h=0,  lty="dotted")
text(c(5), c(-5), "greater than 0", col = "red")
text(c(-8), c(-5), "less than 0", col = "red")
x2_p <- 1 - 0.5*x1
lines(x1,x2_p, type="l", col="blue")
text(c(5), c(3), "greater than 0", col = "blue")
text(c(-8), c(3), "less than 0", col = "blue")
legend("topright", c("x2=1 + 3 * x1", "x2=1 - 0.5*x1"),col=c("red", "blue"), cex=1, lty=1 )



pprint("ex 2")

## testing plotting a circle
x <-seq(-1,1,by=0.01)
y1<- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(x,y1,type="l", ylim=c(-1,1) )
lines(x,y2,type="l",col="blue")


#$(1+X_1)^2 + (2-X_2)^2 =4$ is a circle with radius 2 and center (-1,2)

#this is how to plot a circle
radius = 2
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1,
     xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(radius), add = TRUE,inches=FALSE)
abline(v=0,  lty="dotted")
abline(h=0,  lty="dotted")


pprint("ex3")

x1<-c(3,2,4,1,2,4,4)
x2<-c(4,2,4,4,1,3,1)
y<-c(rep(2,4),rep(4,3))

plot(x1,x2,col=y,pch=20)
grid()
abline(a=-0.5,b=1, lty="dashed")

# $$ 0.5 - X_1  + X_2   = 0$$

###c
# classify to red if $$0.5 - X_1  + X_2  < 0 $$ 
# classify to blue if $$0.5 - X_1  + X_2  >0 $$ 

###e
plot(x1,x2,col=y,pch=1)
grid()
abline(a=-0.5,b=1, lty="dashed")
points(x1[c(2,3,5,6)],x2[c(2,3,5,6)], col=y[c(2,3,5,6)], pch=20)




pprint("ex4")

set.seed(131)
ns <- 100

# x<-matrix(rnorm(ns*2),ncol=2)
# y<- c( rep(-1,ns/2) , rep(1,ns/2) )
# x[y==1,] <- x[y==1,] + 0.5
# dat <- data.frame(x=x, y=as.factor(y))

##generate quadratic dependency data
x1 <- rnorm(ns)
x2 <-  2 + x1^2 + rnorm(ns)
x2[1:(ns/2)] <- x2[1:(ns/2)] + 2  #seperate two half s up and down
x2[(ns/2+1):ns] <- x2[(ns/2+1):ns] - 2

y<- c( rep(-1,ns/2) , rep(1,ns/2) )
plot(x1,x2,col=(3-y))

dat <- data.frame(x1,x2,y=as.factor(y))

#divide to train and test sets half half
train <- c(sample(1:(ns/2), ns/4), sample( (ns/2+1):ns, ns/4) )
test  <- (1:ns)[-train]

dat_train <- dat[train,]
dat_test  <- dat[test,]

## checking visual data 
par(mfrow=c(1,2))
plot(as.matrix(dat[train,1:2]), col=(3-y[train]), main="train" )
plot(as.matrix(dat[test,1:2]), col=(3-y[test]), main="test" )



#linear
cst <- 10
svm_lin <- svm(y~., data=dat_train , kernel="linear", cost=cst, scale=FALSE)

cm <- table(fit=svm_lin$fitted, truth=dat_train$y)
print(cm)
print(paste("svm linear train errror", 1 - sum(diag(cm))/sum(cm) ))

#polynomial
svm_poly <- svm(y~., data=dat_train , kernel="polynomial", degrees=4, cost=cst, scale=FALSE)
cm <- table(fit=svm_poly$fitted, truth=dat_train$y)
print(cm)
print(paste("svm polynomial train error:", 1 - sum(diag(cm))/sum(cm) ))

#radial
svm_rad <- svm(y~., data=dat_train , kernel="radial", cost=cst, gamma=1)
cm <- table(fit=svm_rad$fitted, truth=dat_train$y)
print(cm)
print(paste("svm radial train error:", 1- sum(diag(cm))/sum(cm) ))



##plotting fits on train data

plot(svm_lin, dat_train, main="linear")
plot(svm_poly, dat_train, main="polynomial")
plot(svm_rad, dat_train, main = "radial")


#### lest verify on test error

##linear predict on test
lbl <- "svm linear test error"
yhat <- predict(svm_lin, dat_test)
cm <- table(predict=yhat, truth=dat_test$y)
print(cm)
print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))

##polynomial predict on test
lbl <- "svm polynomial test error"
yhat <- predict(svm_poly, dat_test)
cm <- table(predict=yhat, truth=dat_test$y)
print(cm)
print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))

##radial predict on test
lbl <- "svm radial test error"
yhat <- predict(svm_rad, dat_test)
cm <- table(predict=yhat, truth=dat_test$y)
print(cm)
print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))




#####
dev.off()

pprint("EX 5")
set.seed(421)
#Here we compare SVM with non-linear boundary with logistic regression using non-linear
#transformations

require(glmnet)

##a)
n<-500
p<-2
x1<-runif(n)-0.5
x2<-runif(n)-0.5
y<- (x1^2 - x2^2 >0) 
yy<-rep(-1,length(y))
yy[which(y==T)] <- 1

dat <- data.frame(x1,x2,y=as.factor(yy) )


##b)
plot(x1,x2,col=(3-yy),)

###c)
#define train, test as half 
train <- sample(n, n/2)
test <- (1:n)[-train]
dat_train <- dat[train,]
dat_test <- dat[test,]

fit_logi <- glm(y~x1+x2, data=dat, family=binomial)

##d)
yhat <- predict(fit_logi, dat_train, type="response")

ypred <- rep(-1, length(yhat))
ypred[which(yhat>0.528)] <- 1 

#we obtain a a linear boundary
plot(as.matrix(dat_train[,1:2]), col=(3-ypred) )


###e)
fit_logi_nl <- glm(y ~ poly(x1,2) + poly(x2,2) + I(x1*x2) , data=dat, family=binomial)

###f)
yhat <- predict(fit_logi_nl, dat_train, type="response")
ypred <- rep(-1, length(yhat))
ypred[which(yhat>0.5)] <- 1

##now we have a non-linear boundary
plot(as.matrix(dat_train[,1:2]), col=(3-ypred) )


cm <- table(predict=ypred, truth=dat_train$y)
print(cm)
lbl <- "logistic on non-linear x1 and x2,  train error"
print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))

##g)
cst <-10
svmfit <- svm(y ~ . , data=dat, kernel="radial",cost=cst, gamma=1)

#boundary
plot(svmfit, dat_train, main="train")

yhat <- predict(svmfit, dat_train)
ypred <- rep(-1, length(yhat))
ypred[which(yhat=="1")] <- 1

plot(as.matrix(dat_train[,1:2]), col=(3-ypred) )

cm <- table(predict=ypred, truth=dat_train$y)
print(cm)
lbl <- "svm on x1 , x2  train error"
print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))

#we observe a classification according to svm boundaries

##h)
svmfit_nl <- svm(y ~  x1 + x2 + x1^2 + x2^2 + x1*x2 ,
                data=dat, kernel="radial",cost=cst, gamma=1)

#plot(svmfit_nl, dat_train, main="train")

yhat <- predict(svmfit_nl, dat_train)
ypred <- rep(-1, length(yhat))
ypred[which(yhat=="1")] <- 1

plot(as.matrix(dat_train[,1:2]), col=(3-ypred) )


cm <- table(predict=ypred, truth=dat_train$y)
print(cm)
lbl <- "svm on non-linear x1 and x2,  train error"
print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))


##i )

#we observe that logicstic regresssion on non-linear functions of x1 and x2 finds a correct 
#non-linear boundaries,   but the error rate of svm is much better .  Small differnce between fitting
#sfm on linear x1 and x2 or non-linear x1 and x2



#########
pprint("ex 6")

###a
set.seed(3154)
##class 1
x.one <- runif(500, 0, 90)
y.one <-  runif(500, x.one + 10, 100)

x.one.noise <- runif(50,20,80)
y.one.noise <- 5/4 * (x.one.noise - 10) + 0.1

#class 0
x.zero <- runif(500, 0, 90)
y.zero <-  runif(500, 0, x.zero - 10)

x.zero.noise <- runif(50,20,80)
y.zero.noise <- 5/4 * (x.zero.noise - 10) - 0.1

#combine all 
class.one <- 1:550
class.zero <- (1:1100)[-class.one]

x <- c(x.one, x.one.noise, x.zero, x.zero.noise)
y <- c(y.one, y.one.noise, y.zero, y.zero.noise)
plot(x[class.one], y[class.one], col="blue", pch="+", ylim = c(0,100) )
points(x[class.zero], y[class.zero], col="red", pch=4 )


##b
# require(e1071)
# set.seed(555)
# z = rep(0, 1100)
# z[class.one] = 1
# data = data.frame(x = x, y = y, z = z)
# tune.out = tune(svm, as.factor(z) ~ ., data = data, kernel = "linear", ranges = list(cost = c(0.01, 
#     0.1, 1, 5, 10, 100, 1000, 10000)))
# print( summary(tune.out) )


# print( data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 
#     1100) )

#### c)
set.seed(1111)
x.test = runif(1000, 0, 100)
class.one = sample(1000, 500)
y.test = rep(NA, 1000)
# Set y > x for class.one
for (i in class.one) {  #index of class.one
    y.test[i] = runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {   #index of class zero
    y.test[i] = runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)
