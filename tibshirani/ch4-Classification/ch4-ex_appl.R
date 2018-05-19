#  if ( length(dev.list()) != 0){dev.off()} ; rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch4-Classification/");source("ch4-ex_appl.R")

##### ch4  applied exercices

library(ISLR)

str(Weekly)


#### chap4 ex 10
##10.b) 
#logistic regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume
	          ,data=Weekly, family=binomial)
print(summary(glm.fit))


#10.c)  compute confusion matrix

logr.proba <- predict(glm.fit, type="response")
logr.pred <- rep("Down", length(logr.proba))
logr.pred[logr.proba > .5] <- "Up"

logr.cfm <- table(logr.pred, Weekly$Direction)
print(logr.cfm)

print(paste("log reg model correct rate:",mean(logr.pred == Weekly$Direction)))
print(paste("log reg model error rate:", 1 - mean(logr.pred == Weekly$Direction)))

#10.d) log reg over training (1990-2008) and test >= 2009
# check predictions over test data

#split to training and test  sets
train_idx <- Weekly$Year < 2009
Weekly_train <- Weekly[train_idx,]
Weekly_test  <- Weekly[!train_idx,]
print(dim(Weekly_train))
print(dim(Weekly_test))

glm.fit_tr <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume
	          ,data=Weekly_train, family=binomial)
print(summary(glm.fit_tr))

logr.proba_ts <- predict(glm.fit_tr, Weekly_test, type="response")
logr.pred_ts <- rep("Down", length(logr.proba_ts))
logr.pred_ts[logr.proba_ts > .5] <- "Up"
print(length(logr.pred_ts))

logr.cfm_ts <- table(logr.pred_ts, Weekly_test$Direction)
print(logr.cfm_ts)
print(paste("log reg same model correct rate on test:",mean(logr.pred_ts == Weekly_test$Direction)))
print(paste("log reg same model error rate on test:", 1 - mean(logr.pred_ts == Weekly_test$Direction)))

## so using he same logistic regresson on all predictor
## we have a worsenning of error rate on test 

#log reg using just Lag2 as the only predictor
glm.fit_tr2 <- glm(Direction~Lag2,data=Weekly_train, family=binomial)
print(summary(glm.fit_tr2))

logr.proba_ts <- predict(glm.fit_tr2, Weekly_test, type="response")
logr.pred_ts <- rep("Down", length(logr.proba_ts))
logr.pred_ts[logr.proba_ts > .5] <- "Up"


logr.cfm_ts <- table(logr.pred_ts, Weekly_test$Direction)
print(logr.cfm_ts)
print(paste("log reg Lag2 model correct rate on test:",mean(logr.pred_ts == Weekly_test$Direction)))
print(paste("log reg Lag2 model error rate on test:", 1 - mean(logr.pred_ts == Weekly_test$Direction)))



##10.e)  repeat d) using LDA
require(MASS)
lda.fit <- lda(Direction~Lag2, data=Weekly, subset=train_idx)
lda.pred <- predict(lda.fit, Weekly_test)
lda.class <- lda.pred$class 

lda.cfm <- table(lda.class, Weekly_test$Direction)
print(lda.cfm)
print(paste("LDA Lag2 model correct rate on test:",mean(lda.class == Weekly_test$Direction)))
print(paste("LDA Lag2 model error rate on test:", 1 - mean(lda.class == Weekly_test$Direction)))

##10.f  repead d) using QDA
qda.fit <- qda(Direction~Lag2, data=Weekly, subset=train_idx)
qda.pred <- predict(qda.fit, Weekly_test)
qda.class <-qda.pred$class 
qda.cfm <- table(qda.class, Weekly_test$Direction)
print(qda.cfm)
print(paste("QDA Lag2 model correct rate on test:",mean(qda.class == Weekly_test$Direction)))
print(paste("QDA Lag2 model error rate on test:", 1 - mean(qda.class == Weekly_test$Direction)))


##10.g  repeat d) knn with k=1
library(class)

train.X <- cbind(Weekly_train$Lag2)
test.X <- cbind(Weekly_test$Lag2)


getKnnPrediction <- function(train, test, train_y, test_y, k=1){ 
	set.seed (1)
	knn.pred <- knn(train,test,train_y, k=k)
	cfm_knn <- table(knn.pred, test_y)
	print(cfm_knn) 
    print(paste("knn model error rate on test for k =",k,":",
    	1-mean(knn.pred == test_y)))
}

getKnnPrediction(train=train.X, test=test.X,
	             train_y=Weekly_train$Direction, test_y=Weekly_test$Direction,k=1)


##10.i 
##combine lag1 and lag2 into averaged lag 
Weekly_t <- Weekly 
Weekly_t$Lag12 <- 0.5*(Weekly_t$Lag1+Weekly_t$Lag2)
Weekly_t_train <- Weekly_t[train_idx,]
Weekly_t_test  <- Weekly_t[!train_idx,]

lda.fit <- lda(Direction~Lag12, data=Weekly_t, subset=train_idx)
lda.pred <- predict(lda.fit, Weekly_t_test)
lda.class <- lda.pred$class 

lda.cfm <- table(lda.class, Weekly_t_test$Direction)
print(lda.cfm)
print(paste("LDA Lag12 model correct rate on test:",mean(lda.class == Weekly_t_test$Direction)))
print(paste("LDA Lag12 model error rate on test:", 1 - mean(lda.class == Weekly_t_test$Direction)))


#check Log Reg with Lag1:lAg2   i.e. with inrecaction term

glm.fit_tr2 <- glm(Direction~Lag2:Lag1,data=Weekly_train, family=binomial)

logr.proba_ts <- predict(glm.fit_tr2, Weekly_test, type="response")
logr.pred_ts <- rep("Down", length(logr.proba_ts))
logr.pred_ts[logr.proba_ts > .5] <- "Up"


logr.cfm_ts <- table(logr.pred_ts, Weekly_test$Direction)
print(logr.cfm_ts)
print(paste("log reg Lag2:Lag1 model correct rate on test:",mean(logr.pred_ts == Weekly_test$Direction)))
print(paste("log reg Lag2:Lag1 model error rate on test:", 1 - mean(logr.pred_ts == Weekly_test$Direction)))

#####################################################
#####################################################
## chap 4 ex 11
##predict car's gas mileage based on Auto data set
print("")
print("####################################################")
print("ex 11")
print("####################################################")

#a)
mpg01 = rep(0, nrow(Auto))
idx<- Auto$mpg > median(Auto$mpg)
mpg01[idx] <- 1
Auto <- cbind(Auto, mpg01)

#b)
#pairs(Auto,cex=0.1)

print("correlations of Auto")
print(cor(Auto[,-9]))

par(mfrow=c(2,3))
plot(mpg01~cylinders, data=Auto, cex=0.1, col=1)
plot(mpg01~horsepower, data=Auto, cex=0.1, col=2)
plot(mpg01~weight, data=Auto, cex=0.1, col=3)
plot(mpg01~acceleration, data=Auto, cex=0.1, col=4)
plot(mpg01~displacement, data=Auto, cex=0.1, col=5)
par(mfrow=c(1,1))

# it appears from visual inspection that { cylinders,horsepower, weight, acceleration, displacment} are the
#best suited predictors 


#c)

# train_idx <- seq(1,300)
# test_idx <-seq(301, nrow(Auto))

#the split from Asad
train_idx <- (Auto$year %% 2 == 0 )
test_idx  <- !train_idx 

Auto_train <- Auto[train_idx,]
Auto_test <- Auto[test_idx,]

#d) perform lda on train to predict mpg01
lda.a.fit <- lda(mpg01~cylinders+horsepower+weight+acceleration+displacement
	         ,data=Auto_train)
print(summary(lda.a.fit))
lda.a.pred <- predict(lda.a.fit, Auto_test)
lda.a.class <- lda.a.pred$class 

lda.a.cfm <- table(lda.a.class, Auto_test$mpg01)
print(lda.a.cfm)
print(paste("LDA cyl+hp+wght+accel+displ eror rate on test:"
	        ,1-mean(lda.a.class == Auto_test$mpg01)))

#e) do the same model with QDA

qda.a.fit <- qda(mpg01~cylinders+horsepower+weight+acceleration+displacement
	         , data=Auto_train)
qda.a.pred <- predict(qda.a.fit, Auto_test)
qda.a.class <-qda.a.pred$class 
qda.a.cfm <- table(qda.a.class, Auto_test$mpg01)
print(qda.a.cfm)
print(paste("QDA cyl+hp+wght+accel+displ eror rate on test:"
	 ,1 - mean(qda.a.class == Auto_test$mpg01)))

#f) do the same model with Log Reg

#glm.a.fit <- glm(mpg01~horsepower
glm.a.fit <- glm(mpg01~cylinders+horsepower+weight+acceleration+displacement
	           ,data=Auto_train, family=binomial)
print(summary(glm.a.fit))

logr.a.proba <- predict(glm.a.fit, Auto_test, type="response")
logr.a.pred <- rep(0, length(logr.a.proba))
logr.a.pred[logr.a.proba > .5] <- 1


logr.a.cfm <- table(logr.a.pred, Auto_test$mpg01)
print(logr.a.cfm)
print(paste("LogReg cyl+hp+wght+accel+displ error rate on test:"
	, 1 - mean(logr.a.pred == Auto_test$mpg01)))

#f) repeat with knn
train.X <- cbind(Auto_train$cylinders, Auto_train$horsepower
	           ,Auto_train$weight, Auto_train$acceleration, Auto_train$displacement)
test.X <- cbind(Auto_test$cylinders, Auto_test$horsepower
	           ,Auto_test$weight, Auto_test$acceleration, Auto_test$displacement)

getKnnPrediction(train=train.X, test=test.X,
	             train_y=Auto_train$mpg01, test_y=Auto_test$mpg01,k=1)
getKnnPrediction(train=train.X, test=test.X,
	             train_y=Auto_train$mpg01, test_y=Auto_test$mpg01,k=3)
getKnnPrediction(train=train.X, test=test.X,
	             train_y=Auto_train$mpg01, test_y=Auto_test$mpg01,k=10)
getKnnPrediction(train=train.X, test=test.X,
	             train_y=Auto_train$mpg01, test_y=Auto_test$mpg01,k=20)
getKnnPrediction(train=train.X, test=test.X,
	             train_y=Auto_train$mpg01, test_y=Auto_test$mpg01,k=50)

getKnnPrediction(train=train.X, test=test.X,
	             train_y=Auto_train$mpg01, test_y=Auto_test$mpg01,k=100)


## chap 4 ex 11
##predict car's gas mileage based on Auto data set
print("")
print("####################################################")
print("ex 12")
print("####################################################")


#a)
Power <- function(){
	result <- 2**3
	print(paste("result of 2^3:",result))
}

Power()
#b)
Power2 <- function(x,a){
	result <- x**a
	print(paste("result of ",x,"power",a,":",result))
}

Power2(x=2,a=10)

#c)
Power2(10,3)
Power2(8,17)
Power2(131,3)

#d)
Power3 <- function(x,a){
	result <- x**a
	return(result)
}

print(Power3(2,5))

#e)
x<-seq(1,10,by=0.1)
plot(x,Power3(x,2),cex=0.3,col=2,log="x")

#f)
PlotPower <- function(xvec, a,isLogX=FALSE) {
	if (isLogX){
		plot(xvec, Power3(xvec,a), cex=0.3,col=3, log="x") 

	}else{			
		plot(xvec, Power3(xvec,a), cex=0.3,col=3) 
	}
}

PlotPower(seq(1,10,by=0.1),a=2)

dev.new()
PlotPower(seq(-10,10,by=0.1),a=3,isLogX=FALSE)


## chap 4 ex 13
##Using Boston dataset predict if suburb crime rate is above or below the median 
print("")
print("####################################################")
print("ex 13")
print("####################################################")


#create a new variable 0 below median, 1 above median
crim01 <- rep(0, times=nrow(Boston))
crim01[Boston$crim > median(Boston$crim)] <- 1

Boston <- data.frame(Boston, crim01)

print(paste("sorted correlations of crim01:"))
sort(cor(Boston)[15,])


#split dataset into train 3/4 and test 1/4

tr_lng <- floor( 0.75 * nrow(Boston))

train_idx <- rep(FALSE, nrow(Boston))
random_idx<-sample(1:nrow(Boston), tr_lng, replace=FALSE)
train_idx[random_idx] <- TRUE
test_idx <- !train_idx 

# asad selecton of indices
#train_idx = 1:(dim(Boston)[1]/2)
#test_idx = (dim(Boston)[1]/2+1):dim(Boston)[1]



##logistic regression

glm.fit <- glm(crim01~.-crim01-crim,data = Boston, family=binomial,subset=train_idx)
print(summary(glm.fit))

runLogReg <- function(logregfit, data_test, y_test, y_label="ylabel", proba_threshold=0.5) {
	glm.probs <- predict(logregfit, data_test, type="response")
	glm.pred <- rep(0, length(glm.probs))
	glm.pred[glm.probs > proba_threshold ] <- 1
	glm.cfm <-  table(glm.pred, y_test)
	print(paste("confusion matrix for response:",y_label))
	print(glm.cfm)
	print(paste("LogReg error rate for response",y_label,":", mean(glm.pred != y_test)))
}

runLogReg( glm.fit, data_test=Boston[test_idx,], y_test=Boston[test_idx,]$crim01, y_label="crim01" )


# glm.fit_b <- glm(crim01~nox+dis+rad+tax+ptratio+black+medv
# 				,data = Boston, family=binomial,subset=train_idx)
#LogReg( glm.fit_b, data_test=Boston[test_idx,], y_test=Boston[test_idx,]$crim01 )


##LDA 
require(MASS)
lda.fit <- lda(crim01~.-crim01-crim,data = Boston,subset=train_idx)

runLQDA <- function(lqdafit, data_test, y_test, atype="LDA", y_label="ylabel"){
	da.pred <- predict(lqdafit, data_test)
    cfm <- table(da.pred$class, y_test)
    print(paste("confusion matrix for response:",y_label))
    print(cfm)
	print(paste(atype,"error rate for response",y_label,":", mean(da.pred$class != y_test)))
}

runLQDA(lda.fit, data_test=Boston[test_idx,], y_test=Boston[test_idx,]$crim01
	    ,atype="LDA", y_label="crim01" )

#qda
qda.fit <- qda(crim01~.-crim01-crim,data = Boston,subset=train_idx)

runLQDA(qda.fit, data_test=Boston[test_idx,], y_test=Boston[test_idx,]$crim01
	    ,atype="QDA", y_label="crim01" )

#knn
library(class)

getKnnPrediction <- function(train, test, y_train, y_test, y_label="ylabel",k=1){ 
	set.seed (1)
	knn.pred <- knn(train,test,y_train, k=k)
	cfm_knn <- table(knn.pred, y_test)
	print(cfm_knn) 
    print(paste("knn error rate on test for response",y_label,"with k =",k,":"
    	 ,mean(knn.pred != y_test)))
}

#strip off crim and crim01 column to get a dataset of predictors
Boston_train <- Boston[train_idx,c(-1,-15)]
Boston_test <- Boston[test_idx,c(-1,-15)]

getKnnPrediction(Boston_train, Boston_test,Boston[train_idx,]$crim01
	            ,Boston[test_idx,]$crim01,y_label="crim01", k=1)
getKnnPrediction(Boston_train, Boston_test,Boston[train_idx,]$crim01
	            ,Boston[test_idx,]$crim01,y_label="crim01", k=3)
getKnnPrediction(Boston_train, Boston_test,Boston[train_idx,]$crim01
	            ,Boston[test_idx,]$crim01,y_label="crim01", k=10)
getKnnPrediction(Boston_train, Boston_test,Boston[train_idx,]$crim01
	            ,Boston[test_idx,]$crim01,y_label="crim01", k=100)




