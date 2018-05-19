#rm(list=ls());setwd("c:/users/zimine/Dropbox/cs/bigdata/lagunita/tibshirani/ch4-Classification/");source("ch4-pract.R")
# rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch4-Classification/");source("ch4-pract.R")

##### ch4 lab

library(ISLR)
## sp500 5 lags returns dataset
print(names(Smarket))
print(dim(Smarket))
print(str(Smarket))
print(summary(Smarket))

##correlation between all numeric columsn
print(cor(Smarket[,-9]))
#visualization of variables dependencies
##pairs(Smarket, cex=0.1)


##logistic regression model. 
# we will fit a logistic regression model to predict direction using lag1 through lag5
#glm generalized linear model
attach(Smarket)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)

print(summary(glm.fit))

#how to print p-values of coefficients
print(summary(glm.fit)$coef[,4])

#predict direction using the training data  to store probabilities.
glm.probs <- predict(glm.fit, type="response")

#convert probabilities into a binar reponse  ? P> 0.5 : class 1 ; class b
 glm.pred<-rep("Down",length(glm.probs))
 glm.pred[glm.probs > .5] <- "Up"

print(glm.pred[1:10])

cfu_m <- table(glm.pred,Direction)
print(cfu_m)

#model is correct  this percentage
(cfu_m[1,1]+cfu_m[2,2] )/ sum(cfu_m)
#this also equals
print(paste("percengate that model is correct:",mean(glm.pred == Direction)))
print(paste("training error rate:", 1 - mean(glm.pred == Direction)))

#lets obtain a more realistic error rate on a subset of data used as training set 
#and check it on a test dataset
train_idx <- Smarket$Year<2005
train<- Smarket[train_idx,]
Smarket.2005<-Smarket[!train_idx,]
test<-Smarket.2005
print(dim(train))
print(dim(test))

#model on a train dataset
glm.fit_tr <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume
	             ,data=train,family=binomial)
glm.probs_tst <- predict(glm.fit_tr, test, type="response")

glm.pred_tst<-rep("Down",length(glm.probs_tst))
glm.pred_tst[glm.probs_tst > .5] <- "Up"

cfu_m_tst <- table(glm.pred_tst,test$Direction)
print(cfu_m_tst)

print(paste("percengate model is correct on test data:",mean(glm.pred_tst == test$Direction)))
print(paste("training error rate on test data:", 1 - mean(glm.pred_tst == test$Direction)))


#lets refit the model using only 2 predictors with lowest  p-value

glm.fit_tr <- glm(Direction~Lag1+Lag2
	             ,data=train,family=binomial)
glm.probs_tst <- predict(glm.fit_tr, test, type="response")

glm.pred_tst<-rep("Down",length(glm.probs_tst))
glm.pred_tst[glm.probs_tst > .5] <- "Up"

cfu_m_tst <- table(glm.pred_tst,test$Direction)
print(cfu_m_tst)

print(paste("percentage, model is correct on test data:",mean(glm.pred_tst == test$Direction)))
print(paste("training error rate on test data:", 1 - mean(glm.pred_tst == test$Direction)))

#LDA
require(MASS)

lda.fit <- lda(Direction~Lag1+Lag2,data = Smarket, subset = train_idx)
print(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005) # on test data 
lda.class <- lda.pred$class 
cfu_m_lda <- table(lda.class , test$Direction)
print(cfu_m_lda)
print(paste("lda model is correct on stet data: ", mean(lda.class == test$Direction)))

##QDA
qda.fit <- qda(Direction~Lag1+Lag2,data = Smarket, subset = train_idx)
print(qda.fit)

qda.pred <- predict(qda.fit, Smarket.2005) # on test data 
qda.pred.class <- qda.pred$class 
cfu_qda_tst <- table (qda.pred.class, Smarket.2005$Direction)
print(cfu_qda_tst)
print(paste("qda model is correct on stet data: ", mean(qda.pred.class == Smarket.2005$Direction)))

#KNN
library(class)

train<- Smarket[train_idx,]
Smarket.2005<-Smarket[!train_idx,]

train.X <- cbind(train$Lag1, train$Lag2)
test.X <- cbind(Smarket.2005$Lag1, Smarket.2005$Lag2)

getKnnPrediction <- function(train, test, train_y, test_y, k=1){ 
	set.seed (1)
	knn.pred <- knn(train,test,train_y, k=k)
	cfm_knn <- table(knn.pred, test_y)
	print(cfm_knn) 
#	print(paste("knn model percentage of correct prediction for k =",k,":",
#	            (cfm_knn[1,1] + cfm_knn[2,2])/sum(cfm_knn) ))
    print(paste("knn model percentage of correct prediction for k =",k,":",
	      mean(knn.pred == test_y)))

    print(paste("knn rate of response fist value for k=",k,":",
    	  sum(cfm_knn[1,1]/sum(cfm_knn[1,]))))

    print(paste("knn rate of response second value for k=",k,":",
    	  sum(cfm_knn[2,2]/sum(cfm_knn[2,]))))

}

browser()

getKnnPrediction(train=train.X, test=test.X,
	             train_y=train$Direction, test_y=Smarket.2005$Direction,k=1)


getKnnPrediction(train=train.X, test=test.X,
	             train_y=train$Direction, test_y=Smarket.2005$Direction,k=3)


### Caravan Insurance Data
print("Caravan dataset (Insurace buyers")
print(names(Caravan))
print(dim(Caravan))
print(summary(Caravan$Purchase))

print(paste("purchase = yes prior probability:", sum(Caravan$Purchase == "Yes")/nrow(Caravan) ))

## scale all numeric predictors to have  a standardized variance
standardized.X = scale(Caravan[,-86]) # all but the factor column , all cols have var = 1

#split DS  to test (first 1000 data points ) and train the other 4822  
test_idx <- 1:1000
test.X <- standardized.X[test_idx,]
train.X <- standardized.X[-test_idx,]

test.Y <- Caravan[test_idx,]$Purchase
train.Y <- Caravan[-test_idx,]$Purchase

##run knn prediction
set.seed(1)
k <- 1
#knn.pred <- knn(train.X, test.X, train.Y, k=k)
#print(paste("erorr rate for knn model with k=",k,":",mean(test.Y != knn.pred) ))
#print( mean(test.Y != "No") )
#cfm_knn<- table(knn.pred, test.Y)
#print(cfm_knn)
getKnnPrediction(train=train.X, test=test.X,
	             train_y=train.Y, test_y=test.Y,k=1)


# getKnnPrediction(train=train.X, test=test.X,
# 	             train_y=train.Y, test_y=test.Y,k=3)

# getKnnPrediction(train=train.X, test=test.X,
# 	             train_y=train.Y, test_y=test.Y,k=5)




##for comparison fit a logistic regression on Caravan 
glm.fit <- glm(Purchase~., data=Caravan,subset=-test_idx,family=binomial)
glm.probs <- predict(glm.fit, Caravan[test_idx,], type="response")

glm.predicted<-rep("No",length(test_idx))
glm.predicted[glm.probs > .5] <- "Yes"

cfm_glm <- table(glm.predicted, test.Y)
print("glm with probability threshold of 0.5")
print(cfm_glm)  # not a single one preicted


glm.predicted_bis<-rep("No",length(test_idx))
glm.predicted_bis[glm.probs > .25] <- "Yes"

cfm_glm_bis <- table(glm.predicted_bis, test.Y)
print("glm with probability threshold of 0.25")
print(cfm_glm_bis)  # not a single one preicted
print(paste("logistic model rate of response 2nd value ",  
	sum(cfm_glm_bis[2,2])/sum(cfm_glm_bis[2,]) ))


### exercices

ppd <- 0.8
ppnd <- 0.2 
mud <- 10 
mund <- 0
x <- 4 
sigma <- 6
p <- ppd * exp(-1/(2*sigma^2)*(x-mud)^2) / 
(  ppd * exp(-1/(2*sigma^2)*(x-mud)^2) + ppnd * exp(-1/(2*sigma^2)*(x-mund)^2) )

print(paste("ex 7 proba p:",p))





