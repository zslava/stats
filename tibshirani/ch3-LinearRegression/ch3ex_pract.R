#rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch3-LinearRegression/");source("ch3ex_pract.R")

#rm(list=ls());setwd("c:/users/zimine/Dropbox/cs/bigdata/lagunita/tibshirani/ch3-LinearRegression/");source("ch3ex_pract.R")



#ch3 ex applied helper code

#ex 8
require(ISLR)
data(Auto)
str(Auto)
#8.a
lmfit1<-lm(mpg~horsepower,data=Auto)
print(summary(lmfit1))
plot(mpg~horsepower,data=Auto, cex=0.3)
abline(lmfit1, col=2)
#i. we have a low p-value coef for horsepower variable  i.e. a relation between response and predictor exists
#ii.  the rse = 4.906 the mean value of responnse is 23.45. so  we have a percentage error 4.906/23.45 = 20 %
# R^2 = 0.6  So the model does not fit the data very closely

#ex9
Auto<-read.csv("../data/Auto.csv", header=T, na.strings="?")
summary(Auto$horsepower)


cor(Auto[,1:8], use="complete.obs")

lmfit3<-lm(mpg~.-name+cylinders:displacement, data=Auto)

#9.e
# cylinders:displacement
# cylincders:horsepower
# cylinders:year

##--cylinders:weight
##--cylinders:origin 


##-displacement:horsepower
##- displacement:weight

#9.f

##displacement helps  log(x) and x^2 term 

#horsepower :  log   sqrt  

#weight : log , x^2

 #ex 10 

require(ISLR)
data(Carseats)
# str(Carseats)

## pairs on Sales, Price, Urban, US
pairs(Carseats[,c(1,6,10,11)], cex=0.1)
lmfit101<-lm(Sales~Price+Urban+US, data=Carseats)
print( summary(lmfit101) )

#ex 11

set.seed(1)
x=rnorm(100)
y<-2*x+rnorm(100)

lmfit111<-lm(y~x-1)
summary(lmfit111)


##ex12 
set.seed(1)
x=rnorm(100)
y<-2*x+runif(100)
print(summary(lm(y~x-1)))
print(summary(lm(x~y-1)))



#ex13
set.seed(1)
x<-rnorm(100, mean=0, sd=1) #default values
#(b)
eps<-rnorm(100, mean=0, sd=0.25) #residuals with smaller variance
#c)
y<- -1+0.5*x + eps
#(d)
plot(y~x)

#e)
lmfit131<-lm(y~x)
print(summary(lmfit131))
#(f)
abline(lmfit131, col=2)
abline(-1, 0.5, col=3)
legend(-1, legend=c("model fit", "pop. regression"),col=2:3,lwd=3 )
#(g)
lmfit132<-lm(y~x+I(x^2))
print(summary(lmfit132)) #lower  F-statistic

plot(y~x)
abline(lmfit131, col=2)
points(x,predict(lmfit132, data.frame(x)) ,col=4, cex=0.2)
legend(-1, legend=c("linear model fit", "quadratic model fit"),col=c(2,4),lwd=3 )


##ex 14

set.seed(1)
x1<-runif(100)
x2<-0.5*x1 + rnorm(100)/10
y<-2+2*x1 +0.3*x2+rnorm(100)

#14.c
lmfit141<-lm(y~x1+x2)
print(summary(lmfit141))

# 14.d
lmfit142<-lm(y~x1)
#print(summary(lmfit142))
## ans: due to low p-value for x1 coef we can reject null hypo H_0 : \beta_1 = 0

# 14.e
lmfit143<-lm(y~x2)
#print(summary(lmfit143))
## ans: due to low p-value for x1 coef we can reject null hypo H_0 : \beta_2 = 0

# 14.g
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8) #this is an outlier seen on the plot
y  <- c(y, 6) 

plot(x1,x2)

#y~x1+x1
dev.off() 

par(mfrow=c(3,2))
plot(lmfit141, cex=0.3)

plot(predict(lmfit141), rstudent(lmfit141)) # for outliers  (rstudent studentized resitulas shoud stay in [-3;3] )

lmfit141<-lm(y~x1+x2) #redowing
print(summary(lmfit141))

dev.new()

par(mfrow=c(3,2))
plot(lmfit141, cex=0.3)
plot(predict(lmfit141), rstudent(lmfit141))

# ans. model (c) slighly larger F-statistic and slightly larger R^2
#  2. according to studentized residual chart the new point is not  and outlier  
#  and it is a highly leverated point as its leverage statistics greatly exceeds  vlaue (p+1)/n = 3/101 = 0.03

#y~x1
dev.off()

print(summary(lmfit142))

par(mfrow=c(3,2))
plot(lmfit142, cex=0.3)
plot(predict(lmfit142), rstudent(lmfit142), main ="studentized residuals before")

lmfit142<-lm(y~x1) #remodel after adding a point

dev.new()
print(summary(lmfit142))
par(mfrow=c(3,2))
plot(lmfit142, cex=0.3 )
plot(predict(lmfit142), rstudent(lmfit142),main ="studentized residuals after")

#ans:  in model (d) y~x1 the new point is neither an outlier nor a highly leveraged point  

#y~x2
dev.off()

print(summary(lmfit143))

par(mfrow=c(3,2))
plot(lmfit143, cex=0.3)
plot(predict(lmfit143), rstudent(lmfit143), main ="studentized residuals before")

lmfit143<-lm(y~x2) #remodel after adding a point

dev.new()

print(summary(lmfit143))
par(mfrow=c(3,2))
plot(lmfit143, cex=0.3 )
plot(predict(lmfit143), rstudent(lmfit143),main ="studentized residuals after")

#ans:  in model (e) y~x2 the new point is not an outlier but is a highly leveraged point  

dev.off()
dev.off() 
#############
#ex 15
###############
library(MASS)
print(?Boston)
str(Boston)

#pairs plot for investigation
#pairs(Boston, cex=0.1, col=2)
#15.a
# 'zn' proportion of residential land zoned for lots over 25,000 sq.ft.
lm_zn<-lm(crim~zn,data=Boston)
print(summary(lm_zn)) #### p-value low. linear model statistically significant but rsq very low 3.8% 

# 'indus' proportion of non-retail business acres per town.
lm_indus<-lm(crim~indus,data=Boston)
print(summary(lm_indus))   ### p-value low, lm significant, rsq = 0.163 

#'chas' Charles River dummy variable 
lm_chas<-lm(crim~chas,data=Boston)
print(summary(lm_chas))  ##### p-value high, lm non sig

#'nox' nitrogen oxides concentration
lm_nox<-lm(crim~nox,data=Boston)
print(summary(lm_nox)) ##### p-value low, lm sig, rsq = 0.17

#'rm' average number of rooms per dwelling.
lm_rm<-lm(crim~rm,data=Boston)
print(summary(lm_rm)) # p-val ok lm sig, rsq=0.046

#'age' proportion of owner-occupied units built prior to 1940.
lm_age<-lm(crim~age,data=Boston)
print(summary(lm_age))  ##### p-value ok, lm sig, rsq=0.12

#     'dis' weighted mean of distances to five Boston employmet centres.
lm_dis<-lm(crim~dis,data=Boston)
print(summary(lm_dis)) ###  p-val ok, lm sig, rsq=0.14

#'rad' index of accessibility to radial highways.
lm_rad<-lm(crim~rad,data=Boston)
print(summary(lm_rad)) #### p-val ok, lm sig  rsq = 0.39 !!!! 

#'tax' full-value property-tax rate per \$10,000.
lm_tax<-lm(crim~tax,data=Boston)
print(summary(lm_tax)) ### p-val ok, lm sig , rsq 0.338 !!!

#'ptratio' pupil-teacher ratio by town.
lm_ptratio<-lm(crim~ptratio,data=Boston)
print(summary(lm_ptratio)) ### p-val ok, lm sig  rsq = 0.08

#'black' 1000(Bk - 0.63)^2 where Bk is the proportion of blacks bytown.
lm_black<-lm(crim~black,data=Boston)
print(summary(lm_black))  ### p-val ok, lm sig, rsq = 0.14 !!

#'lstat' lower status of the population (percent).
lm_lstat<-lm(crim~lstat,data=Boston)
print(summary(lm_lstat))  ### p-val ok, lm sig, rsq = 0.2 !!!

#'medv' median value of owner-occupied homes in \$1000s.
lm_medv<-lm(crim~medv,data=Boston)
print(summary(lm_medv)) #### p-val ok, lm sig,  rsq=0.149 !!!

######### 
# 15.(a) in summary liner models are statistically significant with all but one predictor (chas
# predictors producing the highest percengages of the response variance explained are:
# (rad, tax,black,lstat,medv)
####
### plot of predictors with highest relation

par(mfrow=c(3,2))
plot(crim~rad, data=Boston, cex=0.2, col=1)
abline(lm_rad, col=1)

plot(crim~tax, data=Boston, cex=0.2, col=2)
abline(lm_tax, col=2)

plot(crim~black, data=Boston, cex=0.2, col=3)
abline(lm_black, col=3)

plot(crim~lstat, data=Boston, cex=0.2, col=4)
abline(lm_lstat, col=4)

plot(crim~medv, data=Boston, cex=0.2, col=5)
abline(lm_medv, col=5)

#15.b
## linear model using all predictors
lm_15b<-lm(crim~., data=Boston)
print(summary(lm_15b))
#lm sig,  rsq = 0.43 
# we can reject null hypothesis for predictors: {zn,dis,rad,black,medv}

### my own test to check influence of predictors having a p-value near a threshold of 0.05
# lm_15a<-lm(crim~zn+dis+rad+black+medv,data=Boston)
# print(summary(lm_15a))
# lm_15b<-lm(crim~zn+dis+rad+black+medv+nox+lstat,data=Boston)
# print(summary(lm_15b))

##15.c
# linear model on all predictors in (b) offers a higher R squared commpared to single predictor linear models in (a)

#construct y vec
coef_y <- lm_15b$coef[-1]
#constrct x vec 
coef_x <- c(lm_zn$coef[-1], lm_indus$coef[-1], lm_chas$coef[-1], lm_nox$coef[-1], lm_rm$coef[-1],
            lm_age$coef[-1],  lm_dis$coef[-1], lm_rad$coef[-1],  lm_tax$coef[-1], lm_ptratio$coef[-1],
            lm_black$coef[-1], lm_lstat$coef[-1], lm_medv$coef[-1])

par(mfrow=c(1,1))
plot(coef_x, coef_y, main = "coefficients")

### 15.d

lm_nox_q <- lm(crim~nox+I(nox^2)+I(nox^3),data=Boston)
print(summary(lm_nox_q)) ## all t3 degrees of predictors have low p-values

#zz<-42
#eval(as.name("zz"))

print("###########")
print( paste( names(Boston)[-1], collapse=" ") )
for (p in names(Boston)[-1]) {
  print(p)
  #pval <- eval(as.name(p))
  lm_p_q <- lm(crim~eval(as.name(p))+I(eval(as.name(p))^2) + I(eval(as.name(p))^3), data=Boston  )
  print(summary(lm_p_q))
}	

## zn - lin,  indus - non-lin,  chas - lin, nox  non-lin  , rm lin low r2, age - non-lin, dis - non-lin , 
# rad  high p-val, tax, high p-val, ptratio - non-lin , black high p-val, lstat high p-val, medv non -lin



### end
#dev.off()

