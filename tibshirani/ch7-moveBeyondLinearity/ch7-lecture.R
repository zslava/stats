##ch7-lab


# rm(list=ls());setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch7-moveBeyondLinearity/");source("ch7-lecture.R")


require(ISLR)

data(Wage)
attach(Wage)
##polynomials
fit <- lm(wage~poly(age,4),data=Wage) #poly degree 4 on 1 predictor 
print(summary(fit))

##plot of function
#```{r fig.width=7, fig.height=6}
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit,newdata=list(age=age.grid), se=TRUE  )
se.bands <- cbind(preds$fit+2*preds$se  , preds$fit-2*preds$se )
plot(age,wage,col="darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands,col="blue", lty=2)


## with I()
fita <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
print(summary(fita))

plot(fitted(fit), fitted(fita))

##compare a sequence of nested models with anova
fita <- lm(wage~education,data=Wage)
fitb <- lm(wage~education+age,data=Wage)
fitc <- lm(wage~education+poly(age,2), data=Wage)
fitd <- lm(wage~education+poly(age,3), data=Wage)

print(anova(fita,fitb,fitc,fitd))

##logisitc regression
## applied to high earners

lgfit <- glm(I(wage>250) ~ poly(age,3), data=Wage, family=binomial)
print(summary(fit))

preds <- predict(lgfit, list(age=age.grid), se=TRUE)
se.bands <- preds$fit + cbind(fit=0, lower=-2*preds$se, upper=2*preds$se)

print(se.bands[1:5,] )


prob.bands <- exp(se.bands) / (1+exp(se.bands))
matplot(age.grid, prob.bands,col="blue", lwd=c(2,1,1), lty=c(1,2,2),
        type="l", ylim=c(0,.1)  )
points(jitter(age), I(wage>250)/10, pch="l", cex=.5)

##### Splines

require(splines)
fit<-lm(wage~bs(age,knots=c(25,40,60)), data=Wage )

#print(summary(fit))
plot(age,wage, col="darkgrey")
lines(age.grid, predict(fit, list(age=age.grid)),col="darkgreen",lwd=2 )
abline(v=c(25,50,60), lty=2, col="darkgreen" )

fit <- smooth.spline(age,wage, df=16) # knots an be undefined
lines(fit,col="red", lwd=2)

fit<- smooth.spline(age,wage, cv=TRUE)
lines(fit, col="purple", lwd=2)


#### GAM

require(gam)

gam1<- gam(wage~s(age,df=4)+s(year, df=4)+education, data=Wage)

# s()  smoothing spline for variable in gam package
par( mfrow=c(1,3))
plot(gam1,se=TRUE)

#for logistic regression
gam2 <- gam(I(wage>250)~s(age,df=4)+s(year, df=4)+education, data=Wage, family=binomial )
plot(gam2)


## using gam to plot models by lm() or glm()
par(mfrow=c(1,3))
lm1 <- lm(wage~ns(age,df=4)+ns(year,df=4)+education
          ,data=Wage )
plot.Gam(lm1,se=TRUE)


print("***** quiz *****")
load(file='7.R.RData')
par(mfrow=c(1,1))
plot(x,y)
print(summary(lm(y~x)))

fit1 <- lm(y~1+x+I(x^2))
fit2 <- lm(y~1+poly(x,2,raw=TRUE) )
lines(x,predict(fit1), col="red", lwd=2)
x.grid <- sort(x)
preds <- predict(fit2, newdata=list(x=x.grid), se=TRUE)
lines(x.grid, preds$fit, col="green", lwd=2)
