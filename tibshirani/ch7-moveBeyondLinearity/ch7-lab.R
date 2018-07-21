##ch7-lab


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch7-moveBeyondLinearity/");source("ch7-lab.R")

prhead <- function(label="lbl"){
   print("#######################################")
    print("## ")
    print(paste("##", label))
    print("## ")
   print("#######################################")
}

ch7lect <- function(){
    rm(list=ls())
    if ( length(dev.list()) > 0 ) { graphics.off() }
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
	plot.gam(lm1,se=TRUE)


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

}

ch7lab <- function(){

if ( length(dev.list()) > 0 ) { graphics.off() }

	require(ISLR)
data(Wage)

##poly regression and step fucntions
fit=lm(wage~poly(age,4),data=Wage) # only 1 poly age^4
print(coef(summary(fit)))

#poly(..,raw=TRUE) to obtain directly age+age^2+age^3+age^4 
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
print(coef(summary(fit2)))

#equivalent model using I() func
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4)
       ,data=Wage)

print(coef(summary(fit2a)))
#do the same more compactly (cbind makes a matrix)
fit2b=lm(wage~cbind(age,age^2,age^3,age^4)
	    ,data=Wage)
print(coef(summary(fit2b)))

#create a grid of values for age
agelims=range(Wage$age)
age.grid=seq(from=agelims[1],to=agelims[2])

preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit
	          ,preds$fit-2*preds$se.fit )

preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
   
##make the plot
plot(Wage$age,Wage$wage,xlim=agelims ,cex=.5
	,col="darkgrey")
title("Degree -4 Polynomial ",outer=TRUE)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#how to determine the minimum value of degrees 
#of freedom of polynomial regression. Use Anova()
   
#5 models 
fit.1 <- lm(wage~age,data=Wage) 
fit.2 <- lm(wage~poly(age,2),data=Wage)
fit.3 <- lm(wage~poly(age,3),data=Wage)
fit.4 <- lm(wage~poly(age,4),data=Wage)
fit.5 <- lm(wage~poly(age,5),data=Wage)
print(anova(fit.1,fit.2,fit.3,fit.4,fit.5))

#the output  eigher cubic or quadaratic polynome is ok


## multiliner logistic regression
# response a probability of folks who make > 250 K

fit <- glm(I(wage>250)~poly(age,4)
	      ,data=Wage,family=binomial)
preds <- predict(fit, newdata=list(age=age.grid),se=TRUE)

## for logit standard errors band
pfit <- exp(preds$fit) / (1+exp(preds$fit))
se.bands.logit <- cbind( preds$fit+2*preds$se.fit
	                    ,preds$fit-2*preds$se.fit)

se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

#we can also have  predicted the respons as probabilities with type="response"
preds <- predict(fit, newdata=list(age=age.grid)
	            ,type="response" )

##plotting 
plot(Wage$age, I(Wage$wage>250), xlim=agelims, type="n", ylim=c(0,.2))
points(jitter(Wage$age), I((Wage$wage>250)/5),cex=.5
	   ,pch="|",col =" darkgrey ")

lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue")


## step function

print(table(cut(Wage$age,6)))

fit <- lm(wage~cut(age,6), data=Wage)
print(coef(summary(fit)))

preds <- predict(fit, newdata=list(age=age.grid))

#plot
plot(age.grid, preds, lwd=2,col="blue", lty=1,ylim=c(70,130))


#### splines 

require(splines)

library(splines)
data(Wage)
age <- Wage$age 
wage <- Wage$wage 

fit <- lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred <- predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray", main="wages vs age")
lines(age.grid,pred$fit,lwd=2,col="blue")
lines(age.grid,pred$fit+2*pred$se ,lty="dashed")
lines(age.grid,pred$fit-2*pred$se ,lty="dashed")
           
## a spline with 3 knots have 7 degrees of freedom 


#define fit with a natural spline
fit2 <- lm(wage~ns(age,df=4),data=Wage)
pred2 <- predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,lwd=2,col="red")

## with smoothing spline

plot(Wage$age,Wage$wage,xlim=agelims,cex=.5,col="darkgrey")
title ("Smoothing Spline ")
fit <- smooth.spline(Wage$age,Wage$wage,df=16)
fit2 <- smooth.spline(Wage$age,Wage$wage,cv=TRUE)
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF")
		,col=c("red","blue"),lty=1,lwd=2,cex=.8)

fit2$df

### local regression

plot(Wage$age,Wage$wage,xlim=agelims 
	,cex=.5,col="darkgrey")
title ("Local Regression ")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)), col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)), col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5")
	  ,col=c("red","blue"),lty=1,lwd=2,cex=.8)


#### gam
library(gam)
#s func for smoothing spline
gam.m3<-gam(wage~s(year ,4)+s(age ,5)+education ,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")


gam.m1=gam(wage~s(age ,5)+education ,data=Wage)
gam.m2=gam(wage~year+s(age ,5)+education ,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")


}

 

###### main  ######
#prhead(label="ch7 lecture")
#ch7lect()
prhead(label="ch7 lab")
ch7lab()
