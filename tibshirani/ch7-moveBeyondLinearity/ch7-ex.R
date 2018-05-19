#  rm(list=ls()) ; setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch7-moveBeyondLinearity/");source("ch7-ex.R")

ex6 <-function(){
## ex 6 
   print("#######################################")
    print("## ")
    print("## ex 6")
    print("## ")
    print("#######################################")
require(ISLR)
require(boot)
data(Wage)

# ### me
# agelims=range(Wage$age)
# age.grid=seq(from=agelims[1],to=agelims[2])

# k<-10
# ndf <- 7
# set.seed(1)
# folds<-sample(1:k, nrow(Wage), replace=TRUE)


# cv.emat <- matrix(NA,k,ndf,dimnames=list(NULL,paste(1:ndf)))

# for  (j in 1:k)  {  #folds
#     for ( i in 1:ndf) {  
#     	cfit<- lm(wage~poly(age,i),data=Wage[folds!=j,])
#     	cpred <- predict(cfit, newdata=list(age=Wage$wage[folds==j]),se=TRUE )
#     	cv.emat[j,i] <- mean( ( Wage$wage[folds==j] - cpred$fit )^2 )
#     }
# }

# mean.cv.errors <- apply(cv.emat, 2, mean)

# asad
all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i,raw=TRUE), data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}

plot(1:10, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

}

## ex 8
   print("#######################################")
    print("## ")
    print("## ex 8")
    print("## ")
    print("#######################################")
require(ISLR)
require(boot)
data(Auto)


#pairs(Auto, col="red", cex=0.5)

#cylynders
# polynomial regression
maxpd <-4
all.deltas = rep(NA, maxpd)
for (i in 1:maxpd) {
  glm.fit = glm(mpg~poly(cylinders, i), data=Auto)
  all.deltas[i] = cv.glm(Auto, glm.fit, K=10)$delta[2]
}
plot(1:maxpd, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2)

cylr <- range(Auto$cylinders)
cyl.grid <-seq(from=cylr[1], to=cylr[2])

cy.fit = glm(mpg~poly(cylinders, 3), data=Auto)
cy.preds <- predict(cy.fit, newdata=list(cylinders=cyl.grid),se=TRUE)
cy.se.bands=cbind(cy.preds$fit+2*cy.preds$se.fit ,cy.preds$fit-2*cy.preds$se.fit )

plot(mpg~cylinders,data=Auto)
lines(cyl.grid,cy.preds$fit,lwd=2,col="blue")
matlines(cyl.grid,cy.se.bands,lwd=1,col="blue",lty=3)

## step function (gives the same result)
# cy.fit <- glm(mpg~cut(cylinders,4), data=Auto)
# cy.preds <- predict(cy.fit, newdata=list(cylinders=cyl.grid),se=TRUE) 
# cy.se.bands=cbind(cy.preds$fit+2*cy.preds$se.fit ,cy.preds$fit-2*cy.preds$se.fit )

# plot(mpg~cylinders,data=Auto)
# lines(cyl.grid,cy.preds$fit,lwd=2,col="blue")
# matlines(cyl.grid,cy.se.bands,lwd=1,col="blue",lty=3)


##### main
#ex6()


