##ch7-applied


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch7-moveBeyondLinearity/");source("ch7-applied.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}

pprint("exercise 9")

require(MASS)
attach(Boston)

#a)
fit.1 <- lm(nox~poly(dis,3), data=Boston)
dis.grid <- seq(range(dis)[1], range(dis)[2], by=0.05)
preds <- predict(fit.1, newdata=list(dis=dis.grid), se=T )
plot(nox~dis,data=Boston,cex=0.6,col="gray")
lines(dis.grid, preds$fit, lwd=2, col="red")

#b)
rss<-rep(NA,10)
fits <- list()
preds <- list()
plot(nox~dis,data=Boston,cex=0.6,col="gray")
for (d in 1:10){
  fits[[d]] <- lm(nox~poly(dis,d), data=Boston)
  rss[d] <- deviance(fits[[d]])
  preds[[d]] <- predict(fits[[d]], newdata=list(dis=dis.grid))
  lines(dis.grid, preds[[d]], lwd=1, col=d)
}
print(rss)

#c)
require(boot)

set.seed(1)
cv.errs <- rep(NA,10)
for (d in 1:10){
  fit <- glm(nox~poly(dis,d), data=Boston)
  cv.errs[d] <- cv.glm(Boston, fit, K=10)$delta[2]
}
print( which.min(cv.errs) )
plot(cv.errs, type="l")

#d)
require(splines)
fit <- lm(nox~bs(dis,df=4, knots=c(3,6,8)), data=Boston)
pred <- predict(fit, newdata=list(dis=dis.grid), se=T )
plot(nox~dis,data=Boston,cex=0.6,col="gray")
lines(dis.grid,pred$fit,lwd=2, col="blue")
lines(dis.grid,pred$fit+2*pred$se ,lty="dashed")
lines(dis.grid,pred$fit-2*pred$se ,lty="dashed")


#e)
cv.errs <- rep(NA,14)
rss <- rep(NA,14)
for (df in 3:14){
  fit <- glm(nox~bs(dis,df=df), data=Boston)
  cv.errs[df] <- cv.glm(Boston, fit, K=10)$delta[2]
  rss[df] <- sum(fit$residuals^2)
}
print( rss[-c(1,2)] )

#d)
cv.errs <- rep(NA,14)
for (df in 3:14){
  fit <- glm(nox~bs(dis,df=df), data=Boston)
  cv.errs[df] <- cv.glm(Boston, fit, K=10)$delta[2]
}
print( which.min(cv.errs) )

#####################
pprint("exercise 10")
dev.off()
options(warn=-1)
attach(College)
#a)  split into training and test
require(leaps)
set.seed(1)
nsam <- nrow(College)
train <- sample(1:nsam, size=floor(2/3*nsam) )
test <- (1:nsam)[-train]
p_feats <- dim(College)[2]-1
regfit.fwd <- regsubsets(Outstate~., data=College[train,], nvmax=p_feats, method="forward")
reg.summary <- summary(regfit.fwd)

par(mfrow=c(2,2))
##rss
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
type="l", main="Best subset select")

#adj r2
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
ylab="Adjusted RSq",type="l", main="Best subset select")

idx_r2<- which.max(reg.summary$adjr2)
points(idx_r2, reg.summary$adjr2[idx_r2], col="red", xex=2,pch=20)
print( paste("R-squared is at max with  number of features:",idx_r2))
max.adjr2 <- reg.summary$adjr2[idx_r2]
std.ajr2 <- sd(reg.summary$adjr2)
abline(h=max.adjr2 - 0.2*std.ajr2, col="red", lty=2)

#cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp",type="l", main="subset select")
cp_idx <- which.min(reg.summary$cp)
min.cp <- reg.summary$cp[cp_idx]
std.cp <- sd(reg.summary$cp)
points(cp_idx,reg.summary$cp[cp_idx],col="red", cex=2, pch=20)
abline(h=min.cp + 0.2*std.cp, col="red", lty=2)

#bic
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC",type="l", main="subset select")
b_idx <- which.min(reg.summary$bic)
points(b_idx,reg.summary$bic[b_idx],col="red", cex=2, pch=20)
std.bic <- sd(reg.summary$bic)
abline(h=reg.summary$bic[b_idx] + 0.2*std.bic, col="red", lty=2)

# based on chart the best model is of size 6, lets see variables of that model
reg.fit <- regfit.fwd <- regsubsets(Outstate~., data=College, method="forward")
coefi <- coef(reg.fit, id=6)
print( names(coefi) )


#b)
library(gam)
gam.fit <- gam(Outstate~Private+s(Room.Board,3)+s(PhD,3)+s(perc.alumni,3)
                    +s(Expend,3)+s(Grad.Rate,3), data=College[train,])

par(mfrow=c(2,3))
plot(gam.fit,se=TRUE, col="blue")

#c) evaluate the model on test 
gam.pred <- predict(gam.fit, College[test,] )
gam.rss <- mean((College[test,]$Outstate - gam.pred)^2)
gam.tss <- mean( (College[test,]$Outstate - mean(College[test,]$Outstate))^2)
gam.test_r2 <- 1 - gam.rss/gam.tss 
print(gam.test_r2)

#lets compare it with OLS fit 
ols.fit <- lm(Outstate~Private+Room.Board+PhD+perc.alumni
                    +Expend+Grad.Rate, data=College[train,] )
ols.pred <- predict(ols.fit, College[test,])
ols.rss<- mean((College[test,]$Outstate - ols.pred)^2)
tss <- mean( (College[test,]$Outstate - mean(College[test,]$Outstate))^2)
ols.test_r2 <- 1 - ols.rss/gam.tss 
print(ols.test_r2)


##############
pprint("exercise 11")
dev.off()
#a), b)
ns<-1000
x1<-rnorm(ns)
x2<-rnorm(ns)
eps<-rnorm(ns)

#generate response
y<- -2.1 +1.3*x1 +0.54*x2 + eps

beta1<-2.0
#c)
set.seed(1)
a1<-y-beta1*x1
fit_a1 <- lm(a1~x2)
beta2_est <- fit_a1$coef[2]
bet0_est <-  fit_a1$coef[1]
a2<- y-beta2_est*x2
fit_a2 <- lm(a2~x1)
beta1_est <-fit_a2 $coef[2]

nsim <- 100
beta1_v <- rep(NA,nsim)
beta2_v <- rep(NA,nsim)
beta0_v <-rep(NA,nsim)

beta1<-2.0 #initial value 
for (i in 1:nsim){
    a<- y-beta1*x1
    cfit <- lm(a~x2)
    beta2_v[i] <- cfit$coef[2]
    beta0_v[i] <- cfit$coef[1]
    a <- y - beta2_v[i] * x2
    beta1_v[i] <- lm(a~x1)$coef[2]
    beta1 <- beta1_v[i] #assign beta1 for next iteration
}

plot(beta0_v, type="l", ylim=c(-3,2), col=1)
lines(beta1_v, type="l", col=2)
lines(beta2_v, type="l", col=3)
legend("topright", c("beta0", "beta1", "beta2"), lty=1, col=c(1,2,3) )
#f)
fit_m <- lm(y~x1+x2)
print(fit_m$coef)

abline(h=fit_m$coef[1], col=1, lty=2)
abline(h=fit_m$coef[2], col=2, lty=2)
abline(h=fit_m$coef[3], col=3, lty=2)
#legend("topright", c("beta0", "beta1", "beta2", "multiple regression"), lty=1, col=c(1,2,3,1) )
