##ch10 pract


# rm(list=ls()); setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch10-Unsupervised/");source("ch10_pract.R")


##10.R.!
load("10.R.Rdata")

xx<- data.frame(rbind(x,x.test))

pr.out <- prcomp(xx, scale=TRUE)

pve <- pr.out$sdev^2/sum(pr.out$sdev^2)

#PVE for 1st 5 PC
print( sum(pve[1:5]) )

### 2
z <- as.matrix(xx)%*%pr.out$rotation
zdf <- data.frame(z,y=c(y,y.test))

lmfit <- lm(y~PC1+PC2+PC3+PC4+PC5, data=zdf)

x.test_rot <- as.matrix(x.test)%*%pr.out$rotation


pred <- predict(lmfit,newdata=data.frame(x.test_rot))

t_pc_mse <- mean( (pred-y.test)^2 ) 
print(t_pc_mse)

browser()

###  3
xx <- data.frame(x,y)

lmfit <- lm(y~.,data=xx)

pred <- predict(lmfit,newdata=x.test)

t_mse <- mean( (pred-y.test)^2 ) 

print(t_mse)
