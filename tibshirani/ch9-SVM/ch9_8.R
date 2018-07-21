##ch9-8


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch9-SVM/");source("ch9_8.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}

pprint("ex 8")

require(ISLR)
require(e1071)
#orange Juice dataset. customer purchases either Citrus Hill or Minute Maid Orange Juice
data(OJ)
print(dim(OJ))

##a)
set.seed(9004)
n <- dim(OJ)[1]
nf <- dim(OJ)[2] -1
train_size <- 800 
train <- sample(n, train_size)
test <- (1:n)[-train]


##b)
print("linear kernel svm classifier")
cst <- 0.01
svmfit <- svm(Purchase ~ . , data=OJ[train,], kernel="linear", cost=cst)
print( summary(svmfit) )


##c)

printModelError <- function(model, data, ydata, lbl){
    yhat <- predict(model, data)
    cm <- table(predict=yhat, truth=ydata)
    print(cm)
    print(paste(lbl, 1 - sum(diag(cm))/sum(cm) ))
}

printModelError(svmfit, OJ[train,], OJ[train,]$Purchase, "svm  classifier  cost=0.01  train error")


printModelError(svmfit, OJ[test,], OJ[test,]$Purchase, "svm  classifier  cost=0.01  test error")



##d)
set.seed(1554)
costs <- c(0.01, 0.1, 1,  5, 10, 100)

tune.out <- tune(svm, Purchase ~. , data=OJ[train,], kernel="linear", 
               ranges=list(cost=costs))
print(summary(tune.out))


##e)
bestmod = tune.out$best.model
print( summary(bestmod) )

printModelError(bestmod, OJ[train,], OJ[train,]$Purchase, "svm  classifier  with best cost  train error")


printModelError(bestmod, OJ[test,], OJ[test,]$Purchase, "svm  classifier  with best cost test error")



##f
print("radial kernel svm ")
set.seed(410)
cst <- 0.01
svmfit <- svm(Purchase ~ . , data=OJ[train,], kernel="radial", cost=cst)
print( summary(svmfit) )

printModelError(svmfit, OJ[train,], OJ[train,]$Purchase, "svm  radial  with cost=0.01 train error")

printModelError(svmfit, OJ[test,], OJ[test,]$Purchase, "svm  radial with cost=0.01  test error")



set.seed(755)
print("Tuning for radial kernel")
tune.out <- tune(svm, Purchase ~. , data=OJ[train,], kernel="radial", 
               ranges=list(cost=costs))
print(summary(tune.out))

bestmod = tune.out$best.model
print( summary(bestmod) )

printModelError(bestmod, OJ[train,], OJ[train,]$Purchase, "svm  radial  with best cost train error")


printModelError(bestmod, OJ[test,], OJ[test,]$Purchase, "svm  radial with best cost  test error")




###g)
print("polynomial kernel svm ")
set.seed(8112)
cst <- 0.01
deg <- 2
svmfit <- svm(Purchase ~ . , data=OJ[train,], kernel="polynomial", degrees=deg, cost=cst)
print( summary(svmfit) )

printModelError(svmfit, OJ[train,], OJ[train,]$Purchase, "svm  polynomial  with cost=0.01 train error")

printModelError(svmfit, OJ[test,], OJ[test,]$Purchase, "svm  polynomial with cost=0.01  test error")


set.seed(322)
print("Tuning for polynomial kernel")
tune.out <- tune(svm, Purchase ~. , data=OJ[train,], kernel="polynomial", 
               ranges=list(cost=costs), degrees=deg)
print(summary(tune.out))

bestmod = tune.out$best.model
print( summary(bestmod) )

printModelError(bestmod, OJ[train,], OJ[train,]$Purchase, "svm  polynomial  with best cost train error")

printModelError(bestmod, OJ[test,], OJ[test,]$Purchase, "svm  polynomial with best cost  test error")



#h   svm classifier seems to give a very small advantage over radial svm or polynomial svm
