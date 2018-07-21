##ch9-7


# setwd("~/Dropbox/cs/bigdata/lagunita/tibshirani/ch9-SVM/");source("ch9_7.R")


require(e1071)
data(Auto)

###a

mpgh <- ifelse(Auto$mpg > median(Auto$mpg), 1,0)
Auto <- data.frame(Auto, mpgh=as.factor(mpgh))

##b
set.seed(3255) 
costs <- c(0.01, 0.1, 1,  5, 10, 100)
tune.out <- tune(svm, mpgh ~ .  , data=Auto, kernel="linear", 
               ranges=list(cost=costs))
print(summary(tune.out))

bestmod_lin = tune.out$best.model
print(summary(bestmod_lin))

##c
print("tuning radial")
gammas <- c(0.5,1,2,3,4)
set.seed(21)
tune.out <- tune(svm, mpgh ~ .  , data=Auto, kernel="radial", 
               ranges=list(cost=costs, gamma=gammas))
print(summary(tune.out))

print("best model for radial")
bestmod_rad = tune.out$best.model
print( summary(bestmod_rad) )

print("tuning polynomial")
gammas <- c(0.5,1,2,3,4)
degs <- c(2,3)
set.seed(463)
tune.out <- tune(svm, mpgh ~ . , data=Auto, kernel="polynomial", 
               ranges=list(cost=costs, gamma=gammas, degrees=degs))
print(summary(tune.out))

print("best model for linear")
bestmod_poly = tune.out$best.model
print( summary(bestmod_poly) )


#d 
plotpairs <- function(fit) {
    for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpgh", "name"))]) {

        plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
    }
}
plotpairs(bestmod_lin)



