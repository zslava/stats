#rm(list=ls());  setwd("~/Dropbox/cs/bigdata/datacamp/intro_ml"); source("iml_practice.R")

#rm(list=ls());  setwd("C:/Users/zimine/Dropbox/cs/bigdata/datacamp/intro_ml"); source("iml_practice.R") 

##############################################################
#  chap. Performance
##############################################################

##read titanic dataset
# surviced is a response 
# features is pcClass (passenger glass), sex and age


titanic<-read.csv(file="titanic.csv", row.names=1,header=TRUE
				,colClasses=c("numeric", "factor", "numeric", "factor", "numeric") )

titanic$Survived <- factor(titanic$Survived, levels=c("1", "0")) #arrange factor levels

set.seed(1)
# A decision tree classification model is built on the data
library(rpart)
tree <- rpart(Survived ~ ., data = titanic, method = "class") #survived response, predictors all others
pred<-predict(tree, titanic, type="class")
conf<-table(titanic$Survived, pred) # make a confusion matrix
print(conf)

### compute accuracy, precision and recall
TP <- conf[1,1] 
FN <- conf[1,2] 
FP <- conf[2,1] 
TN <- conf[2,2] 

accuracy <- (TP + TN) / (TP+FN+FP+TN)
accuracy
precision <- TP/ (TP + FP)
precision 
recall <- TP / (TP + FN)
recall 

seeds <- read.csv(file="seeds.csv", row.names=1, header=T)

km_seeds <- kmeans(seeds, 3)
plot(length ~ compactness, data = seeds, col=km_seeds$cluster)
print(km_seeds$tot.withinss / km_seeds$betweenss)

### titanic example with  a dataset spilt between train and test sets
set.seed(1)
n <- nrow(titanic) # number of observations
shuffled <- titanic[sample(n),] #shuffle the dataset
pct_split<-0.7
train_indices <- 1:round(pct_split * n)
train <- shuffled[train_indices, ]
test_indices <- (round(pct_split * n) + 1):n
test <- shuffled[test_indices, ] # the rest of the dataset

#fill the model but on the training set
tree <- rpart(Survived ~ ., train, method = "class")
# Predict the outcome on the test set with tree:
pred<-predict(tree, test, type="class")
# Calculate the confusion matrix: conf
conf<-table(test$Survived, pred)
print(conf)
print(paste("accuracy:" , sum(diag(conf))/sum(conf) ))

##cross validation
# run 6 times with moving train set and data set
npass <- 6
accs <- rep(0,6) # a vec of accurencies
n<- nrow(shuffled)
for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (( (i-1) * round((1/npass)*n)) + 1):((i*round((1/npass) * n)))
  
  test <- shuffled[indices,] #indices are indexesof test set
  train <- shuffled[-indices,] #train is the rest
  tree <- rpart(Survived ~ ., train, method = "class")
  pred<-predict(tree, test, type="class")  
  conf<-table(test$Survived, pred)
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs
print(paste("cross validation on 6 runs " ,mean(accs)))


### check the performance of classifier on two emails data sets
emails_small <- read.csv(file="emails_small.csv", header=T, row.names=1,colClasses=c("numeric","numeric","factor") )
emails_small$spam <- factor(emails_small$spam, levels=c("1", "0")) #rearrange factor levels
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels=c("1","0")))
}

pred_small<-spam_classifier(emails_small$avg_capital_seq) #apply spam classifier
conf_small<-table(emails_small$spam, pred_small) #confusion matrix
acc_small <- sum(diag(conf_small))/sum(conf_small)
print(paste("accuracy for emails_small", acc_small)) #accuracy for emails set


### now on much bigger set
emails_full <- read.csv(file="emails_full.csv", header=T, row.names=1,colClasses=c("numeric", "numeric", "factor") )
emails_full$spam <- factor(emails_full$spam, levels=c("1", "0")) #rearrange factor levels

pred_full<-spam_classifier(emails_full$avg_capital_seq)
conf_full<-table(emails_full$spam, pred_full)
acc_full <- sum(diag(conf_full))/sum(conf_full)
print(paste("accuracy for emails_full",acc_full)) #accuracy for emails_full


#### now repeat exercise with indroducing bias to the model 
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x <= 4 ] <- 0
  return(factor(prediction, levels=c("1","0")))
}
conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
print(conf_small)
acc_small <- sum(diag(conf_small)) / sum(conf_small)
print(acc_small)

conf_full <- table(emails_full$spam, spam_classifier(emails_full$avg_capital_seq))
print(conf_full)
acc_full <- sum(diag(conf_full)) / sum(conf_full)

print(acc_full)


##############################################################
#  chap. Classification
##############################################################

set.seed(1)

titanic<-read.csv(file="titanic.csv", row.names=1,header=TRUE
				,colClasses=c("numeric", "factor", "numeric", "factor", "numeric") )

titanic$Survived <- factor(titanic$Survived, levels=c("1", "0")) #arrange factor levels
titanic$Pclass <- factor(titanic$Pclass, levels=c("1","2","3"))

idx_test <- c(555,147,140,652,290,412,84,176,156,110,179,407,2,420,391,326,385,618,222,703,573,327,427,525,677,536,606,142,486,547,697,414,467,503,22,599,470,517,225,358,70,271,613,455,537,20,670,367,94,632,299,346,671,647,124,61,601,186,129,679,231,283,52,362,544,237,36,673,395,139,487,375,312,14,433,411,510,423,308,642,315,681,86,5,614,54,688,471,587,600,69,648,707,657,135,619,259,588,261,333,546,105,509,208,108,197,302,43,616,466,328,187,711,359,701,682,674,245,492,698,273,603,320,434,393,569,200,195,257,329,73,189,26,133,221,191,534,131,522,482,653,220,235,149,691,668,3,687,46,182,543,256,463,505,548,633,172,97,175,479,672,174,306,32,18,419,167,232,621,83,236,65,164,180,63,709,397,478,11,634,27,493,530,574,297,71,591,145,549,340,661,106,365,91,9,409,443,120,211,641,476,516,440,625,7,284,230,640,59,413,360,241,269,441)

test <- titanic[idx_test,]
train <- titanic[-idx_test,]

require(rpart)
#require(rpart.plot)
#require(rattle)
#require(RColorBrewer)

tree <- rpart(Survived ~ Pclass+Sex+Age, data=train, method="class")

# Draw the decision tree (in R 2.15 no package  but it is availabe in R 3.2  in jupyter)
#fancyRpartPlot(tree)

#### k-nn 
## reuse  train and test  data sets of titanic
titanic<-read.csv(file="titanic.csv", row.names=1,header=TRUE
				,colClasses=c("numeric", "factor", "numeric", "factor", "numeric") )

titanic$Survived <- factor(titanic$Survived, levels=c("1", "0")) #arrange factor levels
idx_test <- c(555,147,140,652,290,412,84,176,156,110,179,407,2,420,391,326,385,618,222,703,573,327,427,525,677,536,606,142,486,547,697,414,467,503,22,599,470,517,225,358,70,271,613,455,537,20,670,367,94,632,299,346,671,647,124,61,601,186,129,679,231,283,52,362,544,237,36,673,395,139,487,375,312,14,433,411,510,423,308,642,315,681,86,5,614,54,688,471,587,600,69,648,707,657,135,619,259,588,261,333,546,105,509,208,108,197,302,43,616,466,328,187,711,359,701,682,674,245,492,698,273,603,320,434,393,569,200,195,257,329,73,189,26,133,221,191,534,131,522,482,653,220,235,149,691,668,3,687,46,182,543,256,463,505,548,633,172,97,175,479,672,174,306,32,18,419,167,232,621,83,236,65,164,180,63,709,397,478,11,634,27,493,530,574,297,71,591,145,549,340,661,106,365,91,9,409,443,120,211,641,476,516,440,625,7,284,230,640,59,413,360,241,269,441)

test <- titanic[idx_test,]
train <- titanic[-idx_test,]

#convert factor Sex  to a numeric variable
test$Sex <- as.numeric(test$Sex)-1
train$Sex <- as.numeric(train$Sex)-1

train_labels <- train$Survived
test_labels <- test$Survived 
knn_train <- train 
knn_test  <- test 
knn_train[,1] <- NULL # drop the Survived column 
knn_test[,1] <- NULL 

##normalized Pclass 
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class) # observe min, max from train set used for train
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)  # same min, max used for test


min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age) 
knn_test$Age <- (knn_test$Age - min_age) / (max_age - min_age) 

## now knn_train, knn_tess  are normalized
require(class)
set.seed(1)
pred <- knn(train = knn_train, test=knn_test, cl=train_labels, k=5)
conf <- table(test_labels, pred)
print(conf)

##determine the optimal k in k-nn 
set.seed(1)
range <- 1:round(0.2 * nrow(knn_train))  # 1:100
accs <- rep(0,length(range))

## interate over k from 1 to 100 
for ( k in range) {
	pred <- knn(train = knn_train, test=knn_test, cl=train_labels, k=k)
	conf <- table(test_labels,pred)  #current confusion matrix
	accs[k] <- sum(diag(conf))/sum(conf) #compute the current accuracy
}
#graphical view of accuracy
plot(range, accs, xlab="k")
#the best k is found as the one with the highest accuracy
print(paste("k with highest accurancy:" ,  which(accs == max(accs)) ))


##############################################################
#  chap. Classification
##############################################################
## kangaroo nose dataset
# measures of grey kangaroos nose widht and length
kang_nose <-read.csv(file="kang_nose.csv", row.names=1, header=T)

plot(kang_nose, xlab="nose width", ylab="nose length", main="Kangaroo noses")
lm_kang <- lm(nose_length ~ nose_width, data=kang_nose)

print(lm_kang$coefficients)

##add the regression line to the plot 
abline(lm_kang$coefficients, col="red")

nose_width_value <- c(250, 270)
predicted <- predict(lm_kang, data.frame(nose_width=nose_width_value))
print(paste( "predicted nose length:" , paste(predicted, collapse=","))) 


##compaute manually RMSE
nose_length_est <- predict(lm_kang, kang_nose) #predic all (\hat{y})
res <- kang_nose$nose_length - nose_length_est #a vector of residuals
rmse <- sqrt( 1 / nrow(kang_nose) * sum(res^2) )
print(paste("RMSE",rmse))

#compute manually R2
ss_res  <- sum(res^2) #sum of residuals
ss_tot <- sum((kang_nose$nose_length - mean(kang_nose$nose_length))^2) #total sum of squares
r_sq <- 1 - (ss_res)/ss_tot
print(paste("manually compuated R squared", r_sq))
#print(summary(lm_kang))


world_bank_train <-read.csv(file="worldbank_urb_gdp.csv", row.names=1, header=T)
plot(world_bank_train)
lm_wb <- lm(urb_pop ~ cgdp, data=world_bank_train)
abline(lm_wb$coefficients, col="red")
print(paste('afganistan' , predict(lm_wb, data.frame(cgdp=413))))

#someties there a log scaling can help
plot(urb_pop ~ log(cgdp), data = world_bank_train, 
     xlab = "log(GDP per Capita)", 
     ylab = "Percentage of urban population")

lm_wb <- lm(urb_pop ~ log(cgdp), data = world_bank_train)
abline(lm_wb$coefficients, col = "red")
print(paste('r squared after log transform',summary(lm_wb)$r.squared))

print(paste('afganistan again' , predict(lm_wb, data.frame(cgdp=413))))

#### mulivariable linear regression
shop_data <- read.csv(file='shop_data.csv', row.names=1, header =T)
par(mfrow=c(3,1))
 plot(sales ~ sq_ft, shop_data)
 plot(sales ~ size_dist, shop_data)
 plot(sales ~ inv, shop_data)
par(mfrow=c(1,1))

lm_shop <- lm(sales ~ . , data=shop_data)
print(summary(lm_shop))

### are all predictors relevant
choco_data <- read.csv(file='choco_data.csv', row.names=1, header =T)
par(mfrow=c(3,1))
plot(energy ~ protein, choco_data)
plot(energy ~ fat, choco_data)
plot(energy ~ size, choco_data)
par(mfrow=c(1,1))
lm_choco <- lm(energy ~ ., data=choco_data)
plot(lm_choco$fitted.values, lm_choco$residuals  ) #residuals, expect uncorrelated
qqnorm(lm_choco$residuals) # expect linear fit
summary(lm_choco)

#### comparinng rmse of train and test data to check if the model genealizes well
world_bank_train <- read.csv(file='world_bank_train.csv', row.names=1, header =T)
world_bank_test <- read.csv(file='world_bank_test.csv', row.names=1, header =T)

lm_wb_log <- lm(urb_pop ~ log(cgdp), data = world_bank_train) # log linear model

 
rmse_train <- sqrt(mean(lm_wb_log$residuals ^ 2)) #rmse training set

world_bank_test_truth <- world_bank_test$urb_pop
world_bank_test_input <- data.frame(cgdp = world_bank_test$cgdp)
world_bank_test_output <- predict(lm_wb_log, world_bank_test_input)
# The residuals: the difference between the ground truth and the predictions
res_test <- world_bank_test_output - world_bank_test_truth

rmse_test <- sqrt(mean(res_test^2)) # explicit formula belo
#rmse_test <- sqrt ( 1/nrow(world_bank_test) * sum(res_test^2)  )

# Print the ratio of the test RMSE over the training RMSE
print(paste("rmse rationtest/train", rmse_test/ rmse_train)) #value close to 1 => model generalizes well 


## parametric vs non-parametric
#check Rmse for linear, log linear and knn
lm_wb <- lm(urb_pop ~ cgdp, data = world_bank_train) # log linear model
lm_wb_log <- lm(urb_pop ~ log(cgdp), data = world_bank_train) # log linear model

##define the basic knn algorithm function
my_knn <- function(x_pred, x, y, k){
  m <- length(x_pred)
  predict_knn <- rep(0, m)
  for (i in 1:m) {
    
    # Calculate the absolute distance between x_pred[i] and x
    dist <- abs(x_pred[i] - x)
    
    sort_index <- order(dist)   # ordered vector index  dist[order] will be sorted 
    
    # Apply mean() to the responses of the k-nearest neighbors
    predict_knn[i] <- mean(y[sort_index[1:k]])    
    
  }
  return(predict_knn)
}

ranks <- order(world_bank_test$cgdp) # index for sorint test$cgdp

# Scatter plot of test set
plot(world_bank_test,
     xlab = "GDP per Capita", ylab = "Percentage Urban Population")

# Predict with simple linear model and add line
test_output_lm <- predict(lm_wb, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm[ranks], lwd = 2, col = "blue")
# Predict with log-linear model and add line
test_output_lm_log <- predict(lm_wb_log, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm_log[ranks], lwd = 2, col = "red")
# Predict with k-NN and add line
test_output_knn <- my_knn(world_bank_test$cgdp, world_bank_train$cgdp, world_bank_train$urb_pop, 30)
lines(world_bank_test$cgdp[ranks], test_output_knn[ranks], lwd = 2, col = "green")
#add legend
legend('topright', c('linear', 'log linear', 'knn') , 
   lty=1, col=c('blue', 'red', 'green'), bty='y', cex=.75)
#compute rmses for 3 models
rmse_linear<-sqrt(mean( (test_output_lm - world_bank_test$urb_pop) ^ 2))
print(paste("rmse linear",rmse_linear))
rmse_log_linear<-sqrt(mean( (test_output_lm_log - world_bank_test$urb_pop) ^ 2))
print(paste("rmse log linear",rmse_log_linear))
rmse_knn<-sqrt(mean( (test_output_knn - world_bank_test$urb_pop) ^ 2))
print(paste("rmse knn non parametric" ,rmse_knn))


##############################################################
#  chap. Clustering
##############################################################
set.seed(100)
##dataset with 3 types of seeds
seeds <- read.csv(file="seeds.csv", row.names=1, header=T)
##actual seeds labels loaded
seeds_type<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
seeds_km <- kmeans(seeds,  centers=3, nstart=20)
print(str(seeds_km))
print(table(seeds_type, seeds_km$cluster)) #confusion matirx
plot(length~width, data=seeds, col=seeds_km$cluster, main="seeds color-labeled with clusters")


##check the importance nstart of iterations
set.seed(100)
nstrt <-1
seeds_km_1<-kmeans(seeds,centers=5, nstart=nstrt)
seeds_km_2<-kmeans(seeds,centers=5, nstart=nstrt)
# Return the ratio of the within cluster sum of squares
print(seeds_km_1$tot.withinss / seeds_km_2$tot.withinss)
print(table(seeds_km_1$cluster,seeds_km_2$cluster ))
#observe how some cluster match and other splitted
#now rerun this code with nstrt <- 15



##make a scree plot
#school_result has  school level data recording reading and arithmetic scores for each school's 4th and 6th graders
school_result <- read.csv(file="school_result.csv", row.names=1, header=T)
nk <- 7
ratio_ss <- rep(0,nk)
##loop over different k
for (k in 1:nk) {

  school_km <- kmeans(school_result, centers=k, nstart=20)
  ratio_ss[k] <- school_km$tot.withinss / school_km$totss
}
plot(1:nk,ratio_ss, type="b", xlab="k")


##standarized vs non-standardized clustering
run_record <- read.csv(file="run_record.csv", row.names=1, header=T)
require(clValid)
#standadize data
run_record_sc<-as.data.frame(scale(run_record))
run_km<-kmeans(run_record,centers=5,nstart=20) #unscaled data
set.seed(1)
run_km_sc<-kmeans(run_record_sc,centers=5,nstart=20) #scaled data
plot(run_record$marathon, run_record$X100m, col=run_km_sc$cluster
     ,xlab="marathon", ylab="100 m" ) #original dataset used but modified clustering

table(run_km$cluster, run_km_sc$cluster)

dunn_km<-dunn(clusters=run_km$cluster, Data=run_record)
dunn_km_sc<-dunn(clusters=run_km_sc$cluster, Data=run_record_sc)
print(dunn_km)
print(dunn_km_sc)

###single hierarchical clustering
run_dist <- dist(run_record_sc)
run_single <-hclust(run_dist,method="single")
memb_single <- cutree(run_single,k=5)
plot(run_single)
rect.hclust(run_single, k=5,border=2:6)

##complete hierarchical clustering
run_dist <- dist(run_record_sc, method = "euclidean")
run_complete <-hclust(run_dist,method="complete")
memb_complete <- cutree(run_complete,k=5)
plot(run_complete)
rect.hclust(run_complete, k=5,border=2:6)


##comparing k-means, single and complete clusterings on dunn index
dunn_km <- dunn(clusters=run_km_sc$cluster, Data=run_record_sc)
dunn_single <- dunn(clusters=memb_single, Data=run_record_sc)
dunn_complete <- dunn(clusters=memb_complete, Data=run_record_sc)
print(paste("kmeans dunn",dunn_km))
print(paste("single dunn", dunn_single))
print(paste("complete dunn", dunn_complete))
##plotting data with cluster partion with highers dunn, single
plot(run_record$marathon, run_record$X100m, col=memb_single )

## clustering us states based on criminal activity
#data for us states on muder assault rrate of urpban population and rape
crime_data <- read.csv(file="crime_data.csv", row.names=1, header=T)

#scale
crime_data_sc <- scale(crime_data)
k<-4
crime_km <- kmeans(crime_data_sc, centers=k, nstart=20) #kmeans

#hierarchical clustering
dist_matrix <- dist(crime_data_sc,method="euclidean")
crime_single <- hclust(dist_matrix,method="single")
memb_single <- cutree(crime_single,k=k) #partition

dunn_km <- dunn(clusters=crime_km$cluster, Data=crime_data_sc)
dunn_single <- dunn(clusters=memb_single, Data=crime_data_sc)
print(paste("kmeans dunn",dunn_km))
print(paste("single dunn", dunn_single))

plot(crime_single)
rect.hclust(crime_single, k=k,border=2:5)

plot(assault~murder, data=crime_data, col=memb_single)


#end
