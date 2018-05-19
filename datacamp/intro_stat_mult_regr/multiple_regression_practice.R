#rm(list=ls());  setwd('~/Dropbox/cs/bigdata/datacamp/intro_stat_mult_regr'); source('multiple_regression_practice.R')

#rm(list=ls());  setwd('C:/Users/zimine/Dropbox/cs/bigdata/datacamp/intro_stat_mult_regr'); source('multiple_regression_practice.R') 

# ### a dataset with professors salaries
# dependent variable: salary
# independent variables (features) :
#  * age - professor's age
#  * years - number of years of active career after a phd
#  * pubs - number of publications through a career
#  * dept - categorical with 3 levels 'H', 'P', 'S'


##create a dataset with  yearly wages of professors


# salary<- c(60072,61017,61618,61976,66398,67083,69314,71653,72519,74821,79725,80882,83525,84023,89254,89387,91309,91413,94553,95561,96024,98398,98822,101076,101330,101981,104699,106374,108571,109671,110178,110936,111600,112355,113904,115150,116494,116790,117913,118314,119329,121024,121878,122277,124491,126868,128285,131222,132000,132505,133593,134198,135777,135837,138672,138882,141535,142191,143980,154545,155845,155936,156294,157228,157268,158139,158199,159075,160924,163279,164402,165398,165695,168116,170020,171435,172515,172576,173199,175335,176230,177797,177813,179080,179944,180942,181368,181427,182206,184407,185797,185800,186096,187338,189447,189507,190120,191868,197288,199606)
# age <-c(38,39,38,31,32,39,32,31,66,66,66,66,66,32,34,32,35,38,32,36,37,55,55,55,55,48,43,43,48,42,49,49,46,47,47,45,48,46,43,49,44,40,47,45,44,45,41,55,55,45,45,52,58,59,53,57,59,58,54,59,53,59,57,57,55,55,58,56,51,44,44,44,44,44,44,44,55,61,66,64,62,60,63,66,34,66,44,66,60,64,63,44,67,60,63,54,44,61,61,62)
# years<-c(16,14,18,15,30,25,24,29,32,26,29,30,31,5,8,6,7,12,10,11,5,6,13,6,5,19,17,15,21,16,21,23,17,20,19,19,22,18,17,22,17,13,18,18,18,16,14,18,15,30,25,24,29,32,26,29,30,31,28,31,24,33,31,29,29,28,29,27,22,22,22,22,22,22,22,22,32,37,36,40,40,32,34,39,39,32,22,40,32,37,36,22,40,32,34,39,39,32,41,40)
# pubs <-c(22,23,23,14,32,27,29,37,61,69,58,53,46,67,25,39,35,37,17,23,41,21,17,34,32,66,66,66,66,25,67,70,55,62,61,60,67,59,52,69,56,42,56,58,57,52,47,57,50,91,80,77,90,66,83,92,94,98,87,96,75,66,98,89,90,88,92,86,69,77,80,67,67,66,66,44,122,125,122,44,112,110,122,44,98,104,44,44,44,122,44,44,110,122,122,98,104,101,125,124)
# dept <-c('P','P','P','P','P','P','P','S','S','S','S','S','S','S','S','H','H','H','H','H','H','H','P','P','P','P','P','P','P','S','S','S','S','S','S','S','S','H','H','H','H','H','H','H','P','P','P','P','P','P','P','S','S','S','S','S','S','S','S','H','H','H','H','H','H','H','P','P','P','P','P','P','P','S','S','S','S','S','S','S','S','H','H','H','H','H','H','H','P','P','P','P','P','P','P','S','S','S','S','S') 
# fs<-data.frame(salary=salary, age=age,years=years,pubs=pubs,dept=factor(dept))
# write.csv(fs,file='salary.csv', row.names=FALSE)
fs<-read.csv(file='salary.csv',header=TRUE)
# to see an 'average' professor
print(summary(fs))
#lets see correlations ( we are interested to predict salary)
print(cor(fs[1:4]))

model_years <- lm(fs$salary ~ fs$years)
model_pubs <- lm(fs$salary ~ fs$pubs)

# Plot both enhanced scatter plots in one plot matrix of 1 by 2
par(mfrow = c(1, 2))
plot(fs$salary~fs$years, main ="plot_years", xlab = "years", ylab = "salary")
abline(model_years,col="red")
plot(fs$salary~fs$pubs, main = "plot_pubs", xlab = "pubs", ylab = "salary")
abline(model_pubs, col="blue")

##compare a model with 1 predictor and 2 predictor variables
model_1 <- model_years
model_2 <- lm(fs$salary ~ fs$years + fs$pubs)

print(summary(model_1))
print(summary(model_2))

r_squared <- c(summary(model_1)$r.squared, summary(model_2)$r.squared )
print(paste('how r squared evolves: ',paste(r_squared, collapse= ' ')))

#adding more predictors might not be useful
model_3 <- lm(fs$salary ~ fs$years + fs$pubs + fs$age)
print(summary(model_3))
#observe large p-value near age predictor
#and the r-squared is only marginally increased
#-> coefficient for age tends to be insignificant


#### the most significant predictor is a predictor with the biggest z-scaled coefficient
z_model_2<-lm(scale(fs$salary) ~ scale(fs$years) + scale(fs$pubs) )
summary(z_model_2) # it is fs$pus

### chap 2 review of matrix  muliplication
r <- matrix(1:24,nrow=3)
s <- matrix(21:44,nrow=3)
t <-  t(r) 
r+s 
t(r) %*% s # matrix multiplcation

#matrix of deviations (mean - score) mutliped by its transpose gives
#a matrix of summed squares (diagonal) and cross products 
#divide it by N an you obtain a variance covariance matrix
#multiple this by standard deviation matrix and you get a correlation matrix

#example matrix input
col1<-c(3,3,2,4,4,5,2,3,5)
col2<-c(2,2,4,3,4,4,5,3,3,5)
col3<-c(3,3,4,4,3,3,4,2,4,4)
X <- cbind(col1,col2,col3)

#construct a vector of column sums 
#identity matrix
I<-matrix(rep(1,10))
#sums
t_mat<-t(I) %*% X #transpose of Id  * X 
n <- 10 # 10 obesrvations ( n . rows)
# the row of means 
M <- t_mat * n^-1
#10x1 column Id matrix
J<-matrix(1,10,1)
# a matrix of mean 
MM <- J %*% M  
# a matrix of deviations (each element subsraction)
D <- X - MM 
# matrix of sum of squares and sum of cross products 
S <- t(D) %*% D 
#variance covariance matrix 
C <-  S * n^-1 
#standard deviation diagonal matrix (sd on diagonal, zeros off diagonal)
SD <- diag( diag(C)^0.5,  nrow=3,ncol=3)
#an inverse of SD 
solve(SD)
#correlation matrix 
R <- solve(SD) %*% C %*% solve(SD)

## chap 3 dummy coding

#it is a system to code categorial  predictors

#in fs dataset a categorical variable is fs$dept
print(describeBy(fs, fs$dept))

library(psych)
# Create the dummy variables
dept_code <- dummy.code(fs$dept)

# Merge the dataset in an extended dataframe
extended_fs <- cbind(dept_code, fs)

# Provide summary statistics
print(summary(extended_fs))

### from now will be using C() contrast function to create dummy variables
# Create dummies for the categorical variable fs$dept by using the C() function
dept_code <- C(fs$dept, treatment)

model <- lm(fs$salary ~ fs$years + fs$pubs)
# Regress salary against years, publications and department 
model_dummy <- lm(fs$salary ~ fs$years + fs$pubs + dept_code)
print(summary(model_dummy))

# Compare model 4 with model3
anova(model,model_dummy) # P-value is significant (small)  sum of residuals smaller

###unweighted effects coding
dept.f <- factor(fs$dept)

# Assign the 3 levels generated in step 2 to dept.f
contrasts(dept.f) <-contr.sum(3)

# Regress salary against dept.f
model_unweighted <- lm(fs$salary ~ dept.f)

# Apply the summary() function
summary(model_unweighted)

#small commit in master

#end+