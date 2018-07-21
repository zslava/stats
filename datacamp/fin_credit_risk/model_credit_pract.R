#setwd("~/Dropbox/cs/bigdata/datacamp/fin_credit_risk");source("model_credit_pract.R")

pprint<-function(msg, bc="*"){
  mlen <- nchar(msg)
  bord_str <- paste(rep(bc, mlen+4),collapse="")
  msg_bord_str <- paste(rep(bc, 1),collapse="")
  print(bord_str)
  print(paste( msg_bord_str,msg, msg_bord_str), sep="")
  print(bord_str)
}

require(data.table)
require(gmodels)

loan_data<-fread("loan_data.csv",  stringsAsFactors=TRUE)

str(loan_data)

pprint("exploring the credit data")

print("grade vs loan_status")
CrossTable(loan_data$grade, loan_data$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

print(paste('CrossTable() on home_ownership and loan_status (1 = default)'))
CrossTable(loan_data$home_ownership, loan_data$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

paste('CrossTable() on grade and loan_status')
CrossTable(loan_data$grade, loan_data$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
     
######################
pprint("plotting data with outliers cut")

par(mfrow=c(2,1))
n_breaks <- sqrt(nrow(loan_data)) # ~170
hist_1<-hist(loan_data$annual_inc, breaks=n_breaks, main="histogram of annual income")


outlier_cutoff <- quantile(loan_data$annual_inc, 0.75) + 1.5 * IQR(loan_data$annual_inc)
index_outlier_ROT <- which(loan_data$annual_inc > outlier_cutoff)
loan_data_ROT <- loan_data[-index_outlier_ROT,]
nr_breaks <- sqrt(nrow(loan_data_ROT))
hist_2<-hist(loan_data_ROT$annual_inc, breaks=nr_breaks)

par(mfrow=c(1,1))
#take out outlier with unrealistic age
index_highage <- which(loan_data$age > 93)
loan_data_p <- loan_data[-index_highage, ]
plot(loan_data_p$age, loan_data_p$annual_inc, xlab = "Age", ylab = "Annual Income",cex=0.2)

######################
pprint("Deleting  or keeping missing data")
summary(loan_data$int_rate)

#index of rows with int_rate == NA
na_index<- which(is.na(loan_data$int_rate)==TRUE)
loan_data_delrow_na <- loan_data[-na_index, ]  #strip off this rows from the copied dataset

#removing entire column
loan_data_delcol_na <- loan_data
# Delete interest rate column from loan_data_delcol_na
loan_data_delcol_na$int_rate <- NULL

# Replace missing interest rates with median
median_ir <- median(loan_data$int_rate, na.rm=TRUE)
loan_data_replace <- loan_data
loan_data_replace$int_rate[na_index] <- median_ir

loan_data$ir_cat <- NULL  # strip this column as we are going to recreate it
##introducing a categorical variable ir_cat  with keeping NA in 'Missing' value
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))

loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"

loan_data$ir_cat <- as.factor(loan_data$ir_cat)

# Look at your new variable using plot()
plot(loan_data$ir_cat, main="Int Rate category")



######################
pprint("splitting data set to train and test (2/3 and 1/3")

set.seed(567)

loan_data<-fread("loan_data.csv",  stringsAsFactors=TRUE)
##strip this two columns
loan_data$int_rate <- NULL
loan_data$emp_length <- NULL

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(loan_data), 2/3*nrow(loan_data) )

# Create training set: training_set
training_set <- loan_data[index_train, ]

# Create test set: test_set
test_set <- loan_data[-index_train,]


model_pred <- c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,1,0,1,0,1,1,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,1,1,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1,1,0,1,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,1,1,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,1,0,0,0,0,1,1,0,1,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,1,0,0,0,0,1,0,1,1,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,1,0,1,1,1,1,0,0,1,0,0,1,0,1,1,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,1,1,0,0,1,0,0,0,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,1,0,1,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,1,0,1,1,1,0,1,0,1,0,1,0,0,0,0,0,1,1,0,0,1,1,1,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1,1,1,1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,1,0,1,1,1,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,0,1,0,1,0,0,0,0,0,0,1,1,1,1,0,0,1,0,1,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,1,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,1,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,0,1,1,0,1,1,1,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,1,1,0,1,1,0,1,0,1,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,1,1,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,1,0,1,0,0,1,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,1,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,1,1,0,1,0,1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,0,1,1,1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,1,1,1,1,0,0,1,0,0,1,1,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,1,0,0,1,0,0,1,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0,1,0,1,0,1,0,0,1,0,1,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,1,1,0,1,0,1,1,0,1,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,1,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,0,1,0,1,1,0,0,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,1,1,0,1,0,0,1,0,1,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,0,1,0,1,1,1,1,1,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0,1,0,1,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,0,1,1,0,1,0,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,1,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,1,0,1,1,0,0,0,0,0,1,0,1,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,1,0,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,0,1,0,1,1,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,1,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,1,1,0,0,1,1,1,0,1,0,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,1,0,1,0,0,1,0,0,0,0,0,0,0,1,1,1,0,0,0,1,0,0,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,1,1,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,1,0,1,0,0,0,0,1,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,1,0,1,0,1,0,0,0,0,1,0,1,1,0,0,0,0,0,1,1,1,0,1,0,0,1,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,1,0,1,1,1,0,0,1,1,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,1,0,1,0,1,0,0,1,0,1,0,1,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,1,1,0,0,1,1,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,1,1,0,1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,1,1,0,0,0,1,0,1,0,0,1,0,0,1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,0,0,0,1,0,0,0,1,1,1,0,0,0,0,1,1,0,0,1,0,1,1,1,0,0,1,0,0,1,0,1,0,0,0,0,1,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,1,1,1,1,1,0,0,0,1,0,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,0,1,1,0,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,1,0,0,0,1,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,1,1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,1,0,1,0,0,0,1,1,1,0,1,0,0,1,0,0,1,0,0,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,0,0,1,0,1,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,1,1,1,0,0,0,1,0,1,0,1,0,1,0,1,1,1,0,0,1,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,1,0,0,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,1,0,0,0,1,0,0,0,1,1,0,0,0,1,0,1,0,1,1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,1,1,0,1,0,0,0,1,0,0,0,0,1,1,0,0,1,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,1,0,0,0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,1,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,1,1,0,1,0,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,1,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,1,0,0,1,0,1,0,0,0,0,0,1,1,1,0,0,0,1,0,1,1,1,0,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,0,1,0,1,1,0,1,1,0,0,0,1,0,0,0,0,0,1,1,1,0,0,1,1,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,1,1,0,1,0,1,0,0,1,0,0,0,1,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,1,0,1,0,0,0,1,1,0,0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,1,1,0,0,1,0,0,0,1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,0,1,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,1,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,1,0,0,0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,1,0,0,0,0,1,0,1,0,0,0,1,0,0,1,1,0,0,0,1,1,0,0,0,1,1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,1,0,1,1,0,1,0,1,1,1,1,0,0,0,0,0,1,1,0,0,1,0,1,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,1,1,0,0,1,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,1,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,1,1,1,0,1,0,0,0,0,1,0,0,0,1,1,0,1,0,1,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,1,0,1,0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,1,1,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,0,1,1,0,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,1,0,1,0,0,0,1,1,1,0,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,0,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,0,1,1,1,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0,0,1,1,0,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1,1,0,1,0,0,1,0,1,1,0,1,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,0,1,1,1,0,0,1,0,1,0,0,0,0,0,0,1,1,0,1,1,1,0,0,1,0,1,1,1,1,0,1,0,1,1,0,0,1,0,0,0,1,0,0,0,0,1,1,1,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,1,1,1,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,1,0,0,0,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,0,0,1,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,1,1,1,0,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,1,1,1,1,1,0,1,1,0,0,1,1,0,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,0,0,0,1,0,0,1,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,1,0,0,1,0,1,1,1,0,0,0,1,1,0,0,0,0,0,1,1,1,0,0,1,0,1,0,0,0,1,0,0,1,1,1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,0,0,0,0,1,1,0,0,1,0,0,1,1,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,1,1,1,0,0,1,0,0,1,0,0,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,0,1,0,0,0,1,1,0,1,1,1,1,0,0,1,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0,1,1,1,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,1,1,0,1,0,0,0,1,0,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,0,0,1,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,1,0,0,1,0,1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,0,1,0,1,1,0,0,0,1,1,1,1,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,1,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,1,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,1,1,1,0,0,1,0,1,1,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,1,1,0,1,0,0,0,0,0,1,0,0,1,0,1,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,1,0,1,0,1,0,0,0,1,0,1,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,1,1,0,1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,1,0,1,0,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,1,0,0,0,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,1,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,0,1,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,0,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,1,0,1,0,0,1,1,0,0,0,1,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0,1,1,1,0,0,0,1,0,0,1,0,1,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,1,1,0,1,0,0,0,0,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,0,1,0,0,0,0,1,1,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,1,0,0,0,0,1,0,1,1,0,1,0,1,1,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0)

conf_matrix <-  table(test_set$loan_status, model_pred)

print('confusion matrix')
print(conf_matrix)


print("Compute classification accuracy")
print( sum(diag(conf_matrix)) / sum(conf_matrix) )

print("Compute sensitivity")
print( conf_matrix[2,2] / (conf_matrix[2,2] + conf_matrix[2,1]) )


                                
######################
pprint("Chapter 2. logistic regression")
###########
pprint("basic logistic regression", bc='-')        
# Build a glm model with variable ir_cat as a predictor

log_model_cat <- glm(loan_status ~ ir_cat, family="binomial", data=training_set)

# Print the parameter estimates 
print(log_model_cat)

# Look at the different categories in ir_cat using table()
print(table(loan_data$ir_cat))

#########
pprint("multiple variables in logistic regression", bc="-")
# Build the logistic regression model
log_model_multi <- glm(loan_status ~ age+ir_cat+grade+loan_amnt+annual_inc, family="binomial", data=training_set)

# Obtain significance levels using summary()
print(summary(log_model_multi))

#########
pprint("predicting the probability of default", bc="-")
# Build the logistic regression model
log_model_small <- glm(loan_status ~ age+ir_cat, family="binomial", data=training_set)
predictions_all_small <- predict(log_model_small, newdata = test_set, type = "response")

# Look at the range of the object "predictions_all_small"
print( range(predictions_all_small) )


#########
pprint("making more discriminative models", bc="-")

log_model_full <- glm(loan_status~., family="binomial", data=training_set) 


predictions_all_full <- predict(log_model_full, newdata=test_set, type="response")
print(range(predictions_all_full))

##### 
pprint("specifying a cutoff", bc="-")

log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")

# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predictions_all_full > 0.15, 1, 0)

# Construct a confusion matrix
cfm <- table(test_set$loan_status, pred_cutoff_15) 
print(cfm)
accu <- sum(diag(cfm))/sum(cfm)
print(paste("accuracy:",accu))

sensi <- cfm[2,2] / sum(cfm[2,])
print(paste("sensitivity:",sensi))

########
pprint("Comparing link functions for a given cut-off", bc="-")
log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = logit), data = training_set)
log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = probit), data = training_set)

log_model_cloglog <-  glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = cloglog), data = training_set)
  
# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")
  
# Use a cut-off of 14% to make binary predictions-vectors
cutoff <- 0.14
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)

true_val <- test_set$loan_status
  
# Make a confusion matrix for the three models
tab_class_logit <- table(true_val,class_pred_logit)
tab_class_probit <- table(true_val,class_pred_probit)
tab_class_cloglog <- table(true_val,class_pred_cloglog)
  
# Compute the classification accuracy for all three models
acc_logit <- sum(diag(tab_class_logit)) / nrow(test_set)
acc_probit <- sum(diag(tab_class_probit)) / nrow(test_set)
acc_cloglog <- sum(diag(tab_class_cloglog)) / nrow(test_set)

print(acc_logit)
print(acc_probit)
print(acc_cloglog)

########
pprint("Chap 3 Decision Trees" )


pprint("computing the gain for a tree", bc='-' )

status <- c(0,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,1,0,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0)

n_sample <- length(status)
n_defaults <- length(which(status==1))

gini_root <- 2 * n_defaults / n_sample * (n_sample - n_defaults) / n_sample

##########
pprint("building decision trees using rpart package", bc='-' )
require(rpart)

##########
pprint("undersampling the training set", bc='-' )

idx_default <- which(training_set$loan_status == 1)
idx_nodefault <- which(training_set$loan_status == 0)

sample_idx_nodefault <- sample(idx_nodefault, size=4380) #to have undersampled_training_set length to 6570

idx_undersmapled <- c(idx_default,sample_idx_nodefault)
undersampled_training_set <- training_set[idx_undersmapled,]


tree_undersample <- rpart(loan_status ~ ., method = "class",
                          data = undersampled_training_set,
                          control = rpart.control(cp = 0.001))

# Plot the decision tree
plot(tree_undersample,uniform=TRUE)
text(tree_undersample)



##########
pprint("changing the prior probabilities", bc='-' )
# Change the code below such that a tree is constructed with adjusted prior probabilities.
tree_prior <- rpart(loan_status ~ ., method = "class",
                    data = training_set,
                    parms = list(prior=c(0.7, 0.3)),
                    control = rpart.control(cp = 0.001))

# Plot the decision tree
par(mfrow=c(1,2))
plot(tree_prior,uniform=TRUE)
plot(tree_prior,uniform=TRUE)

# Add labels to the decision tree

text(tree_prior)

par(mfrow=c(1,1))


##########
pprint("including a loss matrix", bc='-' )

# parms = list(loss = matrix(c(0, cost_default_as_nondefault, cost_nondefalt_as_default, 0), ncol=2))
# cost_default_as_nondefault = 10, cost_nondefalt_as_default = 1
# strong skew to avoid misclassification of actual defaults

tree_loss_matrix <- rpart(loan_status ~ ., method = "class",
                          data =  training_set,
                          parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)), 
                          control = rpart.control(cp = 0.001))


# Plot the decision tree
par(mfrow=c(1,2))
plot(tree_loss_matrix, uniform=TRUE)
plot(tree_loss_matrix, uniform=TRUE)
# Add labels to the decision tree
text(tree_loss_matrix)

par(mfrow=c(1,1))

###########
pprint("Pruning the tree with changed prior probabilities", bc='-')

plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized.
printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[,"xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_prior)



###########
pprint("Pruning the tree with the loss matrix", bc='-')

set.seed(345)
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix)

# Prune the tree using cp = 0.0012788
ptree_loss_matrix<-prune(tree_loss_matrix, cp=0.0012788)

# Use prp() and argument extra = 1 to plot the pruned tree
prp(ptree_loss_matrix,extra=1)

##########
pprint("One final tree using more options", bc='-')

#construct case_weights 
training_set$case_weights <- ifelse(training_set$loan_status == 1, 3, 1)
case_weights <- training_set$case_weights
training_set$case_weights <- NULL 

set.seed(345)
tree_weights <- rpart(loan_status ~ ., method = "class",
                      data = training_set,
                      weights <- case_weights,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))

# Plot the cross-validated error rate for a changing cp
plotcp(tree_weights)

# Create an index for of the row with the minimum xerror

index <- which.min(tree_weights$cp[ , "xerror"])

# Create tree_min

tree_min <- tree_weights$cp[index, "CP"]

# Prune the tree using tree_min
ptree_weights <- prune(tree_weights, cp=tree_min)

# Plot the pruned tree using the rpart.plot()-package
prp(ptree_weights,extra=1)

#######
pprint("Confusion matrices and accuracy of our final trees", bc="-")

## preprocessing prune tree_undersample

index <- which.min(tree_undersample$cptable[,"xerror"])
tree_min <- tree_undersample$cptable[index, "CP"]
ptree_undersample <- prune(tree_undersample, cp = tree_min)

# Make predictions for each of the pruned trees using the test set.
pred_undersample <- predict(ptree_undersample, newdata = test_set,  type = "class")
pred_prior <- predict(ptree_prior, newdata = test_set,  type = "class")
pred_loss_matrix <-predict(ptree_loss_matrix, newdata = test_set,  type = "class")
pred_weights <-predict(ptree_weights, newdata = test_set,  type = "class")

# construct confusion matrices using the predictions.
confmat_undersample <- table(test_set$loan_status, pred_undersample)
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
confmat_weights <-table(test_set$loan_status, pred_weights)

# Compute the accuracies
acc_undersample <- sum(diag(confmat_undersample)) / nrow(test_set)
acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)
acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)

########################

pprint("Chapter 4 Evaluation a credit risk model")

pprint("Computing a bad rate given a fixed acceptance rate", bc="-")

prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]

# Obtain the cutoff for acceptance rate 80%
  cutoff_prior <- quantile(prob_default_prior, 0.80)

# Obtain the binary predictions.
bin_pred_prior_80 <- ifelse(prob_default_prior > cutoff_prior, 1, 0)

# Obtain the actual default status for the accepted loans
accepted_status_prior_80 <- test_set$loan_status[bin_pred_prior_80 == 0]

# Obtain the bad rate for the accepted loans
bad_rate <- length(which(accepted_status_prior_80 == 1)) / length(accepted_status_prior_80)

print(paste("bad rate:", bad_rate))

######
pprint("The strategy table and strategy curve", bc="-")

## this function iterates over accept_rate probability from 0 to 1 with steop 0.05
strategy_bank <- function(prob_of_def){
    cutoff=rep(NA, 21)
    bad_rate=rep(NA, 21)
    accept_rate=seq(1,0,by=-0.05)
    for (i in 1:21){
      cutoff[i]=quantile(prob_of_def,accept_rate[i])
      pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
      pred_as_good=test_set$loan_status[pred_i==0]
      bad_rate[i]=sum(pred_as_good)/length(pred_as_good)}
    table<-cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
    return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
}
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")
predictions_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set )[,2] #probas from a tree

# Apply the function strategy_bank to both predictions_cloglog and predictions_loss_matrix
strategy_cloglog <- strategy_bank(predictions_cloglog)
strategy_loss_matrix <- strategy_bank(predictions_loss_matrix)

# Obtain the strategy tables for both prediction-vectors
print( strategy_cloglog$table )
print( strategy_loss_matrix$table  )

# Plot the strategy functions
par(mfrow = c(1,2))
plot(strategy_cloglog$accept_rate, strategy_cloglog$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "logistic regression")

plot(strategy_loss_matrix$accept_rate, strategy_loss_matrix$bad_rate, 
     type = "l", xlab = "Acceptance rate", 
     ylab = "Bad rate", lwd = 2, main = "tree")


par(mfrow = c(1,1))

#########
pprint("ROC-curves for comparison of logistic regression models", bc='-')

predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")
predictions_all_full <- predict(log_model_full, newdata=test_set, type="response")

# Load the pROC-package
require(pROC)

# Construct the objects containing ROC-information
ROC_logit <- roc(test_set$loan_status, predictions_logit)
ROC_probit <- roc(test_set$loan_status, predictions_probit)
ROC_cloglog <- roc(test_set$loan_status, predictions_cloglog)
ROC_all_full <- roc(test_set$loan_status, predictions_all_full)

# Draw all ROCs on one plot
plot(ROC_logit)
lines(ROC_probit, col="blue")
lines(ROC_cloglog , col="red")
lines(ROC_all_full, col="green")

# Compute the AUCs
print("AUCs for 4 models")

print( auc(ROC_logit) )
print( auc(ROC_probit) )
print( auc(ROC_cloglog) )
print( auc(ROC_all_full) )


######
pprint("ROC-curves for comparison of tree-based models", bc='-')

predictions_undersample <- predict(ptree_undersample, newdata = test_set)[,2]
predictions_prior <- predict(ptree_prior, newdata = test_set )[,2]
predictions_loss_matrix <-predict(ptree_loss_matrix, newdata = test_set)[,2]
predictions_weights <-predict(ptree_weights, newdata = test_set)[,2]

# Construct the objects containing ROC-information
ROC_undersample <- roc(test_set$loan_status, predictions_undersample)
ROC_prior <- roc(test_set$loan_status, predictions_prior)
ROC_loss_matrix <- roc(test_set$loan_status, predictions_loss_matrix)
ROC_weights <- roc(test_set$loan_status, predictions_weights)

# Draw the ROC-curves in one plot
plot(ROC_undersample)
lines(ROC_prior, col="blue")
lines(ROC_loss_matrix , col="red")
lines(ROC_weights, col="green")
  

    
# Compute the AUCs
print( auc(ROC_undersample) )
print( auc(ROC_prior) )
print( auc(ROC_loss_matrix) )
print( auc(ROC_weights) )


###########
pprint("Another round of pruning based on AUC", bc='-')

# Build four models each time deleting one variable in log_3_remove_ir
log_4_remove_amnt <- glm(loan_status ~ grade + annual_inc + emp_cat, 
                        family = binomial, data = training_set) 
log_4_remove_grade <- glm(loan_status ~ loan_amnt + annual_inc + emp_cat, 
                        family = binomial, data = training_set)
log_4_remove_inc <- glm(loan_status ~ grade + loan_amnt + emp_cat, 
                        family = binomial, data = training_set)
log_4_remove_emp <-glm(loan_status ~ grade + annual_inc + loan_amnt, 
                        family = binomial, data = training_set)

# Make PD-predictions for each of the models
pred_4_remove_amnt <- predict(log_4_remove_amnt, newdata = test_set, type = "response")
pred_4_remove_grade <- predict(log_4_remove_grade, newdata = test_set, type = "response")
pred_4_remove_inc <- predict(log_4_remove_inc, newdata = test_set, type = "response")
pred_4_remove_emp <- predict(log_4_remove_emp, newdata = test_set, type = "response")

# Compute the AUCs

print( auc(test_set$loan_status,pred_4_remove_amnt ) )
print( auc(test_set$loan_status,pred_4_remove_grade ) )
print( auc(test_set$loan_status,pred_4_remove_inc ) )
print( auc(test_set$loan_status,pred_4_remove_emp ) )

############
pprint("Further model reduction?", bc='-')
  
# Build three models each time deleting one variable in log_4_remove_amnt
log_5_remove_grade <- glm(loan_status ~ annual_inc + emp_cat, family = binomial, data = training_set) 
log_5_remove_inc <- glm(loan_status ~ grade + emp_cat, family = binomial, data = training_set) 
log_5_remove_emp <- glm(loan_status ~ grade + annual_inc, family = binomial, data = training_set) 

# Make PD-predictions for each of the models
pred_5_remove_grade <- predict(log_5_remove_grade, newdata = test_set, type = "response")
pred_5_remove_inc <- predict(log_5_remove_inc, newdata = test_set, type = "response")
pred_5_remove_emp <- predict(log_5_remove_emp, newdata = test_set, type = "response")

# Compute the AUCs
print( auc(test_set$loan_status,pred_5_remove_grade ) )
print( auc(test_set$loan_status,pred_5_remove_inc ) )
print( auc(test_set$loan_status,pred_5_remove_emp ) )


# Plot the ROC-curve for the best model here
plot(roc(test_set$loan_status, pred_4_remove_amnt ))
