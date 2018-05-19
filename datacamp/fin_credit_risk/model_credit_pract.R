#setwd("~/Dropbox/cs/bigdata/datacamp/fin_credit_risk");source("model_credit_pract.R")

require(data.table)
require(gmodels)

loan_data<-fread("loan_data.csv",  stringsAsFactors=TRUE)

str(loan_data)

print(paste('CrossTable() on home_ownership and loan_status (1 = default)'))

CrossTable(loan_data$home_ownership, loan_data$loan_status, prop.r = TRUE,
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


     