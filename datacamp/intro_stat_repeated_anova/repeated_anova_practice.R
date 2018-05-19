#rm(list=ls());  setwd('~/Dropbox/cs/bigdata/datacamp/intro_stat_repeated_anova/'); source('repeated_anova_practice.R')

## course 4 repeated anova

 #working memory data set
#condition how many days training lasted
#iq  gain in iq
#subject a person's id 
condition<-c('8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days')
iq<-c(12.4,11.8,14.6,7.7,15.7,11.6,7,8.4,10.7,10.6,5.4,12.3,9.3,11,11.9,14.1,12.1,13.2,8.8,9.6,12.5,11.6,8.9,8.3,10.9,13.4,12.3,8.7,9.7,9.7,6.9,10.5,11.6,13.8,15.6,11.7,16.1,11.7,15,15.1,13.4,9.8,13.2,12.8,18.1,13.8,14.5,17.2,14.2,10.4,12,17.4,12.8,12.5,14.6,15.6,13.2,11.2,15.4,16,14,17,10.4,11.3,18.9,16.7,13.9,19.2,14.2,11.5,15.5,12.5,16.1,11.1,16.1,15.2,15.4,16.8,13.5,15.7)
subject <- rep(1:20,4)
wm<-data.frame(subject=factor(subject), condition=factor(condition, levels=c('8 days', '12 days', '17 days', '19 days')),iq=iq)
# Summary statistics by all groups
describeBy(wm)

boxplot(wm$iq ~ wm$condition, main="Boxplot", xlab="Training days", ylab="IQ")

#### plot evolution of each subjects across independent variables (feautures)
library(ggplot2)
ggplot(data = wm, aes(x = condition, y = iq, group = subject, colour = subject)) +  geom_line() + geom_point()

##mauchly's test
iq <- cbind(wm$iq[wm$condition == "8 days"], 
            wm$iq[wm$condition == "12 days"], 
            wm$iq[wm$condition == "17 days"], 
            wm$iq[wm$condition == "19 days"]) 
#make a leaner model of response vaiable vs 1
mlm <- lm(iq ~ 1)
# Mauchly's test
mt<-mauchly.test(mlm, x = ~ 1) 
print(mt)
## sphericity of variables holds as p > 0.05

### compute F-value for repeated measures
# 1. systematic between-groups variance
n<-20
y_j <- tapply(wm$iq,wm$condition, mean) # group by experiement ( 4 measures)
y_t <-mean(wm$iq) # grand mean
ss_cond<-n*sum( (y_j-y_t)^2 )
df <- 4-1
ms_cond <- ss_cond/df 
print(ms_cond)

# 2. subject variance 
n<-4 # # of conditions for each subject
y_j <- tapply(wm$iq, wm$subject,mean) #grouping by subject , 20 measures
y_t <-mean(wm$iq) # grand mean
ss_subjects <- n*sum( (y_j -y_t)^2 )
df<-20-1
ms_subjects <- ss_subjects/df
print(ms_subjects)

#3. unsystematic within-group variance or the error term of between-groups design

#create 4 subsets on cndition
y_i1 <- subset(wm$iq, wm$condition == "8 days")
y_i2 <- subset(wm$iq, wm$condition == "12 days")
y_i3 <- subset(wm$iq, wm$condition == "17 days")
y_i4 <- subset(wm$iq, wm$condition == "19 days")


#compute ss_s/a
s_1 <-  y_i1 - mean(y_i1)
s_2 <-  y_i2 - mean(y_i2) 
s_3 <-  y_i3 - mean(y_i3) 
s_4 <-  y_i4 - mean(y_i4) 

s_t <- c(s_1,s_2,s_3,s_4)

ss_sa <- sum(s_t^2)


df<-4*19  #  number of groups * ( numbe of subjects in group -1)
ms_sa <- ss_sa/df
print(ms_sa)

##compute errror term 
ss_rm <- ss_sa - ss_subjects 
df<-3*19
ms_rm <- ss_rm /df
print(ms_rm)

#compute f-ratio
f_rat <- ms_cond / ms_rm 
df1 <- 4-1
df2 <- (4-1)*(20-19)
p_val <- 1-pf(f_rat, df1, df2)
print(paste(f_rat,p_val))

# in R 
# ANOVA model
model <- aov(wm$iq ~ wm$condition + Error(wm$subject / wm$condition) ) #Error() term because repeated measures

# Summary of model
print(summary(model))

# effect size
# eta squared  is ss_a / ss_total
eta_sq <- ss_cond / (ss_cond + ss_rm)
print(eta_sq)


##post hoc tests 
#test 1
# Post-hoc test: default procedure
pt1<-with(wm, pairwise.t.test(iq, condition, paired = TRUE))
print(pt1)

#with conservative bonferoni
pt2<-with(wm, pairwise.t.test(iq, condition, p.adjust.method='bonferroni',paired = TRUE, ))
print(pt2)

##check p_values with individual paired t-test
cond_12days <- subset(wm$iq, wm$condition =='12 days')
cond_17days <- subset(wm$iq, wm$condition =='17 days')
tt<-t.test(cond_12days, cond_17days, paired=TRUE)
print(tt)




#end