#rm(list=ls());  setwd('~/Dropbox/cs/bigdata/datacamp/intro_stat_anova/'); source('stats_modeling_practice.R') 

#rm(list=ls());  setwd('C:/Users/zimine/Dropbox/cs/bigdata/datacamp/statistical_modeling'); source('stats_modeling_practice.R') 



#1. course Introduction
#1.1 variables
#converting variables to nominal variables


var1<-factor(c(1,2,3))
class(var1)

#factor with order = ordinal variables

temperature_vector <- c("High", "Low", "High", "Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, 
                                    levels = c("Low", "Medium", "High"))


# Create an interval variable called longitudes
longitudes <- c(10, 20, 30, 40)

# Create a ratio variable called chronos
chronos <- c(10.60, 10.12, 9.58, 11.1)                                    


# 1.2 histograms
##create a historgramß
hist(rnorm(100), main="histo title",xlab="score", ylab="freq")

##obtain the z-measure (z-score) z=(x-mu)/sigma
gain<-c(1,2,0,-1,2,1,3,2,1,1,-1,2,1,3,3,0,1,1,2,3,2,2,4,2,2,5,2,4,2,2,0,2,4,2,2,5,2,4,2,2,4,4,7,3,6,4,4,3,3,6,4,4,7,3,6,4,4,3,3,6,7,5,9,6,4,7,5,3,6,4,7,5,9,6,4,7,5,3,6,4,1,2,0,-1,2,1,3,2,1,1,-1,2,1,3,3,0,1,1,2,3,2,2,4,2,2,5,2,4,2,2,0,2,4,2,2,5,2,4,2,2)
length(gain)
mean(gain)
median(gain)
var(gain)
sd(gain)
gain_z<-scale(gain)





#rm(list=ls()) ; dev.off()
#browser()

# course 2 t-test 

## working memory data set
cond<-c('t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t08','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t12','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t17','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','t19','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control','control')
pre<-c(8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,12,12,12,12,12,8,8,9,9,9,9,10,10,10,10,11,11,11,12,12,12,12,8,8,9,9,9,9,10,10,10,10,11,11,11,12,12,12,12,8,8,9,9,9,9,10,10,10,10,11,11,11,12,12,12,12,8,8,9,9,9,9,10,10,10,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,12,12,12,12,12,8,8,9,9,9,9,10,10,10,10,11,11,11,12,12,12,12,8,8,9)
post<-c(9,10,8,7,11,10,12,11,11,11,9,12,12,14,14,12,13,13,14,15,10,10,13,11,11,14,12,14,12,12,11,13,15,14,14,17,14,12,10,11,13,13,16,13,16,14,14,14,14,17,16,16,19,15,14,12,13,12,12,15,17,15,19,16,15,18,16,15,18,16,19,13,17,15,13,16,14,13,16,14,9,10,8,7,11,10,12,11,11,11,9,12,12,14,14,12,13,13,14,15,10,10,13,11,11,14,12,14,12,12,11,13,15,14,14,17,14,12,10,11)
gain<-c(1,2,0,-1,2,1,3,2,1,1,-1,2,1,3,3,0,1,1,2,3,2,2,4,2,2,5,2,4,2,2,0,2,4,2,2,5,2,4,2,2,4,4,7,3,6,4,4,3,3,6,4,4,7,3,6,4,4,3,3,6,7,5,9,6,4,7,5,3,6,4,7,5,9,6,4,7,5,3,6,4,1,2,0,-1,2,1,3,2,1,1,-1,2,1,3,3,0,1,1,2,3,2,2,4,2,2,5,2,4,2,2,0,2,4,2,2,5,2,4,2,2)
train<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


wm_1<-data.frame(cond=factor(cond), pre=pre,post=post,gain=gain,train=train)
summary(wm_1) #descriptive statistics on a data frame


dev.off()

#browser()


#course 3 ANOVA
#####################

#working memory data set
#condition how many days training lasted
#iq  gain in iq
#subject a person's id 
condition<-c('8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','8 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','12 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','17 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days','19 days')
iq<-c(12.4,11.8,14.6,7.7,15.7,11.6,7,8.4,10.7,10.6,5.4,12.3,9.3,11,11.9,14.1,12.1,13.2,8.8,9.6,12.5,11.6,8.9,8.3,10.9,13.4,12.3,8.7,9.7,9.7,6.9,10.5,11.6,13.8,15.6,11.7,16.1,11.7,15,15.1,13.4,9.8,13.2,12.8,18.1,13.8,14.5,17.2,14.2,10.4,12,17.4,12.8,12.5,14.6,15.6,13.2,11.2,15.4,16,14,17,10.4,11.3,18.9,16.7,13.9,19.2,14.2,11.5,15.5,12.5,16.1,11.1,16.1,15.2,15.4,16.8,13.5,15.7)
wm<-data.frame(subject=seq(1:80), condition=factor(condition),iq=iq)

# dependent variable is wm$iq  , independent variable wm$condition  (groups)
#null hypo:  iq gain does not change statistically over different training period

# summary statistics by group
require(psych)
describeBy(wm, wm$condition)

# visualize preliminary statistics
#  boxplot (dependent_variable  ~ independent_variable )  # how iq (dependent) changes as a function of group cond (indepedent)
boxplot(wm$iq ~ wm$condition,  main="Boxplot", xlab="Group (cond)", ylab="IQ")

##generate density plot of the F distribution
## df has df1, and df2  degrees of freedom
x<-seq(from=0,to=2,length=200)
y_1 <- df(x,1,1)
y_2 <- df(x,3,1)
y_3 <- df(x,6,1)
y_4 <- df(x,3,3)
y_5 <- df(x,6,3)
y_6 <- df(x,3,6)
y_7 <- df(x,6,6)

# Plot the densities
plot(x,  y_1, col = 1, type = "l")
lines(x, y_2, col = 2)
lines(x, y_3, col = 3)
lines(x, y_4, col = 4)
lines(x, y_5, col = 5)
lines(x, y_6, col = 6)
lines(x, y_7, col = 7)

# Add the legend
legend("topright", title = "F distribution",
       c("df = (1,1)", "df = (3,1)", "df = (6,1)", "df = (3,3)", 
         "df = (6,3)", "df = (3,6)", "df = (6,6)"), 
       col = c(1, 2, 3, 4, 5, 6, 7), lty = 1)


##computing the F ratio
#1 compute sum of squaires between group 

# number of subjects in each group 
n <- nrow(subset(wm, wm$condition %in% "8 days"))
### a vecgtor of means per group
y_j <- tapply(wm$iq,wm$condition, mean)  # 2nd argument = column with info on each group
y_t<-mean(wm$iq)  ## grand mean
##between group sum of squaires
ss_a<-n*sum((y_j-y_t)^2)

#2 compute variance within groups  (within groups sum of squaires)
y_i1 <- subset(wm$iq, wm$cond == "8 days")
y_i2 <- subset(wm$iq, wm$cond == "12 days")
y_i3 <- subset(wm$iq, wm$cond == "17 days")
y_i4 <- subset(wm$iq, wm$cond == "19 days")

i<-which(attributes(y_j)$dimnames[[1]] == "8 days")
s_1 <- y_i1 - y_j[i]  ## individual scores - mean of its group
i<-which(attributes(y_j)$dimnames[[1]] == "12 days")
s_2 <- y_i2 - y_j[i]
i<-which(attributes(y_j)$dimnames[[1]] == "17 days")
s_3 <- y_i3 - y_j[i]
i<-which(attributes(y_j)$dimnames[[1]] == "19 days")
s_4 <- y_i4 - y_j[i]

s_t <- c(s_1,s_2,s_3,s_4) # concatenate all groups

ss_sa<-sum(s_t^2)  # compute  sum of squares (within group variance)

# Number of groups
a <- 4
# Number of subjects in each group
n <- 20
# Define degrees of freedom
df_a <- a-1
df_sa <- a*(n-1)

# Calculate mean squares using ss_a and ss_sa
ms_a <- ss_a / df_a
ms_sa <- ss_sa / df_sa
# Calculate the F-ratio
f_rat <-  ms_a / ms_sa

##compute f-value with R aov(dependedent_var ~ independent var)
anova_wm <- aov(wm$iq ~ wm$condition)
summary(anova_wm)

##levene's test  (homogeneity of variance)
require(car)
leveneTest(wm$iq, wm$condition,center=mean)
#Pr value > F value so homegeneity of variance holds
 
#chap 2 Post-hoc tests

## def. allow for multiple pairwise comparisons without an incarease in the probability
# of type I error

anova_wm1<-aov(wm_1$gain ~wm_1$cond)
summary(anova_wm1)
tukey<-TukeyHSD(anova_wm1)
plot(tukey)


##bonferonni a more conservative method
bonferroni_ex<-p.adjust(p=0.005, method="bonferroni", n=8)
pairwise.t.test(wm_1$gain, wm_1$cond, p.adjust.method="bonferroni", paired=FALSE)

#chap 3 factorial anova
# when 1 dependent variable and two independent variables

#preparing a dataset
conversation<-c('None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','None','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','Low demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand','High demand')
driving<-c('Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Easy','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult','Difficult')
errors<-c(20,19,31,27,31,17,23,26,11,15,13,18,21,13,13,14,20,26,20,14,10,23,31,32,24,26,46,25,23,25,13,32,32,16,26,24,11,15,19,12,21,23,7,25,15,21,19,1,25,20,32,32,14,34,21,13,20,28,10,15,46,30,37,31,25,23,38,36,43,41,43,32,37,23,34,46,54,45,33,42,21,33,24,21,29,24,28,31,22,21,42,20,25,20,21,16,23,37,34,25,46,49,56,41,56,36,46,39,42,56,51,27,45,48,50,61,43,47,54,41)

ab<-data.frame(subject=seq(1,120),conversation=factor(conversation),driving=factor(driving),errors=errors)
head(ab)

##create 6 subgroups
ab_groups <- tapply(ab$errors, list(ab$driving, ab$conversation), sum)
barplot(ab_groups,beside=T,col=c("orange","blue"), main="Driving Errors", xlab="Conversation Demands", ylab="Errors")
legend("topright", rownames(ab_groups), title="Driving", fill=c("orange","blue"))

###NB from this barplot it is visually clear that subjects tend
#to makre more driving errors in difficult driving conditions. 
#This effects seems to get stronger when subjects are having 
#a difficult conversation.# Pr value > F value -> variances are comparable
# so we can go on with Main and Int 
#It what follows, you will learn how to formally assess the
#statistical significance of these intuitive obeservations

### 
##checking of homogeneity of variance assumption
leveneTest(ab$errors ~ ab$conversation * ab$driving)
# effect analysis
##performaing  factorial anova
ab_model <- aov(ab$errors ~ ab$conversation * ab$driving) 
summary(ab_model)
## 2 main effects and the interaction effect are significant

#post-hoc tests and effect sizes
#effect size  complete eta^2 , partial eta^2
ab_1<-subset(ab, ab$driving =="Easy")
ab_2<-subset(ab, ab$driving =="Difficult")

#one-way anovas on 2 subsets
aov_ab_1<-aov(ab_1$errors ~ ab_1$conversation)
aov_ab_2<-aov(ab_2$errors ~ ab_2$conversation)

summary(aov_ab_1)
summary(aov_ab_2)
require(lsr)
#compute the effect size with eta^2
etaSquared(aov_ab_1,anova=TRUE)
etaSquared(aov_ab_2,anova=TRUE)
#the eta.sq  value shows what percentage of variance in dependent variable explained
#by  an independent variable specific value "Easy driving", "Difficult driving"
#eta.sq (effect size ) is significantly larger for aov_ab_2 (difficult driving) thena for aov_ab_1 (easy driving)

#pairwise comparisons
TukeyHSD(aov_ab_1)
TukeyHSD(aov_ab_2)
#we obseve on p values (p-val < 0.05) values of independent variables with significant effect on  dependent variable


#rm(ist=ls())
#browser()