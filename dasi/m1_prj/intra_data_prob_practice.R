#rm(list=ls());setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1_prj/");source("intra_data_prob_practice.R")

#setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1_prj/");source("intra_data_prob_practice.R")

library(statsr)
library(dplyr)
library(ggplot2)
library(data.table)

#library(openintro)


##load data
print('not reloading data')
load("brfss2013.Rdata")

ds<-data.table(brfss2013)

##rename a dataframe variable
rf13<-brfss2013


## q1 is variable height is normally distributed ? 
# variable htm4

print( summary(rf13$htm4))

#selected 
crf13<-rf13 %>% filter(!is.na(htm4) & htm4 > 50 & htm4 < 280)

smu<-mean(crf13$htm4)
smed<-median(crf13$htm4)


## OK takes time to execute 
# print(
# ggplot(data = crf13,  aes(x = htm4)) +
#   geom_histogram(binwidth=5)
#   + ggtitle("BRFSS 2013 height of subjects") 
#   + labs(x="height [cm]",y="number of subjects") 
#   + geom_vline(aes(xintercept=mean(crf13$htm4),color="Mean"))
#   + geom_vline(aes(xintercept=median(crf13$htm4),color="Median"))
# )


cds<-ds[!is.na(htm4) & htm4 > 50 & htm4 < 280]
ssd<-sd(cds$htm4)
smu <-mean(cds$htm4)


## OK
# print(
# ggplot(data=cds, aes(x=0, y=htm4))
#   + geom_boxplot() + coord_flip() 
#   + ggtitle("BFRSS 2013 subject heights") 
#  + labs(y="height [cm]", x="" ) 
#  + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), aspect.ratio=0.3 )
# )



cds[, `:=`(in_1sd=ifelse( abs(htm4-smu) < 1*ssd, 1,0)
           ,in_2sd=ifelse( abs(htm4-smu) < 2*ssd, 1,0)
           ,in_3sd=ifelse( abs(htm4-smu) < 3*ssd, 1,0) ) ]

print(nrow(cds[in_1sd==1,])/nrow(cds) )
print(nrow(cds[in_2sd==1,])/nrow(cds) )
print(nrow(cds[in_3sd==1,])/nrow(cds) )


crf13 <- crf13 %>%
mutate(in_1sd = ifelse(abs(htm4 -smu) < 1*ssd, 1, 0)
      ,in_2sd = ifelse(abs(htm4 -smu) < 2*ssd, 1, 0)
      ,in_3sd = ifelse(abs(htm4 -smu) < 3*ssd, 1, 0))


crf13 %>% filter(in_1sd ==1) %>% summarize(count=n(), percent=n()/nrow(crf13))
crf13 %>% filter(in_2sd ==1) %>% summarize(count=n(), percent=n()/nrow(crf13))
crf13 %>% filter(in_3sd ==1) %>% summarize(count=n(), percent=n()/nrow(crf13))


qqplot.data <- function (vec, titlestr) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int) + ggtitle(titlestr)
}

# print(
# qqplot.data(crf13$htm4, 'Normal probability plot for BRFSS 2013 height')
# )



#end