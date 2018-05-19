# setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1_prj/"); source('brfss.R')

library(statsr)
library(dplyr)
library(ggplot2)
library(gridExtra)
require(data.table)


is_reload <-  TRUE
#is_reload <- FALSE

if(is_reload){
	print('data reloaded')
	rm(list=ls())
	load(file="brfss2013.Rdata")
}else{
	rm(dt13)
	rm(sel1_brfss2013)
	print('data not reloaded')
}


dt13<-data.table(brfss2013)
sel1_brfss2013 <- brfss2013 %>% 

select (X_frutsum  # sum of consumed fruits in 30 days
	   ,X_vegesum  # sum fo consumed vegatables in 30 days
	   ,X_pacat1  # sport activity
	   ,physhlth) # number of days sick in past 30 days

print ( str(sel1_brfss2013) )

## motivating charts
print (summary(sel1_brfss2013$physhlth) )

p1 <- ggplot(data = sel1_brfss2013 %>% filter(physhlth>0.99 & physhlth < 32) ,  aes(x = physhlth)) +
  geom_histogram(bins=8)
print(p1)

print(
sel1_brfss2013 %>% filter(physhlth>0.99 & physhlth < 32) %>%
group_by(X_pacat1) %>% summarize(sickdays_median=median(physhlth)) %>%
ggplot(aes(x=X_pacat1, y=sickdays_median)) +
  geom_bar(stat="identity") +
  ggtitle("Levels of physical activity vs median of sick days") +
  labs(x="physical activity")
)



print (summary(dt13$physhlth) ) #poor health in last 30 days
#hist(dt13[physhlth <32 & physhlth>0.99,]$physhlth)  ## bi modal
#hist(dt13[physhlth <32 & physhlth>0.99,]$physhlth)  ## bi modal

#create variables
dt13[, is_sick := factor(ifelse(physhlth >0.99 & physhlth <32, "yes", "no"))]

dt13[, is_sport_active := factor(ifelse(is.na(X_pacat1),NA
	                             ,ifelse(X_pacat1 %in%c("Highly active", "Active"), "yes", "no")))]
#	                             ,ifelse(X_pacat1 %in%c("Highly active"), "yes", "no")))]


#  create a health condition variable 
sel1_brfss2013 <- sel1_brfss2013  %>%
mutate(is_sick =factor(ifelse(is.na(physhlth), NA
                       ,ifelse(physhlth >0.99 & physhlth <32, "yes", "no"))))

#create physical condition active variable
sel1_brfss2013 <- sel1_brfss2013  %>%
mutate(is_sport_active =factor(ifelse(is.na(X_pacat1), NA
                       ,ifelse(X_pacat1 %in%c("Highly active", "Active"), "yes", "no"))))


# print (summary(dt13$ftjuda1_) ) #juice
# print (summary(dt13$frutda1_) )  #fruits
# print (summary(dt13$vegeda1_) )  #vegetables

print('fruit and vegetable sums')
print (summary(dt13$X_frutsum) )  #fruits sum
print (summary(dt13$X_vegesum) )  #vegetables sum

# hist(dt13[X_frutsum <1000,]$X_frutsum)  ## right skewed
# hist(dt13[X_vegesum<1000]$X_vegesum)  # right skewed

#make a motivating chart for fruis + vegetables
## creating new variables showing actual number of fruits and vegetables
sel1_brfss2013 <- sel1_brfss2013  %>%
mutate(fruit_consum = X_frutsum/100)

sel1_brfss2013 <- sel1_brfss2013  %>%
mutate(vegetable_consum = X_vegesum/100)

p1<-ggplot(data=sel1_brfss2013 %>% filter(fruit_consum < 10.0)
	,aes(x=fruit_consum)) + 
      geom_histogram(bins=20) +
      ggtitle("Fruits consumed per day") 

p2<-ggplot(data=sel1_brfss2013 %>% filter(vegetable_consum < 10.0)
	,aes(x=vegetable_consum)) + 
     geom_histogram(bins=20) +
      ggtitle("Vegatables consumed per day") 

print(grid.arrange(p1,p2,ncol=2))


frsum_med<- median(dt13[X_frutsum <1000,]$X_frutsum)
sd(dt13[X_frutsum <1000,]$X_frutsum)

vegsum_med<-median(dt13[X_vegesum<1000]$X_vegesum)
sd(dt13[X_vegesum<1000]$X_vegesum)

dt13[, hlthyfood := factor(ifelse( is.na(X_frutsum) | is.na(X_vegesum), NA 
	                   ,ifelse( X_frutsum > frsum_med & X_vegesum > vegsum_med
	                   	,"very good", "poor and average"))) ] 


fruit_med <- median( (sel1_brfss2013 %>% filter(fruit_consum < 10.0))$fruit_consum )
vegetable_med <- median( (sel1_brfss2013 %>% filter(vegetable_consum < 10.0))$vegetable_consum )

sel1_brfss2013 <- sel1_brfss2013  %>%
mutate(healthyfood =  factor(ifelse( is.na(fruit_consum) | is.na(vegetable_consum), NA 
	                   ,ifelse( fruit_consum > fruit_med & vegetable_consum > vegetable_med
	                   	,"very good", "poor and average")))  )



# dt13[, hfood := factor(ifelse( X_frutsum > frsum_med & X_vegesum > vegsum_med
# 	                   	,"very good", "poor and average")) ] 


print (summary(dt13$hlthyfood) ) 
summary(sel1_brfss2013$hlthyfood)



#hist(dt13[physhlth <32 & physhlth>0.99 & hlthyfood %in%c("very good"),]$physhlth)  

#hist(dt13[physhlth <32 & physhlth>0.99 & X_pacat1 %in%c("Highly active", "Active"),]$physhlth)  




fs<-xtabs(~is_sick+is_sport_active, data=dt13)
ffs<-xtabs(~is_sick+is_sport_active, data=sel1_brfss2013)

p_is_sick <- nrow(dt13[is_sick=="yes",]) / nrow(dt13[!is.na(is_sick),])
p_is_sport <- nrow(dt13[is_sport_active=="yes",]) / nrow(dt13[!is.na(is_sport_active),])

p_has_sickdays <- nrow(sel1_brfss2013 %>% filter(is_sick=="yes")) / 
                  nrow(sel1_brfss2013 %>% filter(!is.na(is_sick)))

p_is_sport_active <- nrow(sel1_brfss2013 %>% filter(is_sport_active=="yes")) / 
                     nrow(sel1_brfss2013 %>% filter(!is.na(is_sport_active)))

p_is_sick_and_sport <- fs[2,2] /sum(fs)

p_is_sick_given_sport <- p_is_sick_and_sport / p_is_sport


ff<-xtabs(~is_sick+hlthyfood, data=dt13)

p_is_hlfood <- nrow(dt13[hlthyfood%in%c("very good"),]) / nrow(dt13[!is.na(hlthyfood),])

p_is_hlfood_and_sick <- ff[2,2] / sum(ff)

p_is_sick_given_hlfood <- p_is_hlfood_and_sick / p_is_hlfood 
## goood
print(paste('p is sick', p_is_sick))
print(paste('p is sick given physical activity', p_is_sick_given_sport))
print(paste('p is sick given healthy food', p_is_sick_given_hlfood))



#### for notebook variable names
p_has_sickdays <- nrow(sel1_brfss2013 %>% filter(is_sick=="yes")) /
                  nrow(sel1_brfss2013 %>% filter(!is.na(is_sick)))

p_is_sport_active <- nrow(sel1_brfss2013 %>% filter(is_sport_active=="yes")) / 
                     nrow(sel1_brfss2013 %>% filter(!is.na(is_sport_active)))

ms_sick_vs_sport<-xtabs(~is_sick+is_sport_active, data=sel1_brfss2013)
print(ms_sick_vs_sport)
p_has_sickdays_and_sport <- ms_sick_vs_sport[2,2]/sum(ms_sick_vs_sport)
p_has_sickdays_given_sport_active <- p_has_sickdays_and_sport / p_is_sport_active


p_has_healthyfood <- nrow(sel1_brfss2013 %>% filter(healthyfood %in%c("very good"))) / 
                     nrow(sel1_brfss2013 %>% filter(!is.na(healthyfood)))

ms_sick_vs_healthfood<-xtabs(~is_sick+healthyfood, data=sel1_brfss2013)

p_has_sickdays_and_healthyfood <- ms_sick_vs_healthfood[2,2] / sum(ms_sick_vs_healthfood)

p_has_sickdays_given_healthyfood <- p_has_sickdays_and_healthyfood  / p_has_healthyfood

##final output of probabilities
print(paste('p to have active physical activity', p_is_sport_active))
print(paste('p to consume healthy food', p_has_healthyfood))

print(paste('p has sick days', p_has_sickdays))
print(paste('p has sick days given actived physical activity', p_has_sickdays_given_sport_active))
print(paste('p has sick days given consuming healthy food', p_has_sickdays_given_healthyfood))

#end
