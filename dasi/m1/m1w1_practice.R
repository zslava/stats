#rm(list=ls());setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1/");source("m1_practice.R")


library(statsr)
library(dplyr)
library(ggplot2)


data(arbuthnot)

str(arbuthnot)

za<-arbuthnot # for the sake of  typing
#qz q1  3 variables

#qz  q2  
za$girls

print( ggplot(data = arbuthnot, aes(x = year, y = girls)) +
  geom_point() )

#qz q3
#option D explaining trends

total<-za$boys + za$girls
za<-cbind(za,total)

#dplyer way 
# %>% is a piping operator

arbuthnot <- arbuthnot %>%
  mutate(total = boys + girls)
str(arbuthnot)

print(
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_line() +  geom_point()
) 

print(
ggplot(data = arbuthnot, aes(x = year, y = boys/total)) +
  geom_line() +  geom_point()
) 
#add logical column
arbuthnot <- arbuthnot %>%
  mutate(more_boys = boys > girls)

#exercise generate a plot of boys proportion

print(
ggplot(data = arbuthnot, aes(x = year, y = boys/total)) +
  geom_line() +  geom_point()
) 

data(present)

#qz q4 
# ans 3 variables
range(present$year)

present<- present %>%
  mutate(total = boys + girls)

present<- present %>%
  mutate(prop_boys = boys/total)

str(present)


print(
ggplot(data = present, aes(x = year, y = prop_boys)) +
  geom_line() +  geom_point()
) 
#qz q 5 opt A 


present <- present %>%
  mutate(more_boys = boys > girls)
present$more_boys
#qz q 6 opt B

present <- present %>%
  mutate(prop_boy_girl = boys / girls)
present$more_boys

plot(prop_boy_girl~year, data=present,type="b")
#qz q 7 opt 4


require(data.table)

tdp <- data.table(present)
tail(tdp[order(total)])
#qz q 8  opt E 