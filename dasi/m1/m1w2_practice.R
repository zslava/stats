#rm(list=ls());setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1/");source("m1w2_practice.R")


library(statsr)
library(dplyr)
library(ggplot2)

#load data on nyc plane flights and their delays
data(nycflights)

str(nycflights)

names(nycflights)

###dplyr way . available functions
# filter()
# arrange()
# select()
# distinct()
# mutate()
# summarise()
# sample_n()

##summary statistics functions
# mean
# median
# sd
# var
# IQR
# range
# min
# max

#### first working example
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)

rdu_flights <- nycflights %>%
filter(dest == "RDU")

print(
ggplot(data = rdu_flights, aes(x = dep_delay)) +
  geom_histogram()
)

##summary stats
print(
rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())
)

### quiz 1
sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)

print(nrow(sfo_feb_flights))

print(
ggplot(data = sfo_feb_flights, aes(x = arr_delay)) +
  geom_histogram()
)

print(
sfo_feb_flights %>%
  summarise(mean_ad = mean(arr_delay), sd_dd = sd(arr_delay), n = n())
)


##grouping with dplyr
rdu_flights %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

#quiz 3 (highest iqr  is for DL and UA)
sfo_feb_flights %>%
group_by(carrier) %>%
summarise(arr_med=median(arr_delay), arr_iqr=IQR(arr_delay))


# Which month would you expect to have the highest
# average delay departing from an NYC airport?
# quiz 4.  Which month has the highest average departure delay from an NYC airport? ans July
print(
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))
)
#quiz 5 Which month has the highest median departure delay from an NYC airport? ans December
print(
  nycflights %>%
  group_by(month) %>%
  summarise(med_dd = median(dep_delay)) %>%
  arrange(desc(med_dd))
)
#quiz 6 Is the mean or the median a more reliable measure for deciding which month(s) to avoid flying if you really dislike delayed flights, and why?
#ans median is more reliable as the distribution of delays is skewed
#nice plot of 12 boxplots
  ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

## quiz 7
## adding new column
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

# q 7 If you were selecting an airport simply based on on time departure percentage,
# which NYC airport would you choose to fly out of?
# ans LGA
print(
nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%  # pct of flights "on time" divided by total n of flights 
  arrange(desc(ot_dep_rate))
)
### quiz 8
##  What is the tail number of the plane with the fastest avg_speed?
# ans N526AS
nycflights<- nycflights %>%
  mutate( avg_speed = distance/air_time*60 )

print(
nycflights %>% 
  arrange(desc(avg_speed)) %>%
select(tailnum, avg_speed)
)
### quiz 9
#Make a scatterplot of avg_speed vs. distance.
print(
ggplot(data=nycflights, aes(x=distance, y=avg_speed)) + geom_point(shape=1)
)
#Which of the following is true about the relationship between average speed and distance.
#  q 9 ans C  There is an overall postive association between distance and average speed.

##quiz 10
nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))

## rate of  "on time " general arrivals  ( 0.588)
print(
nycflights %>% summarise(ot_arr_rate = sum(arr_type=="on time") / n()  )
)
#What proportion of flights that were "delayed" departing arrive "on time"?
print(
nycflights %>% 
filter( dep_type == "delayed") %>% 
summarise(ot_arr_rate = sum(arr_type=="on time") / n())
)

#end 