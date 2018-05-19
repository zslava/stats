#rm(list=ls());setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1/");source("m1w3_practice.R")


library(statsr)
library(dplyr)
library(ggplot2)
library(data.table)
#This data frame contains 133 observations and 6 variables,
# where every row records a shot taken by Kobe Bryant.
data(kobe_basket)

kb <- data.table(kobe_basket)

kobe_streak <- calc_streak(kobe_basket$shot)

print(
ggplot(data = kobe_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)
)

coin_outcomes <- c("heads", "tails")
print(sample(coin_outcomes, size = 1, replace = TRUE)) #draws th number of size

sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)
print( table(sim_fair_coin))

sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, 
                          prob = c(0.2, 0.8))
print(table(sim_unfair_coin))


### simulate Kobe  his p(H) = 0.45
shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))

sim_streak <- calc_streak(sim_basket)


print(
ggplot(data = sim_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)
)


##end