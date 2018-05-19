#rm(list=ls());setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1_prj/");source("intra_data_prob_practice_bis.R")

#setwd("c:/Users/zimine/Dropbox/cs/bigdata/coursera/dasi/m1_prj/");source("intra_data_prob_practice_bis.R")

library(statsr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)

#library(openintro)


##load data
#print('not reloading data')
load("brfss2013.Rdata")

dt13<-data.table(brfss2013)

##exploring

#### question 2
#q if  college educated people in good health with lower income
#are more likely to make an economic decision to  forgo a health insurance
# compared to people with 
#high school education from the same income category

#plan 
# 1. good health check  levels(ds$genhlth)  or levels(ds$X_rfhlth)
# 2. level of education computed  levels(ds$X_educag)
# 3. income category levels(ds$X_incomg)
# 4. has health coverage levels(ds$hlthpln1)


#0 select variables to work on
sel_dt_brs <- dt13[,.(genhlth,X_rfhlth,X_educag,X_incomg,hlthpln1)]

sel_brfss2013 <- brfss2013 %>% select (genhlth,X_rfhlth,X_educag,X_incomg,hlthpln1)


#-1 motivating chart

#plot(sel_brfss2013$hlthpln1~dt13$genhlth)


# 0
#create a health plan variable with clear name

sel_dt_brs[, has_plan := hlthpln1]
sel_brfss2013 <- sel_brfss2013  %>% mutate(has_plan =hlthpln1 )
# 1.  create a health variable 
	  # will use d X_rfhlth


sel_dt_brs[, g_health := factor(ifelse(is.na(genhlth),NA
							   ,ifelse(genhlth %in% c("Excellent", "Very good", "Good")
					 , "excellent or good", "poor or fair")))  ]


#print( summary(sel_dt_brs$genhlth) )

#print( summary(sel_dt_brs$g_health) )


sel_brfss2013 <- sel_brfss2013  %>%
mutate(g_health =factor(ifelse(is.na(genhlth), NA
					   ,ifelse(genhlth %in% c("Excellent", "Very good", "Good"),
						"excellent or good", "poor or fair"))) )


#print( summary(sel_brfss2013$g_health) )

#2. create a variable on college vs other level
sel_dt_brs[, edu_cat := factor( ifelse( is.na(X_educag),NA
								,ifelse(X_educag %in% c("Attended college or technical school"
													   ,"Graduated from college or technical school")
										,"college education","pre-college education")))  ]

sel_brfss2013 <- sel_brfss2013  %>%
mutate(edu_cat = factor( ifelse( is.na(X_educag),NA
								,ifelse(X_educag %in% c("Attended college or technical school"
													   ,"Graduated from college or technical school")
										,"college education","pre-college education"))) )

#3 create a variable on income category  low middle upper
sel_dt_brs[, incom_cat :=  factor( ifelse(is.na(X_incomg),NA
								   ,ifelse(X_incomg %in% c("Less than $15,000"
														  ,"$15,000 to less than $25,000")
								   ,"low",ifelse(X_incomg %in% c("$25,000 to less than $35,000"
																,"$35,000 to less than $50,000")
								   ,"middle","upper")))) ]


sel_brfss2013 <- sel_brfss2013  %>%
mutate(incom_cat = factor( ifelse(is.na(X_incomg),NA
								   ,ifelse(X_incomg %in% c("Less than $15,000"
														  ,"$15,000 to less than $25,000")
								   ,"low",ifelse(X_incomg %in% c("$25,000 to less than $35,000"
																,"$35,000 to less than $50,000")
								   ,"middle","upper")))) )





#2 college edu flag
#sds[, edu_uni := factor(ifelse(X_educag %in% c("Did not graduate high school","Graduated high school")
#                        ,"pre-colledge","college"))  ]



sel_dt_brs[!is.na(edu_cat) & g_health =="excellent or good"  & incom_cat %in%c("low","middle") & has_plan == "No"  
		  , .(count=.N), by=.(has_plan,g_health,edu_cat,incom_cat)]


## show summary statistics 
sel_brfss2013  %>%
filter( !is.na(edu_cat) 
		& g_health =="excellent or good"  
		& incom_cat %in%c("low") 
		& has_plan == "No" ) %>%
group_by(has_plan,g_health,edu_cat,incom_cat) %>%
summarize(count=n() ) %>% 
arrange(incom_cat)


xtabs(~edu_cat+incom_cat+has_plan, 
	data=sel_dt_brs[!is.na(edu_cat) & g_health =="excellent or good"  & incom_cat %in%c("low","middle", "upper") & has_plan == "No",])


xtabs(~edu_cat+incom_cat
	  ,data=sel_brfss2013 %>% filter(!is.na(edu_cat) 
								   & g_health =="excellent or good" 
								   & !is.na(incom_cat)
								   & has_plan == "No") )




#### question 3
#Q is physical exercise compensate health damage cause by alcohol
sel1_dt_brs <- dt13[,.(genhlth, X_drnkmo4,X_pacat1)]

#motivating chart
#plot(X_pacat1~genhlth, data=sel1_dt_brs)

sel1_dt_brs[, drinker:=factor(ifelse(is.na(X_drnkmo4),NA
							 ,ifelse(X_drnkmo4<=10,"rare"
                             ,ifelse(X_drnkmo4 >10 & X_drnkmo4<=20,"occasional"
                             ,ifelse(X_drnkmo4>20 & X_drnkmo4<=60,"regular"
                             ,ifelse(X_drnkmo4>60 & X_drnkmo4 <1000,"heavy",NA))))))  ]

sel1_dt_brs[, g_health := factor(ifelse(is.na(genhlth),NA
							   ,ifelse(genhlth %in% c("Excellent", "Very good", "Good")
					 , "excellent or good", "poor or fair")))  ]

sel1_dt_brs[!is.na(drinker)&!is.na(g_health),.(cnt=.N), by=.(g_health,drinker)][order(drinker,g_health)]

sel1_dt_brs[drinker=="heavy", (cnt=.N), by =.(g_health, X_pacat1) ]

sel1_dt_brs[drinker=="heavy" &!is.na(g_health) & !is.na(X_pacat1)
           ,.(cnt=.N), by =.(g_health, X_pacat1) ][order(X_pacat1)]

xtabs(~g_health+X_pacat1, data=sel1_dt_brs[drinker=="heavy"])

totsubj <- sel1_dt_brs[!is.na(drinker) &!is.na(g_health) & !is.na(X_pacat1), .(cnt=.N), ]

#proba g_health %in% "poor or fair"
pct_g_health_poor_fair <- nrow(sel1_dt_brs[g_health %in% ("poor or fair")]) /
                          nrow(sel1_dt_brs[!is.na(g_health)])
#proba drinker                           
pct_drinker <- nrow( sel1_dt_brs[drinker %in% c("heavy", "regular"),]) /
              nrow( sel1_dt_brs[!is.na(drinker),])


pct_drinker_hlt_poor <- nrow(sel1_dt_brs[ g_health %in% ("poor or fair") 
	                         & drinker %in% c("heavy", "regular") , ]) /
	                    nrow(sel1_dt_brs[!is.na(g_health) & !is.na(drinker),] )

pct_hlt_poor_given_drinker  <- pct_drinker_hlt_poor / pct_drinker

###- > compare pct_hlt_poor_given_drinker  and pct_g_health_poor_fair (unconclusive)

#next checks  physhlth and poorhlth >1 show folks with health problems
# hist(dt13[poorhlth < 365 & poorhlth > 0.99,]$poorhlth)
# ! hist(dt13[physhlth < 32 & poorhlth > 0.99,]$physhlth)
# check which factors afffect those numbers

##dependency between out of work and doing exercises vs health food
#hist(dt13[physhlth < 32 & poorhlth > 0.99 & X_pacat1 %in%c("Highly active", "Active"),]$physhlth)


#tt

## redo with dplyr



#end
