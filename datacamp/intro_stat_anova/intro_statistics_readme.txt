author: andrew conway  clarement graduate univeristy
name:  intro tos tatistics with R 


1. Variables
4 types :
	nominal  (== !=)
	ordinal ( == <, >)
	Interval (distance) {by how much}
	Ratio (also have a true zero { }
)

from nominal to ratio  more answers from variables can be derived


2. Historgrams & distributions

histogram and distribution shows a frequency of occurence of data values grouped in beans

normal distribution ==  bell curve , 1 peak

2 peaks == bimodal distribution

Not all distirubtion are normal

negatively skewed  distrib == left tail is bigger than right tale 
positively skewed  distrib  = right tail is bigger than left tale



2. t-test 
 independent
 dependent (paired)

 3. VAriance

ANOVA = analysis of variance
necessary to conduct for 3+ groups

F test 
F =  variance_between_groups  /
	 variance within groups  

F = ( systematic variance / 
      unsystematic variance)

F distribution
df(x,df1, df2)  # df1,df2  degrees of freedom


F = MS_A / MS_{S/A}

Post-hoc tests
def. allow for multiple pairwise comparisons without an incarease in the probability
# of type I error

Tukey's procedure

* most liberal = no adjustment
* most conservative = adjust for every possible comparison 
   that COULD be made

inflation of type 1 error
p=0.05 of making type 1 error i 1 groupwise comparison
if 4 groups, there are 6 independent groupwise comparisons
so p=1-0.95^6 = 0.26 !! => error type 1 inflation

Bonferroni approach more conservative: 
test N  hypothesiss at level alpha/n 

use of pairwise.t.test(independent_var, dependent_var, p.adjust.method="bonferroni", paired=FALSE)

