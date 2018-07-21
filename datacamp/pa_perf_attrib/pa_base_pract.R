#rm(list=ls()); setwd("~/Dropbox/cs/bigdata/datacamp/pa_perf_attrib");source("pa_base_pract.R", echo=T)

require(pa)

## pa contains data from msci barra global equity model II (GEM2) contains 3000 security
data(year)
names(year)
#barriid  barra id for security, name : name of security, 

#this is how find  12 monthly vlaues of unique company
year[year$barrid=='USAQGY1',]

data(jan)

##portfolio: top 200 securities based on value feature
# value feature caputes the extent to which a stock is priced indexpensively in market
# e.g. low P/E
##portfolio is equal-weighted w = 0.005

p <- which (jan$portfolio > 0)
str(p)

## benchmarkhas non-zero benchmark for top 1000 companies based on size feature
# size: differentiate between large and small cap companies
# the benchmark is cap-weighted

bb<-which(jan$benchmark>0) #index of securites with non-zero benchmark


print("####### Brinson single period attribution #######")
print("####### Brinson treats active return as arithmetic return ###")
print("##################################################")
##brinson model over a single period (january)
br.single <- brinson(x = jan, date.var = "date", cat.var = "sector",
                     bench.weight = "benchmark", portfolio.weight = "portfolio",
                     ret.var = "return")

summary(br.single)                     

plot(br.single, var="sector",type="return")
##these are actual columns needed for brinson function
bjan <- data.frame(jan$barrid, jan$date, jan$sector,
                   jan$benchmark, jan$portfolio, jan$return)
colnames(bjan) <- c("barrid", "date", "sector", "benchmark", "portfolio", "return")
bbr.single <- brinson(x = bjan, date.var = "date", cat.var = "sector",
                     bench.weight = "benchmark", portfolio.weight = "portfolio",
                     ret.var = "return")

#summary(bbr.single)
#plot(bbr.single, var="sector",type="return")


dj <- data.table(bjan)
active_ret <- dj[,sum(portfolio*return)] - dj[,sum(benchmark*return)]
active_ret#it corresonds to the active return summary(bbr.single)


##example (modified) with data from wikipedia
## see the excel file wikiped_pa_ex.xlsx in the same folder
dw <- read.csv(file="wikiped_pa_ex.csv",header=TRUE)
dw$date <- as.Date(dw$date)
brw.single <- brinson(x = dw, date.var = "date", cat.var = "sector",
                      bench.weight = "benchmark", portfolio.weight = "portfolio",
                      ret.var = "return")
#summary(brw.single)

print("####### Brinson multi period attribution #######")
print("####### R_activ_arithm = sum(R_actif) #######")
print("####### R_B_geom = prod(1+R_b) - 1 #######")
print("####### R_P_geom = prod(1+R_p) - 1 #######")
print("####### R_activ_geom = R_P_geom - R_B_geom #######")

print("##################################################")

data(quarter)
br.multi <- brinson(quarter, date.var = "date",
                    cat.var = "sector",
                    bench.weight = "benchmark",
                    portfolio.weight = "portfolio",
                    ret.var = "return")

#summary(br.multi)

#exposure method on the class br.multi object shows the exposure of the
#portfolio and the benchmark based on a user-defined category 
#e.g. here using feature size
print(exposure(br.multi, var = "size"))


#to see the returns  over multiple months + aggregated
# linking method is specified to used linking adjustment coefficients
print(returns(br.multi, type = "linking"))

plot(br.multi, type = "return")


print("####### single period regression  #######")
print("####### TODO: #######")