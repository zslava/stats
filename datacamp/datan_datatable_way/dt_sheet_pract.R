library(data.table)
set.seed(45L)

DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3], V3=round(rnorm(4),4), V4=1:12)


DT
##subsetting
DT[3:5,]

DT[V2=="A"]

DT[ V2 %in% c("A","C")]

##selecting subolumns

DT[,.(V2,V3)]

DT[,sum(V1)]

DT[,V1]
diff(DT[,V1])
