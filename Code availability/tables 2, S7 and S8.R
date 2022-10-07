#######################################################################
##             A.Table 2& Table S7.sero-prevalence                 ####
#######################################################################
##1. Cohort estimates_stratified by season####
seropos<-read.csv("Seroprevalence-count.csv")

library(binom)
CI8<-binom.confint(seropos$NumberSeroPos8,seropos$NumberCase,methods="exact")
seropos$SeroPos8mean<-CI8$mean
seropos$SeroPos8low<-CI8$lower
seropos$SeroPos8up<-CI8$upper

CI16<-binom.confint(seropos$NumberSeroPos16,seropos$NumberCase,methods="exact")
seropos$SeroPos16mean<-CI16$mean
seropos$SeroPos16low<-CI16$lower
seropos$SeroPos16up<-CI16$upper

CI32<-binom.confint(seropos$NumberSeroPos32,seropos$NumberCase,methods="exact")
seropos$SeroPos32mean<-CI32$mean
seropos$SeroPos32low<-CI32$lower
seropos$SeroPos32up<-CI32$upper

write.csv(seropos,"Seroprevalence-by season-annual visit.csv",row.names = F)
##2. seroprevalence comparison for different cutoffs####
library(readxl)
data<-read_xlsx("seroprevalence comparison.xlsx",sheet=1)
library(MASS)

##2013
##three defition comparison
data1<-data[which(data$year=="2013"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 16
data0<-data[which(data$year=="2013"),c(4,5)]
data1<-data0[c(1,2),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 32
data0<-data[which(data$year=="2013"),c(4,5)]
data1<-data0[c(1,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##16 vs. 32
data0<-data[which(data$year=="2013"),c(4,5)]
data1<-data0[c(2,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))


##2014
##three defition comparison
data1<-data[which(data$year=="2014"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 16
data0<-data[which(data$year=="2014"),c(4,5)]
data1<-data0[c(1,2),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 32
data0<-data[which(data$year=="2014"),c(4,5)]
data1<-data0[c(1,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##16 vs. 32
data0<-data[which(data$year=="2014"),c(4,5)]
data1<-data0[c(2,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##2015
##three defition comparison
data1<-data[which(data$year=="2015"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 16
data0<-data[which(data$year=="2015"),c(4,5)]
data1<-data0[c(1,2),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 32
data0<-data[which(data$year=="2015"),c(4,5)]
data1<-data0[c(1,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##16 vs. 32
data0<-data[which(data$year=="2015"),c(4,5)]
data1<-data0[c(2,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##2016
##three defition comparison
data1<-data[which(data$year=="2016"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 16
data0<-data[which(data$year=="2016"),c(4,5)]
data1<-data0[c(1,2),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##8 vs. 32
data0<-data[which(data$year=="2016"),c(4,5)]
data1<-data0[c(1,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##16 vs. 32
data0<-data[which(data$year=="2016"),c(4,5)]
data1<-data0[c(2,3),]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))


#######################################################################
##                B.Table 2& Table S8.seroincidence                ####
#######################################################################

##1. Cohort estimates_stratified by season####
seroinci<-read.csv("Seroincidence-count.csv")

library(binom)
##incidence-denominator:total participants
CI8<-binom.confint(seroinci$NumberInfec8,seroinci$NumberCase,methods="exact")
seroinci$seroinci.totalcase8mean<-CI8$mean
seroinci$seroinci.totalcase8low<-CI8$lower
seroinci$seroinci.totalcase8up<-CI8$upper

CI16<-binom.confint(seroinci$NumberInfec16,seroinci$NumberCase,methods="exact")
seroinci$seroinci.totalcase16mean<-CI16$mean
seroinci$seroinci.totalcase16low<-CI16$lower
seroinci$seroinci.totalcase16up<-CI16$upper

CI32<-binom.confint(seroinci$NumberInfec32,seroinci$NumberCase,methods="exact")
seroinci$seroinci.totalcase32mean<-CI32$mean
seroinci$seroinci.totalcase32low<-CI32$lower
seroinci$seroinci.totalcase32up<-CI32$upper
write.csv(seroinci,"Seroincidence by season-01152019.csv",row.names = F)

##2 seroincidence comparison for different cutoffs of seroconversion####
library(readxl)
data<-read_xlsx("seroincidence comparison.xlsx",sheet=2)
library(MASS)

##season 1
##three cutoff comparison
data1<-data[which(data$year=="season1"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##season 2
##three cutoff comparison
data1<-data[which(data$year=="season2"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

##season 3
##three cutoff comparison
data1<-data[which(data$year=="season3"),c(4,5)]
data.chi<-as.matrix(data1)
print(chisq.test(data.chi))

#######################################################################
##          C.Table 2.seroprevalence extrapolated to Anhua         ####
#######################################################################
data<-read.csv("Seroprevalence-by age and season-annual visit.csv")
library(epitools) ##epitools package used to figure out CI of extraploated sero-prevalence

##4.1 Extrapolate to all age
data1<-data[which(data$VisitTime==1),]
result_16_2013<-ageadjust.direct(count=data1$NumberSeroPos16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

data1<-data[which(data$VisitTime==3),]
result_16_2014<-ageadjust.direct(count=data1$NumberSeroPos16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

data1<-data[which(data$VisitTime==5),]
result_16_2015<-ageadjust.direct(count=data1$NumberSeroPos16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

data1<-data[which(data$VisitTime==7),]
result_16_2016<-ageadjust.direct(count=data1$NumberSeroPos16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

seropre.adjust<-as.data.frame(rbind(result_16_2013,result_16_2014,result_16_2015,result_16_2016),row.names = F)[,(2:4)]

write.csv(seropre.adjust,"seroprevalence_extrapolate to Anhua.csv",row.names=F)

#######################################################################
##         D.Table 2.seroincidence extrapolated to Anhua            ####
#######################################################################
data<-read.csv("Seroincidence by age and season.csv")
library(epitools) ##epitools package used to figure out CI of extraploated sero-prevalence
data1<-data[which(data$VisitTime=="season1valid"),]
result_16_2013<-ageadjust.direct(count=data1$NumberInfec16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

data1<-data[which(data$VisitTime=="season2valid"),]
result_16_2014<-ageadjust.direct(count=data1$NumberInfec16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

data1<-data[which(data$VisitTime=="season3valid"),]
result_16_2015<-ageadjust.direct(count=data1$NumberInfec16,pop=data1$NumberCase,stdpop=data1$AnhuaPopSize, conf.level = 0.95)

seroinci.adjust<-as.data.frame(rbind(result_16_2013,result_16_2014,result_16_2015),row.names = F)[,(2:4)]

write.csv(seroinci.adjust,"seroincidence_extrapolate to Anhua.csv",row.names=F)
