#######################################################################
##             A.times serials of EV-A71 Anhua                     ####
#######################################################################
library(readxl)
library(ggplot2)
library(plyr)
library(grid)

##1.lable x axis ####

##1.1 compute consecutive weeks from 2013/9/1 to 2018/8/31, with Monday as 1st day for a week (used for tick week and month in x axis##
s<-as.Date("2013-01-01")
s1<-as.Date("2012-12-31")
e<-as.Date("2016-12-31")
d<-seq(from=s,to=e,by=1)
label.name<-data.frame(d)
p<-as.POSIXlt(d)
p.s<-as.POSIXlt(s1)

label.name$day<-(1:length(d))
label.name$month<-p$mon+1 ##计算每年自然???, p$month start from 0, so plus 1
label.name$week<-as.numeric(strftime(p,format="%W"))+1    ##计算每年自然周次
label.name$week.serial<-as.numeric(floor((label.name$d-s1)/7)+1)   ##计算多年累积周次

label.name$d.string<-as.character(label.name$d)
label.name$month.start.day<-substr(label.name$d.string,9,10)
label.name.sub<-label.name[which(label.name$month.start.day=="01"),]

##1.2 tick month
tick.month<-as.numeric(c(label.name.sub$week.serial)) ##tick the first day of each month

##1.3 label months at x axis
month.name<-c(rep(c("Jan","","Mar","","May","","Jul","","Sep","","Nov",""),4))

##2.ggplot plot: start from 2013.09: No.positive of EV-A71####
virological_EV_A71<-read.csv("virological_EV_A71.csv")
tick.week<-as.numeric(virological_EV_A71$week.serial)  ##tick week
windows(width=10,height=4)
ggplot(virological_EV_A71, aes(x=virological_EV_A71$week.serial, y=NumberEV71PositivebyWeek))+
  theme_bw()+
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2013-09-23")],xmax=label.name$week.serial[which(label.name$d=="2013-11-25")],ymin=0,ymax=30),alpha=0.03,fill="#DFDFDF")+  ##lower alpha, higher transperency
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2014-02-25")],xmax=label.name$week.serial[which(label.name$d=="2014-03-11")],ymin=0,ymax=30),alpha=0.01,fill="#C9AAAA")+
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2014-08-25")],xmax=label.name$week.serial[which(label.name$d=="2014-10-22")],ymin=0,ymax=30),alpha=0.03,fill="#DFDFDF")+
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2015-03-04")],xmax=label.name$week.serial[which(label.name$d=="2015-03-31")],ymin=0,ymax=30),alpha=0.01,fill="#C9AAAA")+
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2015-08-26")],xmax=label.name$week.serial[which(label.name$d=="2015-10-14")],ymin=0,ymax=30),alpha=0.03,fill="#DFDFDF")+
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2016-03-05")],xmax=label.name$week.serial[which(label.name$d=="2016-03-27")],ymin=0,ymax=30),alpha=0.01,fill="#C9AAAA")+
  geom_rect(aes(xmin=label.name$week.serial[which(label.name$d=="2016-08-18")],xmax=label.name$week.serial[which(label.name$d=="2016-11-13")],ymin=0,ymax=30),alpha=0.03,fill="#DFDFDF")+
  geom_bar(stat="identity", fill="#FC0D1B")+  ##repeat bar chart to avoid shadow cover the bar 
  scale_x_continuous(name = "2013                                                  2014                                                            2015                                                            2016                      \n\n Week of specimens collected",limits=c(32,209),expand=c(0,0), breaks=tick.month,labels =month.name)+ ##X limit from 36, represent the surveillance start from sep. 2013
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10),name="Number of EV-A71 positive specimens",expand=c(0,0))+
  theme(panel.background=element_blank(),panel.grid =element_blank(),axis.text.y=element_text(size=9),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold",size = 10),
        axis.title.x = element_text(vjust = -3.6),
        plot.margin = margin(0.2,0.2,0.8,0.2,"cm"))+
  annotate(geom="segment",y=0,yend=0.2,x=tick.week[32:209],xend=tick.week[32:209]) ##add minor ticks for weeks

ggsave("EV-A71 surveillance_No.EV71.png",width=10,height=4,dpi=600)
