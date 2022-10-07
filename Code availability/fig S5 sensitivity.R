setwd("D:/1-9 cohort/manuscript/reply_wxl/code&data for sharing/data")
library(openxlsx) 
library(binom)
library(dplyr)
library(splines)
library(ggplot2)
library(survival)
library(survminer)

#fit gp dividing by 128=============
nm <- read.xlsx("nm_final.xlsx",sheet = 1)
#calculate proportion
length(unique(nm$kid))
table(nm$gp)#128
prop.table(table(nm$gp))#369 (21.68%)

table(nm$gp1)#64
prop.table(table(nm$gp1))#119 (6.99%)

table(nm$gp2)#32
prop.table(table(nm$gp2))#98 (5.76%)

table(nm$gp3)#256
prop.table(table(nm$gp3))#435 (25.56%)

nms <- nm[,c("status","left","right","gp")]
nms$left <- nms$left/12
nms$right<- nms$right/12

nms_all<-nms
nms_all$gp<-0

nms_final<-rbind(nms,nms_all)
nms_final$gp <- factor(nms_final$gp,levels = c(0,1,2))

km <- survfit(Surv(time=left,time2=right,type = "interval2")~gp,data=nms_final)

s0 <- ggsurvplot (km,risk.table = "absolute",fun = "event",
                  title="B",break.x.by=1,size = 0.7,xlim=c(0,14),#ylim=c(0,0.3), break.y.by=0.05,
                  ylab="Probability (%)", xlab="Age (year)",
                  conf.int = FALSE,tables.y.text=F,
                  legend.title="",
                  risk.table.height=0.15,
                  ggtheme = theme(legend.key = element_blank() ,
                                  legend.position = c(1,0.8),
                                  legend.direction = "horizontal",
                                  legend.text =element_text(size=13),
                                  legend.title  =element_text(size=13),
                                  panel.background=element_blank(),panel.border=element_blank(),
                                  axis.line = element_line(colour ="black"),
                                  axis.text = element_text(size = 14),
                                  axis.title = element_text(face="bold",size = 16),
                                  plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
                                  plot.title = element_text(hjust = 0,face="bold",size=16)))
p3c <- s0$plot+scale_y_continuous(limits = c(0,0.6),breaks = seq(0,0.6,by=0.05), labels = seq(0,60,by=5))+
  scale_color_manual(labels = c("All","Initial titerâ‰?128","Initial titer<128"),  ##change legend title
                     values=c("#002870","red","blue"))+
  coord_cartesian(expand = F,xlim = c(0,12.5),ylim = c(0,0.6))+
  theme(legend.position = c(0.5, 0.98))
p3c


#fit gp dividing by 64==========
nms <- nm[,c("status","left","right","gp1")]
nms$left <- nms$left/12
nms$right<- nms$right/12

nms_all<-nms
nms_all$gp1<-0

nms_final<-rbind(nms,nms_all)
nms_final$gp1 <- factor(nms_final$gp1,levels = c(0,1,2))
km1 <- survfit(Surv(time=left,time2=right,type = "interval2")~gp1,data=nms_final) #think of each observation as a time interval with (-infinity, t) for left censored, (t, infinity) for right censored, (t,t) for exact and (t1, t2) for an interval. This is the approach used for type = interval2
summary(km1,c(11))
1-c(0.3538,0.4667,0.6156 )#53% (95%CI: 38%, 65%)
##stratified by group
s1 <- ggsurvplot (km1,risk.table = "absolute",fun = "event",
                  title="C",break.x.by=1,size = 0.7,xlim=c(0,14),#ylim=c(0,0.3), break.y.by=0.05,
                  ylab="Probability (%)", xlab="Age (year)",
                  conf.int = FALSE,tables.y.text=F,
                  legend.title="",
                  risk.table.height=0.15,
                  ggtheme = theme(legend.key = element_blank() ,
                                  legend.position = c(1,0.8),
                                  legend.direction = "horizontal",
                                  legend.text =element_text(size=13),
                                  legend.title  =element_text(size=13),
                                  panel.background=element_blank(),panel.border=element_blank(),
                                  axis.line = element_line(colour ="black"),
                                  axis.text = element_text(size = 14),
                                  axis.title = element_text(face="bold",size = 16),
                                  plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
                                  plot.title = element_text(hjust = 0,face="bold",size=16)))
s1
fs1 <- s1$plot+scale_y_continuous(limits = c(0,0.6),breaks = seq(0,0.6,by=0.05), labels = seq(0,60,by=5))+
  scale_color_manual(labels = c("All","Initial titerâ‰?64","Initial titer<64"),
                     values=c("#002870","red","blue"))+
  coord_cartesian(expand = F,xlim = c(0,12.5),ylim = c(0,0.6))+
  theme(legend.position = c(0.5, 0.98))

fs1

#fit gp dividing by 32==========
nms <- nm[,c("status","left","right","gp2")]
nms$left <- nms$left/12
nms$right<- nms$right/12

nms_all<-nms
nms_all$gp2<-0

nms_final<-rbind(nms,nms_all)
nms_final$gp2 <- factor(nms_final$gp2,levels = c(0,1,2))
km2 <- survfit(Surv(time=left,time2=right,type = "interval2")~gp2,data=nms_final) #think of each observation as a time interval with (-infinity, t) for left censored, (t, infinity) for right censored, (t,t) for exact and (t1, t2) for an interval. This is the approach used for type = interval2
##stratified by group
s2 <- ggsurvplot (km2,risk.table = "absolute",fun = "event",
                  title="D",break.x.by=1,size = 0.7,xlim=c(0,14),#ylim=c(0,0.3), break.y.by=0.05,
                  ylab="Probability (%)", xlab="Age (year)",
                  conf.int = FALSE,tables.y.text=F,
                  legend.title="",
                  risk.table.height=0.15,
                  ggtheme = theme(legend.key = element_blank() ,
                                  legend.position = c(1,0.8),
                                  legend.direction = "horizontal",
                                  legend.text =element_text(size=13),
                                  legend.title  =element_text(size=13),
                                  panel.background=element_blank(),panel.border=element_blank(),
                                  axis.line = element_line(colour ="black"),
                                  axis.text = element_text(size = 14),
                                  axis.title = element_text(face="bold",size = 16),
                                  plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
                                  plot.title = element_text(hjust = 0,face="bold",size=16)))
s2
fs2 <- s2$plot+scale_y_continuous(limits = c(0,0.6),breaks = seq(0,0.6,by=0.05), labels = seq(0,60,by=5))+
  scale_color_manual(labels = c("All","Initial titerâ‰?32","Initial titer<32"),
                     values=c("#002870","red","blue"))+
  coord_cartesian(expand = F,xlim = c(0,12.5),ylim = c(0,0.6))+
  theme(legend.position = c(0.5, 0.98))

fs2

#fit gp dividing by 256==========
nms <- nm[,c("status","left","right","gp3")]
nms$left <- nms$left/12
nms$right<- nms$right/12

nms_all<-nms
nms_all$gp3<-0

nms_final<-rbind(nms,nms_all)
nms_final$gp3 <- factor(nms_final$gp3,levels = c(0,1,2))
km3 <- survfit(Surv(time=left,time2=right,type = "interval2")~gp3,data=nms_final) #think of each observation as a time interval with (-infinity, t) for left censored, (t, infinity) for right censored, (t,t) for exact and (t1, t2) for an interval. This is the approach used for type = interval2
##stratified by group
s3 <- ggsurvplot (km3,risk.table = "absolute",fun = "event",
                  title="A",break.x.by=1,size = 0.7,xlim=c(0,14),#ylim=c(0,0.3), break.y.by=0.05,
                  ylab="Probability (%)", xlab="Age (year)",
                  conf.int = FALSE,tables.y.text=F,
                  legend.title="",
                  risk.table.height=0.15,
                  ggtheme = theme(legend.key = element_blank() ,
                                  legend.position = c(1,0.8),
                                  legend.direction = "horizontal",
                                  legend.text =element_text(size=13),
                                  legend.title  =element_text(size=13),
                                  panel.background=element_blank(),panel.border=element_blank(),
                                  axis.line = element_line(colour ="black"),
                                  axis.text = element_text(size = 14),
                                  axis.title = element_text(face="bold",size = 16),
                                  plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
                                  plot.title = element_text(hjust = 0,face="bold",size=16)))
s3
fs3 <- s3$plot+scale_y_continuous(limits = c(0,0.6),breaks = seq(0,0.6,by=0.05), labels = seq(0,60,by=5))+
  scale_color_manual(labels = c("All","Initial titerâ‰?256","Initial titer<256"),
                     values=c("#002870","red","blue"))+
  coord_cartesian(expand = F,xlim = c(0,12.5),ylim = c(0,0.6))+
  theme(legend.position = c(0.5, 0.98))

fs3
fsp <- ggarrange(fs3,p3c,fs1,fs2,nrow = 1,ncol=4)
fsp
ggsave('Fig_surv_gp0-3.pdf',fsp,width = 24, height =6, limitsize = FALSE) ##symbol>= recognizable for tiff
