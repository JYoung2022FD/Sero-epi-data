setwd("D:/1-9 cohort/manuscript/reply_wxl/code&data for sharing/R code for sharing-revised/Fig2&3/data")
library(openxlsx) 
library(binom)
library(dplyr)
library(splines)
library(ggplot2)
library(survival)
library(survminer)
##Fig2.A======
seroneg <- read.xlsx("Fig2A.seroneg.xlsx",sheet = 1)
prevd <- read.xlsx("Fig2A.dat_all_seronegative_fit_line0802-43.xlsx",sheet = 1) 
prevd[round(prevd$agemonth,2)==0.00,"p2"]#seronegative prevalence at age of 0 month:0.2649307
prevd[round(prevd$agemonth,2)==6.00,"p2"]#seronegative prevalence at age of 6 months:0.9154838
prevd[round(prevd$agemonth,2)==36.00,"p2"]#seronegative prevalence at age of 36 months:0.6384528
prevd[round(prevd$agemonth,2)==60.00,"p2"]#seronegative prevalence at age of 60 months:0.3438043

p2a<-ggplot()+
  geom_point(data=seroneg,aes(x=TrueAgeVisit*12, y=SeroNeg16mean*100, 
                              color=factor(VisitTime),shape=factor(VisitTime)),position=position_dodge(width =6),size=2)+
  geom_errorbar(data=seroneg,aes(x=TrueAgeVisit*12, ymin=SeroNeg16low*100,ymax=SeroNeg16up*100,
                                 color=factor(VisitTime)),position=position_dodge(width = 6),width=0,size=0.3)+
  scale_x_continuous(breaks=c(seq(0,11,1),seq(12,155,12)),labels=c(0,rep("",11),seq(1,12,1)))+
  scale_y_continuous(breaks=c((0:10)*10),labels=c((0:10)*10))+labs(title="a")+
  geom_line(data=prevd,aes(agemonth,p2*100),color="blue")+
  geom_ribbon(data=prevd,aes(agemonth,ymin=uclp*100,ymax=lclp*100),fill="blue",alpha=0.1)+
  scale_color_manual("",values = c("red","#EE7A00","#002870","maroon1",
                                   "#966b2c","gray40"))+
  scale_shape_manual("",values =c(16,17,15,18,3,4) )+
  labs(x="Age (year)",y="Proportion of susceptible individuals\n\ (%)")+
  geom_segment(aes(x=6,xend=6,y=-5,yend=91.54838),linetype=2)+
  geom_segment(aes(x=-8,xend=6,y=91.54838,yend=91.54838),linetype=2)+
  geom_segment(aes(x=36,xend=36,y=-5,yend=63.84528),linetype=2)+
  geom_segment(aes(x=-8,xend=36,y=63.84528,yend=63.84528),linetype=2)+
  geom_segment(aes(x=60,xend=60,y=-5,yend=34.38043),linetype=2)+
  geom_segment(aes(x=-8,xend=60,y=34.38043,yend=34.38043),linetype=2)+
  theme(legend.key = element_blank() ,
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.text =element_text(size=13),
        panel.background=element_blank(),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(face="bold",size = 16),
        plot.margin = margin(0.2,0.4,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=16))+
  coord_cartesian(expand = F,ylim = c(-5,100),xlim = c(-3,155))+
  geom_line(aes(c(0,155),c(-5,-5)),color="black")+ # x axis
  geom_line(aes(c(-8,-8),c(0,100)),color="black")# y axis

p2a

##Fig2.B======
seroinci <-read.xlsx("Fig2B.seroinci.xlsx",sheet = 1)
seroinci$agegp <- factor(seroinci$agegp,levels = c("0-2m","3-4m","5-6m","7-11m",
                                                   "1y","2y","3y","4y",
                                                   "5y","6y","7y","8y",
                                                   "9y","10y","11y"))
p2b<-ggplot()+
  geom_point(data=seroinci,aes(x=agegp, y=incidmean*100, 
                               color=factor(episeason),shape=factor(episeason)),size=2,
             position = position_dodge(width = 0.5))+
  geom_errorbar(data=seroinci,aes(x=agegp, ymin=incidlow*100,ymax=incidup*100,
                                  color=factor(episeason)),width=0,size=0.3,
                position = position_dodge(width = 0.5))+
  scale_x_discrete(breaks=c("0-2m","3-4m","5-6m","7-11m",
                              "1y","2y","3y","4y",
                              "5y","6y","7y","8y",
                              "9y","10y","11y"),
                     labels = c("0-2m","3-4m","5-6m","7-11m",
                                "1y","2y","3y","4y",
                                "5y","6y","7y","8y",
                                "9y","10y","11y"))+
  scale_y_continuous(breaks=seq(0,7.5,0.5)*10,labels=seq(0,7.5,0.5)*10)+
  labs(title="b")+
  scale_color_manual("Epidemic season",values=c("red","blue","maroon1"),labels=c("2013/14","2014/15","2015/16"))+
  scale_shape_manual("Epidemic season",values =c(16,17,15),labels=c("2013/14","2014/15","2015/16") )+
  labs(x="Age",y="Incidence of EV-A71 infections\n\ (%)")+
  theme(legend.key = element_blank() ,
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.text =element_text(size=13),
        panel.background=element_blank(),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold",size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 11),
        plot.margin = margin(0.2,0.4,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=16))+
  coord_cartesian(expand = F,ylim = c(-3,75),xlim = c(0,16))
p2b

##Fig2.C======
pops_est_ev71 <- read.xlsx("Fig2C.pops_est_ev71.xlsx",sheet = 1)

p2c <- ggplot()+
  geom_bar(data=pops_est_ev71,aes(x=year,weight=incidence_total_ev71*10^5,fill=factor(agec)),color="white",alpha=0.8,
           position = position_dodge2(width =1.2,padding = 0))+
  geom_errorbar(data=pops_est_ev71, aes(x =year, ymin = incidence_total_ev71Lower*10^5, ymax = incidence_total_ev71Upper*10^5
  ),size=0.4,
  position = position_dodge2(width =1.2,padding = 0.8))+
  #geom_bar(data=pops_est_ev71,aes(x=agec,weight=incidence_total_ev71*10^5,fill=factor(year)),position = position_dodge())+  
  scale_fill_manual("Age",labels = c("0-2 months","3-4 months","5-6 months","7-11 months","1-2 years","3-5 years","6-14 years"),
                     values=c("#ff3300","#ff6600","#ffcc66","#66ccff","#8c1aff","#cc33ff","#808080"))+
  scale_y_continuous(breaks = seq(0,900,100),labels = seq(0,900,100))+
  scale_x_continuous(breaks = seq(2012.5,2016.5,0.5),labels =c("","2013","","2014","","2015","","2016",""))+ 
  labs(title="c",y="Incidence of EV-A71 related HFMD\n\ (per 100,000)",x="Year")+
  theme(panel.background = element_blank(),panel.grid = element_blank(),axis.line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.text =element_text(size=10),
        legend.title = element_text(size=10),
        axis.line.x =element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(face="bold",size = 16),
        plot.title = element_text(face="bold",size = 16),
        plot.margin = margin(0.2,0.4,0.1,0.2,"cm"),
        strip.text.x= element_text(size = 14))+
  coord_cartesian(expand = F,ylim = c(0,900),xlim=c(2012.5,2016.5))+
  geom_line(aes(c(2012.5,2016.5),c(-32,-32)),color="black")+ # x axis
  geom_line(aes(c(-Inf,-Inf),c(1,900)),color="black")+# y axis
  geom_segment(aes(x=seq(2012.5,2016.5,1),xend=seq(2012.5,2016.5,1),y=rep(-32,5),yend=rep(-43,5)))

p2c

##combine===========
library(ggpubr)
p <- ggarrange(p2a,p2b,p2c,nrow=3)
p
ggsave('Figure2_revised.pdf',p,width = 9, height =16)
