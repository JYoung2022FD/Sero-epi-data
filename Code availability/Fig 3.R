setwd("D:/1-9 cohort/manuscript/reply_wxl/code&data for sharing/R code for sharing-revised/Fig2&3/data")
library(openxlsx) 
library(binom)
library(dplyr)
library(splines)
library(ggplot2)
library(survival)
library(survminer)
##Fig3.A======
data_gmt <- read.xlsx("Fig3A.data_gmt.xlsx",sheet = 1)
traj <- read.xlsx("Fig3A.dat_all_fit_line0727.xlsx",sheet=1)
traj[which(round(traj$agemonth,2)==36.00),]#4.697579
traj[which(round(traj$agemonth,2)==60.00),]#5.727817
p3a<-ggplot()+
  geom_point(data=data_gmt,aes(x=agey, y=gmt, 
                               color=factor(year),shape=factor(year)),position=position_dodge(width = 0.5),size=2)+
  geom_errorbar(data=data_gmt,aes(x=agey, ymin=gmtlw,ymax=gmtup,
                                  color=factor(year)),position=position_dodge(width = 0.5),width=0,size=0.3)+
  geom_line(data=traj,aes(agemonth/12,p),colour="blue")+
  geom_ribbon(data=traj,aes(agemonth/12,ymin=p-1.96*se,ymax=p+1.96*se),fill="blue",alpha=0.1)+
  scale_color_manual("",values = c("red","#EE7A00","#002870","maroon1",
                                   "#966b2c","gray40"))+
  scale_shape_manual("",values =c(16,17,15,18,3,4) )+
  labs(x="Age (year)",y="GMT")+# continuous Age
  scale_x_continuous(breaks=c(0:12),labels=c(0:12))+
  scale_y_continuous(breaks=c(2:8),labels=c(4,8,16,32,64,128,256))+labs(title="a")+
  theme(legend.key = element_blank(),
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.text =element_text(size=13),
        panel.background=element_blank(),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(face="bold",size = 16),
        plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=16))+
  coord_cartesian(expand = F,xlim = c(-0.3,13),ylim=c(2,8))
p3a

##Fig3.B======
data_gmt_p <- read.xlsx("Fig3B.data_gmt_aftinfec.xlsx",sheet = 1)
traj_p <- read.xlsx("Fig3B.dat_aftinfec_0726_fit line.xlsx",sheet=1)
traj_p[which(round(traj_p$agemonth,2)==36.00),]#8.623576
traj_p[which(round(traj_p$agemonth,2)==60.00),]#8.148861

p3b<-ggplot()+
  geom_point(data=data_gmt_p,aes(x=agey, y=gmt, 
                                 color=factor(year),shape=factor(year)),position=position_dodge(width = 0.5),size=2)+
  geom_errorbar(data=data_gmt_p,aes(x=agey, ymin=gmtlw,ymax=gmtup,
                                    color=factor(year)),position=position_dodge(width = 0.5),width=0,size=0.3)+
  scale_color_manual("",values = c("red","#EE7A00","#002870","maroon1",
                                   "#966b2c","gray40"))+
  scale_shape_manual("",values =c(16,17,15,18,3,4) )+
  scale_x_continuous(breaks=c(0:12),labels=c(0:12))+
  scale_y_continuous(breaks=c(4:12),labels=c(16,32,64,128,256,512,1024,2048,4096))+ labs(title="b")+
  #from sas
  geom_line(data=traj_p,aes(agemonth/12,p),colour="blue")+
  geom_ribbon(data=traj_p,aes(agemonth/12,ymin=lclp,ymax=uclp),fill="blue",alpha=0.1)+
  labs(x="Age (year)",y="GMT")+
  theme(legend.key = element_blank() ,
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.text =element_text(size=13),
        panel.background=element_blank(),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(face="bold",size = 16),
        plot.margin = margin(0.2,0.2,0.2,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=16))+
  coord_cartesian(expand = F,xlim = c(-0.3,13),ylim=c(4,12))
p3b

##Fig3.C======
nms_final <- read.xlsx("Fig3C.nms_final.xlsx",sheet = 1)

km <- survfit(Surv(time=left,time2=right,type = "interval2")~gp,data=nms_final)
s0 <- ggsurvplot (km,risk.table = "absolute",fun = "event",
                  title="c",break.x.by=1,size = 0.7,xlim=c(0,14),
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
  scale_color_manual(labels = c("All","Initial titer¡Ý128","Initial titer<128"),  ##change legend title
                     values=c("#002870","red","blue"))+
  coord_cartesian(expand = F,xlim = c(0,12.5),ylim = c(0,0.6))+
  theme(legend.position = c(0.5, 0.98))
p3c

##combine===========
library(ggpubr)
p <- ggarrange(p3a,p3b,p3c,ncol=3,align = "h",widths = c(1,1,1))
p
ggsave('Figure3_revised.pdf',p,width = 19, height =6)
