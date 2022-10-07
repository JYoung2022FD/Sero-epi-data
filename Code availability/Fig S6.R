setwd("D:/1-9 cohort/manuscript/reply_wxl/code&data for sharing/R code for sharing-revised/Fig2&3-sex group/data")

library(openxlsx) 
library(lubridate)
library(binom)
library(dplyr)
library(splines)
library(ggplot2)
library(survival)
library(survminer)
##Fig3.A======
prevd_m <- read.xlsx("dat_all_fit_line_m.xlsx",sheet = 1) #change to degree=2
prevd_f <- read.xlsx("dat_all_fit_line_f.xlsx",sheet = 1) 

prevd_m <- prevd_m %>% mutate(sex="male")
prevd_f <- prevd_f %>% mutate(sex="female")

prevd_mf <- rbind(prevd_m,prevd_f)
prevd_mf$sex <- factor(prevd_mf$sex,levels = c("male","female"))

prevd_m[which(round(prevd_m$agemonth,2)==36.00),]#4.659708
prevd_m[which(round(prevd_m$agemonth,2)==60.00),]#5.577072

prevd_f[which(round(prevd_f$agemonth,2)==36.00),]#4.716571
prevd_f[which(round(prevd_f$agemonth,2)==60.00),]#5.893932


p3as<-ggplot()+
  geom_line(data=prevd_mf,aes(agemonth,p,color=sex))+
  geom_ribbon(data=prevd_mf,aes(agemonth,ymin=p-1.96*se,ymax=p+1.96*se,fill=sex),alpha=0.1)+
  labs(x="Age (year)",y="GMT")+# continuous Age
  scale_x_continuous(breaks=seq(0,144,12),labels=seq(0,12,1))+
  scale_y_continuous(breaks=c(2:8),labels=c(4,8,16,32,64,128,256))+labs(title="A")+
  scale_color_manual("",breaks = c("male","female"),values = c("red","blue"))+
  scale_fill_manual("",breaks = c("male","female"),values = c("red","blue"))+
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
  coord_cartesian(expand = F,xlim = c(-3,155),ylim=c(1.8,8))
p3as

##Fig3.B======
prevd_m <- read.xlsx("dat_aftinfec_fit_male.xlsx",sheet = 1) 
prevd_f <- read.xlsx("dat_aftinfec_fit_female.xlsx",sheet = 1) 

prevd_m <- prevd_m %>% mutate(sex="male")
prevd_f <- prevd_f %>% mutate(sex="female")

prevd_mf <- rbind(prevd_m,prevd_f)
prevd_mf$sex <- factor(prevd_mf$sex,levels = c("male","female"))

prevd_m[which(round(prevd_m$agemonth,2)==36.00),]#8.520181
prevd_m[which(round(prevd_m$agemonth,2)==60.00),]#7.990344

prevd_f[which(round(prevd_f$agemonth,2)==36.00),]#8.734426
prevd_f[which(round(prevd_f$agemonth,2)==60.00),]#8.292994

p3bs<-ggplot()+
  geom_line(data=prevd_mf,aes(agemonth/12,p,color=sex))+
  geom_ribbon(data=prevd_mf,aes(agemonth/12,ymin=lclp,ymax=uclp,fill=sex),alpha=0.1)+
  scale_x_continuous(breaks=c(0:12),labels=c(0:12))+
  scale_y_continuous(breaks=c(4:12),labels=c(16,32,64,128,256,512,1024,2048,4096))+ labs(title="B")+
  scale_color_manual("",breaks = c("male","female"),values = c("red","blue"))+
  scale_fill_manual("",breaks = c("male","female"),values = c("red","blue"))+
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
  coord_cartesian(expand = F,ylim=c(4,12))
p3bs

##Fig3.C======
nm <- read.xlsx("nm_final.xlsx",sheet = 1)
dat_all <- read.xlsx("dat_all_sexgroup.xlsx",sheet = 1)
dat_all_sex <- dat_all[,c("kid","sex")] %>% distinct()
nm <- left_join(nm,dat_all_sex,by="kid")
nms <- nm[,c("status","left","right","sex")]
nms$right[is.na(nms$right)] <- Inf 
nms$left <- nms$left/12
nms$right<- nms$right/12
nms$sex <- factor(nms$sex,levels = c(1,2),labels = c("male","female"))
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Icens")
library(interval)
km.fit <- icfit(Surv(time=left,time2=right,type = "interval2")~sex,data=nms)
library(ggplotify)
library(patchwork)

p3cs <- as.ggplot(~plot(km.fit,dtype="cdf",shade = F,XLAB = "",YLAB = "",
                        yscale=100,ylim=c(0,0.2),xlim=c(0,13),XLEG=4.5,YLEG=0.22,main=""))+
  annotate("text",x = 0.13, y = 0.96, label = "C",size=5.9,fontface=2)+
  annotate("text",x = 0.52, y = 0.04, label = "Age (Year)",size=5.5,fontface=2)+
  annotate("text",x = 0.03, y = 0.5, label = "Probability (%)",size=5.5,fontface=2,angle=90)                 
p3cs
##combine===========
library(ggpubr)
ps <- ggarrange(p3as,p3bs,p3cs,ncol=3,widths = c(1,1,1),heights = c(1,1,1))
ps
ggsave('Figure3_sexgroup.pdf',ps,width = 19, height =6)

