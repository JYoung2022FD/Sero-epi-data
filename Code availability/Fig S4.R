setwd("D:/1-9 cohort/manuscript/reply_wxl/code&data for sharing/R code for sharing-revised/Fig2&3-sex group/data")

library(openxlsx) 
library(lubridate)
library(binom)
library(dplyr)
library(splines)
library(ggplot2)

##Fig2.A======
prevd_m <- read.xlsx("dat_all_seronegative_fit_line_male.xlsx",sheet = 1) 
prevd_f <- read.xlsx("dat_all_seronegative_fit_line_female.xlsx",sheet = 1) 
prevd_m <- prevd_m %>% mutate(sex="male")
prevd_f <- prevd_f %>% mutate(sex="female")

prevd_mf <- rbind(prevd_m,prevd_f)
prevd_mf$sex <- factor(prevd_mf$sex,levels = c("male","female"))
#male
prevd_m[round(prevd_m$agemonth,2)==0.00,"p2"]#seronegative prevalence at age of 0 month:0.2593011
prevd_m[round(prevd_m$agemonth,2)==5.00,"p2"]#seronegative prevalence at age of 5 months:0.9366316
prevd_m[round(prevd_m$agemonth,2)==36.00,"p2"]#seronegative prevalence at age of 36 months:0.657379
prevd_m[round(prevd_m$agemonth,2)==60.00,"p2"]#seronegative prevalence at age of 60 months:0.3419631
#female
prevd_f[round(prevd_f$agemonth,2)==0.00,"p2"]#seronegative prevalence at age of 0 month:0.2622092
prevd_f[round(prevd_f$agemonth,2)==5.00,"p2"]#seronegative prevalence at age of 5 months:0.9006325
prevd_f[round(prevd_f$agemonth,2)==36.00,"p2"]#seronegative prevalence at age of 36 months:0.6453269
prevd_f[round(prevd_f$agemonth,2)==60.00,"p2"]#seronegative prevalence at age of 60 months:0.323852


p2as<-ggplot()+
  geom_line(data=prevd_mf ,aes(agemonth,p2*100,color=sex))+
  geom_ribbon(data=prevd_mf ,aes(agemonth,ymin=uclp*100,ymax=lclp*100,fill=sex),alpha=0.1)+
  scale_x_continuous(breaks=seq(0,144,12),labels=seq(0,12,1))+
  scale_y_continuous(breaks=c((0:10)*10),labels=c((0:10)*10))+labs(title="A")+
  scale_color_manual("",breaks = c("male","female"),values = c("red","blue"))+
  scale_fill_manual("",breaks = c("male","female"),values = c("red","blue"))+
  labs(x="Age (year)",y="Proportion of susceptible individuals\n\ (%)")+
  theme(legend.key = element_blank() ,
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.text =element_text(size=13),
        panel.background=element_blank(),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(face="bold",size = 16),
        plot.margin = margin(0.5,0.5,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=16))+
  coord_cartesian(expand = F,ylim = c(-5,100),xlim = c(-3,150))+
  geom_line(aes(c(0,144),c(-5,-5)),color="black")+ # x axis
  geom_line(aes(c(-8,-8),c(0,100)),color="black")# y axis

p2as
##Fig2.B======
nfd_c <- read.xlsx("nfd_c.xlsx",sheet = 1)
nfd_cnd <- nfd_c %>% group_by(sex,episeason) %>% dplyr::summarise(cases=sum(infect_num),
                                                                  total=n()) 
CI16<-binom.confint(nfd_cnd$cases,nfd_cnd$total,methods="exact")
nfd_cnd$incidmean<-CI16$mean
nfd_cnd$incidlow<-CI16$lower
nfd_cnd$incidup<-CI16$upper
p2bs<-ggplot()+
  geom_point(data=nfd_cnd,aes(x=episeason, y=incidmean*100, 
                              color=factor(sex),shape=factor(sex)),position=position_dodge(width =0.3),size=2)+
  geom_errorbar(data=nfd_cnd,aes(x=episeason, ymin=incidlow*100,ymax=incidup*100,
                                 color=factor(sex)),position=position_dodge(width = 0.3),width=0,size=0.3)+
  scale_x_continuous(breaks=c(1,2,3),labels=c("2013/14","2014/15","2015/16"))+
  scale_y_continuous(breaks=seq(0,20,5),labels=seq(0,20,5))+labs(title="B")+
  scale_color_manual("",breaks=c(1,2),values = c("red","blue"),labels = c("male","female"))+
  scale_shape_manual("",breaks=c(1,2),values =c(16,17),labels = c("male","female"))+
  labs(x="Epidemic season",y="Incidence of EV-A71 infections\n\ (%)")+
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
  coord_cartesian(expand = F,ylim = c(0,20),xlim = c(0.5,3.5))
p2bs

##Fig2.C======
pops_est_ev71<- read.xlsx("pops_est_ev71_sex.xlsx",sheet = 1)
pops_est_ev71<-pops_est_ev71[,c("year","agec","gender","sum_ev71_est","totals")]
pops_est_ev71s <- pops_est_ev71 %>% group_by(year,gender)  %>% dplyr::summarise(sums=sum(sum_ev71_est),
                                                                         totalss=sum(totals))
binom <- binom.confint(pops_est_ev71s$sums,pops_est_ev71s$totalss,methods="exact")
pops_est_ev71s$incidence_total_ev71mean <- binom$mean
pops_est_ev71s$incidence_total_ev71Lower <- binom$lower
pops_est_ev71s$incidence_total_ev71Upper <- binom$upper

p2cs <- ggplot()+
  geom_bar(data=pops_est_ev71s,aes(x=year,weight=incidence_total_ev71mean*10^5,fill=factor(gender)),alpha=0.5,
           position = position_dodge2(width =0.9,padding = 0))+
  geom_errorbar(data=pops_est_ev71s, aes(x =year, ymin = incidence_total_ev71Lower*10^5, ymax = incidence_total_ev71Upper*10^5
  ),size=0.4,
  position = position_dodge2(width =0.9,padding = 0.8))+
  scale_fill_manual("",values = c("red","blue"),labels = c("male","female"))+
  scale_y_continuous(breaks = seq(0,300,50),labels = seq(0,300,50))+
  labs(title="C",y="Incidence of EV-A71 related HFMD\n\ (per 100,000)",x="Year")+
  theme(panel.background = element_blank(),panel.grid = element_blank(),
        legend.position = c(0.5,1.01),
        legend.direction = "horizontal",
        legend.key = element_blank(),
        legend.text =element_text(size=13),
        legend.title = element_text(size=13),
        axis.line =element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(face="bold",size = 16),
        plot.title = element_text(face="bold",size = 16),
        plot.margin = margin(0.2,0.4,0.1,0.2,"cm"),
        strip.text.x= element_text(size = 14))+
  coord_cartesian(expand = T,ylim = c(0,300),xlim = c(2012.5,2016.5))

p2cs

##combine===========
library(ggpubr)
p <- ggarrange(p2as,p2bs,p2cs,ncol=3,align = "h",widths = c(1,1,1))
p
ggsave('Figure2_sexgroup.pdf',p,width = 19, height =6)
