library(gridExtra)
library(ggplot2)
library(ggpubr)

####################################################################
####              Semiannual-visit:seroprevalence            ####
####################################################################
####1.sero-prevalence-2 panels####
col<-c("red","blue","black")
x.label<-c("Baseline visit\n2013","Follow-up visit1\n2014","Follow-up visit2\n2014",
           "Follow-up visit3\n2015","Follow-up visit4\n2015",
           "Follow-up visit5\n2016","Follow-up visit6\n2016")

##plot all paricipants
data<-read.csv("Seroprevalence-by season-semi-annual visit.csv")
p<-ggplot(data, aes(x=index, y=SeroPosmean*100, group=VisitTime))+
  geom_point(aes(color=factor(cutoff)),size=0.8)+
  geom_errorbar(aes(ymin=SeroPoslow*100,ymax=SeroPosup*100,color=factor(cutoff)),width=0,size=0.5)

p.f<-p+labs(x="",y="Seroprevalence (%)")+
  guides(fill="none")+ ##remove legend
  scale_x_continuous(breaks=c(1:7),labels=rep("",7))+
  scale_y_continuous(breaks=c((seq(0,10,0.5))*10),labels=c((seq(0,10,0.5))*10),limits=c(40,90))+labs(title="A")+
  theme(legend.position="none",panel.background=element_blank(),panel.border=element_blank(),axis.text.y=element_text(size=9),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold",size = 10),
        plot.margin = margin(0.2,0.2,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=8))+
  scale_color_manual( ##set point and line color
                     values=col)

##plot full follow-ups
data<-read.csv("Seroprevalence-by season-semi-annual visit_full follow-ups.csv")
p1<-ggplot(data, aes(x=index, y=SeroPosmean*100, group=VisitTime))+
  geom_point(aes(color=factor(cutoff)),size=0.8)+
  geom_errorbar(aes(ymin=SeroPoslow*100,ymax=SeroPosup*100,color=factor(cutoff)),width=0,size=0.5)

p.f1<-p1+labs(x="",y="Seroprevalence (%)")+
  guides(fill="none")+ ##remove legend
  scale_x_continuous(breaks=c(1:7),labels=x.label)+
  scale_y_continuous(breaks=c((seq(0,10,0.5))*10),labels=c((seq(0,10,0.5))*10),limits=c(40,90))+labs(title="B")+
  theme(legend.position="none",panel.background=element_blank(),panel.border=element_blank(),axis.text.y=element_text(size=9),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold",size = 10),
        plot.margin = margin(0.2,0.2,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=8))+
  scale_color_manual( ##set point and line color
                     values=col)


##plot legend
data0<-read.csv("Seroincidence by age and season-01152019.csv")
data0$seroinci8mean<-NA
data0$seroinci8low<-NA
data0$seroinci8up<-NA
legend<-ggplot(data0, aes(x=index, y=seroinci8mean*100, group=VisitTime))+
  geom_point(aes(color=factor(VisitTime)),size=1)+
  geom_errorbar(aes(ymin=seroinci8low*100,ymax=seroinci8up*100,color=factor(VisitTime)),width=0.1,size=0.7)
legend.f<-legend+labs(x="",y="")+
  scale_color_manual(name="Cutoff titer",
                     values=col,
                     breaks=c("season1valid", "season2valid","season3valid"),
                     labels=c("8","16","32"))+
  theme(legend.position="top",panel.background=element_rect(fill ="white", colour ="white",linetype=1),panel.grid =element_blank(),
        axis.line = element_line(colour = "white"),
        plot.margin = margin(0.3,0.2,0,0.2,"cm"),
        axis.text=element_blank(),
        legend.key=element_rect(fill='white'),
        axis.ticks=element_blank())+
  guides(color=guide_legend(nrow=1,byrow=TRUE,title.position="left",title.hjust = 0.5)) ##legend

##combine plots
windows(width=10,height=7)

p <- ggarrange(p.f,p.f1,legend.f,nrow=3,heights = c(3,3,1))

ggsave('fig S2.pdf',p,
       width = 10, height = 7, limitsize = FALSE)



####################################################################
####              Semiannual-visit:seroincidence            ####
####################################################################
####2.sero-incidence-2 panels####
col<-c("red","blue","black")
x.label<-c("Baseline/Follow-up visit1","Follow-up visit1/2","Follow-up visit2/3",
           "Follow-up visit3/4","Follow-up visit4/5", "Follow-up visit5/6")

##plot all paricipants
seroinci<-read.csv("Seroincidence by season_semiannual.csv")

p<-ggplot(seroinci, aes(x=index, y=seroinci.totalcasemean*100, group=cutoff))+
  geom_point(aes(color=factor(cutoff)),size=0.8)+
  geom_errorbar(aes(ymin=seroinci.totalcaselow*100,ymax=seroinci.totalcaseup*100,color=factor(cutoff)),width=0,size=0.5)
p.f<-p+labs(x="",y="Incidence (%)")+
  scale_color_manual(values=col)+
  theme(legend.position="none",panel.background=element_rect(fill ="white", colour ="white",linetype=1),panel.grid =element_blank(),axis.text.y=element_text(size=9),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold",size = 10),
        plot.margin = margin(0.2,0,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=7))+
  guides(fill=F)+
  scale_x_continuous(breaks=c(1:6),labels=rep("",6))+
  scale_y_continuous(breaks=seq(0,2.5,0.2)*10,labels=seq(0,2.5,0.2)*10,limits=c(0,16))+labs(title="A")

##plot full follow-ups
seroinci<-read.csv("Seroincidence by season_semiannual_full follow-ups.csv")

p1<-ggplot(seroinci, aes(x=index, y=seroinci.totalcasemean*100, group=cutoff))+
  geom_point(aes(color=factor(cutoff)),size=0.8)+
  geom_errorbar(aes(ymin=seroinci.totalcaselow*100,ymax=seroinci.totalcaseup*100,color=factor(cutoff)),width=0,size=0.5)
p.f1<-p1+labs(x="",y="Incidence (%)")+
  scale_color_manual(values=col)+
  theme(legend.position="none",panel.background=element_rect(fill ="white", colour ="white",linetype=1),panel.grid =element_blank(),axis.text.y=element_text(size=9),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(face="bold",size = 10),
        plot.margin = margin(0.2,0,0.1,0.2,"cm"),
        plot.title = element_text(hjust = 0,face="bold",size=7))+
  guides(fill=F)+
  scale_x_continuous(breaks=c(1:6),labels=x.label)+
  scale_y_continuous(breaks=seq(0,2.5,0.2)*10,labels=seq(0,2.5,0.2)*10,limits=c(0,16))+labs(title="B")

##plot legend
data0<-read.csv("Seroincidence by age and season-01152019.csv")
data0$seroinci8mean<-NA
data0$seroinci8low<-NA
data0$seroinci8up<-NA
legend<-ggplot(data0, aes(x=index, y=seroinci8mean*100, group=VisitTime))+
  geom_point(aes(color=factor(VisitTime)),size=1)+
  geom_errorbar(aes(ymin=seroinci8low*100,ymax=seroinci8up*100,color=factor(VisitTime)),width=0.1,size=0.7)
legend.f<-legend+labs(x="",y="")+
  scale_color_manual(name="Cutoff titer",
                     values=col,
                     breaks=c("season1valid", "season2valid","season3valid"),
                     labels=c("8","16","32"))+
  theme(legend.position="top",panel.background=element_rect(fill ="white", colour ="white",linetype=1),panel.grid =element_blank(),
        axis.line = element_line(colour = "white"),
        plot.margin = margin(0.3,0.2,0,0.2,"cm"),
        legend.key=element_rect(fill='white'),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  guides(color=guide_legend(nrow=1,byrow=TRUE,title.position="left",title.hjust = 0.5)) ##legend

##combine plots
windows(width=10,height=7)

p <- ggarrange(p.f,p.f1,legend.f,nrow=3,heights = c(3,3,1))

ggsave('fig S3.pdf',p,
       width = 10, height = 7, limitsize = FALSE)
