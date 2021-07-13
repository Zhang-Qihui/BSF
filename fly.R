getwd()
library(car)
library(multcomp)
library(gplots)
library(agricolae) 
library(tidyverse)
library(reshape2)
library(readxl)
library(RColorBrewer)
library(ggplot2)

a<-read.csv(file.choose())
a
attach(a)
table(meal)
qqPlot(lm(CR ~ meal, data = a), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(CR ~ meal, data = a) 

aggregate(CR, by = list(meal), FUN = mean)
aggregate(CR, by = list(meal), FUN = sd)
fit <- aov(CR ~ meal)
summary(fit) 
plotmeans(CR ~ meal, xlab = "meal type", ylab = "conversion rate", 
          main = "Mean Plot\nwith 95% CI") 
TukeyHSD(fit)
out <- LSD.test(fit,"meal",p.adj="bonferroni") 
out 
detach(a)

#table
c<-read.csv(file.choose())
c
c$meal<-as.factor(c$meal)
table1<-data.frame(t1=as.character(1:13))  
for ( i in c(3:7)){           
  means<-tapply(c[[i]],c$meal,mean)
  means<-sprintf('%.3f',round(means,3))       
  SD<-tapply(c[[i]],c$meal,sd)
  SD<-sprintf('%.3f',round(SD,3)) 
  M.aov<-aov(c[[i]]~meal,data=c) 
  out <- LSD.test(M.aov, "meal", p.adj="none") 
  marker<-out$groups  
  row_name <- row.names(marker)
  newmarker <- data.frame(row_name,marker$groups)
  newmarker <- newmarker[order(newmarker$row_name),]
  pmarker<-newmarker$marker.groups
  a<-paste(means,'±',SD,pmarker)
  table1[i-1]<-a}

table1
table1$t1<-c("CF7", "CF8","COH","FC",
             "MIX1","MIX2","NUS","O",
             "P","PKM","RIB","SBH","SBM")

colnames(table1) <- c("meal","Bioconversion Rate","Growth Rate",
                      "Survival Rate","Prepupal Weight","Relative Survival Rate")

table1
write.csv(table1,'Mean&SD.csv')
###输出表格结束#

#curves
b<-read.csv(file.choose())
b
attach(b)
ggplot(b,aes(x=factor(day),y=W,color=meal))
+ geom_errorbar(aes(ymin=W-SEW, ymax=W+SEW), width=.2)
+geom_line(size=1.5,aes(color=meal,group=meal))
+scale_color_manual("meal",values = mycolor12)+geom_point(size=2)+
  labs(x="Larval Development (Days)",
       y="Individual Larval weight (g)")+ #facet_grid(.~line)+#
  theme(plot.title = element_text(hjust = 0.32))+
  theme_bw()+theme(plot.title = element_text(family = "Times",face="bold"))+
  theme(axis.title.x = element_text(family = "Times",face="bold",size = 18.5))+
  theme(axis.title.y = element_text(family = "Times",face="bold",size = 18.5))+
  theme(panel.border = element_rect(colour = "black",fill=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
+scale_y_continuous(limits = c(0,0.35),breaks=seq(0,0.35,0.04))
detach(b)

#bars
c<-read.csv(file.choose())
c
attach(c)
n<-table(meal)
n
qqPlot(lm(SR ~ meal, data.frame = c), simulate = TRUE, 
       main = "QQ Plot", labels = FALSE) 
bartlett.test(SR ~ meal, data = c) 
fit <- aov(SR ~ meal)
TukeyHSD(fit)
out <- LSD.test(fit,"meal",p.adj="bonferroni") 
out
aa = out$groups
mean = as.data.frame(tapply(SR,c$meal,mean,na.rm=TRUE))
sd = as.data.frame(tapply(SR,c$meal,sd,na.rm=TRUE))
se = sd/sqrt(n)
aaa = cbind(mean,se)
all = merge(aa,aaa, by="row.names",all=F)
all
names(all)<-c("meal","SR","sig","mean","se")
all
detach(c)
attach(all)
ggplot(all, aes(x = reorder(meal,mean),y=mean)) + 
  geom_bar(stat = "identity", position = "dodge",colour="black",width=.7,size=.5)+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1,size=1)+
  geom_line(size=.8)+theme_bw()+
  theme(
    axis.title = element_text(size =17, face="bold"),
    axis.text = element_text(vjust=1,size=13,face="bold",colour = "black"),
    axis.ticks = element_line(size = rel(2.5)),
    axis.ticks.length = unit(0.5, "cm"),
    panel.border = element_rect(colour = "black",size=2.5,fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  labs(
    x="Meal",
    y="Survival Rate"
  )+
  #+facet_grid(?~line,space="free")+#
  geom_text(
    aes(label=sig,y=mean+se,x=meal), hjust=-0.3,
    size = 5)+
  geom_hline(yintercept = c(0.3,0.6,0.9))+
  coord_flip()

detach(all)
