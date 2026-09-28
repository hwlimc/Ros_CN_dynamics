source('/Users/hyli0001/Documents/Rwork/functions.R')

setwd("/Users/hyli0001/wrd/b/Rosinedal_NPP/")
Bfr1825<-read.table("raw_data/fineroot_biomass_2018-2025.txt",head=TRUE,sep='\t')

Bfr1825$Bfr_g<-ifelse(Bfr1825$Bfr_g<0,0.001,Bfr1825$Bfr_g)

Bfr13<-read.table("raw_data/fineroot_biomass_2013.txt",head=TRUE,sep='\t')
head(Bfr13)

Bfr<-rbind(Bfr1825,Bfr13[,colnames(Bfr1825)])




tail(Bfr)



### for 2018, the core diameter was 4.5 cm; 2022 and 2025 it was 3.8cm
Bfr$Bfr_g.m2<-(Bfr$Bfr_g/((ifelse(Bfr$year==2018,4.5,3.8)/2)^2*pi))*10000


### For 2018, cores were disected into four layers: Organic, 0-5,5-15,15-25; for 2022 and 2025, they were into five: Organic, 0-5, 5-10, 10-20, 20-30. So scaling was made for 15-25 depth for 2018 samples
Bfr$Bfr_g.m3<-Bfr$Bfr_g.m2/(Bfr$depth/100)

# library(doBy)

Bfr_plot_rep_layer<-aggregate(Bfr_g.m3~depth+layer+rep+plot2+plot1+year, Bfr[Bfr$diameter_mm!='>2',],FUN=sum,na.rm=TRUE)
Bfr_plot_rep_layer$Bfr_g.m2<-Bfr_plot_rep_layer$Bfr_g.m3/100*ifelse(Bfr_plot_rep_layer$layer=='M25'&Bfr_plot_rep_layer$year==2018,15,Bfr_plot_rep_layer$depth)

Bfr_plot_rep<-aggregate(Bfr_g.m2~rep+plot2+plot1+year, Bfr_plot_rep_layer,FUN=sum)


head(Bfr_plot_rep)
boxplot(Bfr_g.m2~year*plot1, Bfr_plot_rep)


Bfr_plot<-aggregate(Bfr_g.m2~plot1+plot2, Bfr_plot_rep,FUN=mean)

aggregate(Bfr_g.m2~plot1, Bfr_plot, FUN=me)
aggregate(Bfr_g.m2~plot1, Bfr_plot_rep, FUN=me)
boxplot(Bfr_g.m2~plot1, Bfr_plot)


fr22<-read.table("Ros_fineroot_mass_2022.txt",head=TRUE,sep='\t')
fr22$lay2<-substr(fr22$lay,1,1)
head(fr22)
fr22$Bfr_g.m2<-fr22$Bfr_g/(((3.8/2)^2)*pi)*10000

fr22$Bfr_g.m3<-fr22$Bfr_g.m2/ifelse(fr22$lay=='M30',10,ifelse(fr22$lay=='M20',10,ifelse(fr22$lay=='M10',5,ifelse(fr22$lay=='M5',5,fr22$dep))))*100

se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}
me<-function(x){c(m=mean(x,na.rm=T),se=se(x))}

frm22mr<-summaryBy(Bfr_g.m3~plot1+plot2+lay,fr22,FUN=mean,keep.names=TRUE)
frm22m<-summaryBy(Bfr_g.m3~plot1+lay,frm22mr,FUN=me,keep.names=TRUE)
frm22m$depth<-c(-7.5,-15,-25,-2.5,2.5,-7.5,-15,-25,-2.5,2.5)
frm22m<-frm22m[order(frm22m$depth),]
plot(depth~Bfr_g.m3.m,frm22m,col=0,xlim=c(0,3500))
for (i in 1:10){
	lines(c(1,-1)*frm22m$Bfr_g.m3.se[i]+frm22m$Bfr_g.m3.m[i],rep(frm22m$depth[i],2))}
points(depth~Bfr_g.m3.m,frm22m[frm22m$plot1==2,],bg=1,pch=21,type='o')
points(depth~Bfr_g.m3.m,frm22m[frm22m$plot1==3,],bg='white',pch=21,type='o')

frm22<-summaryBy(Bfr_g.m2~plot1+plot2+rep+lay2,fr22,FUN=sum,keep.names=TRUE)
frm22a<-summaryBy(Bfr_g.m2~plot1+plot2+rep,fr22,FUN=sum,keep.names=TRUE)

boxplot(Bfr_g.m2~plot1,frm22[frm22$lay=='M',],main='Mineral soil',xlab='Treatment',ylim=c(0,650))
boxplot(Bfr_g.m2~plot1,frm22a,main='Organic + mineral',xlab='Treatment')





