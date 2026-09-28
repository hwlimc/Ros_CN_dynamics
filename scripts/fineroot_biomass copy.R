setwd("/Users/hyli0001/wrd/b/Rosinedal_NPP/")
rm(list=ls())

se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}
me<-function(x){c(m=mean(x,na.rm=T),se=se(x))}


rootbiomass_raw<-readRDS("raw_data/fineroot_biomass.rds")
rootbiomass_raw$metadata[1:2]

Bfr13<-rootbiomass_raw$data[[1]]	## sample from 2013 June

Bfr13$mean_depth<-ifelse(Bfr13$layer=='O',2.5,ifelse(Bfr13$layer=='M5',-2.5,ifelse(Bfr13$layer=='M10',-7.5,ifelse(Bfr13$layer=='M15',-12.5,ifelse(Bfr13$layer=='M20',-17.5,NA)))))	 # mean vertical position of the samples
Bfr13$whole_depth<-ifelse(Bfr13$layer=='O',Bfr13$depth,5)	# whole length of the sample representing

Bfr18<-rootbiomass_raw$data[[2]]
Bfr22<-rootbiomass_raw$data[[3]]
Bfr25<-rootbiomass_raw$data[[4]]
Bfr1825<-rbind(Bfr18,Bfr22,Bfr25)

Bfr1825$mean_depth<-ifelse(Bfr1825$layer=='O',2.5,ifelse(Bfr1825$layer=='M5',-2.5,ifelse(Bfr1825$layer=='M10',-7.5,ifelse(Bfr1825$layer=='M15',-10,ifelse(Bfr1825$layer=='M20',-15,ifelse(Bfr1825$layer=='M25',-20,ifelse(Bfr1825$layer=='M30',-25,NA)))))))	 # mean vertical position of the samples

Bfr1825$whole_depth<-ifelse(Bfr1825$layer=='O',Bfr1825$depth,ifelse(Bfr1825$layer%in%c('M5','M10'),5,10))	# whole length of the sample representing


Bfr<-rbind(Bfr13[Bfr13$species=='pine',colnames(Bfr1825)],Bfr1825)
Bfr$trt<-Bfr$treatment
### core diameter: 2013, 6 cm; 2018, 4.5 cm; 2022 and 2025: 3.8cm
Bfr$Bfr_g.m2<-(Bfr$Bfr_g/((ifelse(Bfr$year==2013,6,ifelse(Bfr$year==2018,4.5,3.8))/2)^2*pi))*10000

### Core length differed between samplings. For 2013 cores were disected into 4 mineral layers, 0-5,5-10,10-15,15-20; For 2018, into 3 mineral layers: 0-5,5-15,15-25; for 2022 and 2025, 4 mineral layers: 0-5, 5-10, 10-20, 20-30. Two estimates were made: 0-20 cm; and 0-30 cm by extrapolating 2013 and 2018 samples.


######## 2026 Sep 28!! ## work on scaling 
# Bfr$Bfr_g.m3<-Bfr$Bfr_g.m2/(Bfr$depth/100)


Bfr_plot_rep_layer<-aggregate(Bfr_g.m3~layer+rep+plot1+plot2+trt+year+mean_depth, Bfr[Bfr$diameter_mm!='>2',],FUN=sum,na.rm=TRUE)	## Summing across diameter class

Bfr_plot_layer<-aggregate(Bfr_g.m3~layer+trt+plot1+year+mean_depth, Bfr_plot_rep_layer[Bfr_plot_rep_layer$trt%in%c('R','N'),],FUN=me)	## mean and se
Bfr_plot_layer<-do.call(data.frame, Bfr_plot_layer)

plot(mean_depth~Bfr_g.m3.m, Bfr_plot_layer,col=plot1-1)
points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$plot1==2&Bfr_plot_layer$year==2013,],type='o',col=2)
points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$plot1==3&Bfr_plot_layer$year==2013,],type='o')

points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$plot1==2&Bfr_plot_layer$year==2018,],type='o',col=2,lty=2)
points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$plot1==3&Bfr_plot_layer$year==2018,],type='o',lty=2)

points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$plot1==2&Bfr_plot_layer$year==2022,],type='o',col=2,lty=3,lwd=2)
points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$plot1==3&Bfr_plot_layer$year==2022,],type='o',lty=2,lwd=2)

points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$trt=='N'&Bfr_plot_layer$year==2025,],type='o',col=2,lty=4,lwd=2)
points(mean_depth~Bfr_g.m3.m, Bfr_plot_layer[Bfr_plot_layer$trt=='R'&Bfr_plot_layer$year==2025,],type='o',lty=4,lwd=2)





Bfr_plot_rep_layer$Bfr_g.m2<-Bfr_plot_rep_layer$Bfr_g.m3/100*ifelse(Bfr_plot_rep_layer$layer=='M25'&Bfr_plot_rep_layer$year==2018,15,Bfr_plot_rep_layer$depth)

Bfr_plot_rep<-aggregate(Bfr_g.m2~rep+plot2+plot1+year, Bfr_plot_rep_layer,FUN=sum)

boxplot(Bfr_g.m2~year*plot1, Bfr_plot_rep)


Bfr_plot<-aggregate(Bfr_g.m2~plot1+year, Bfr_plot_rep,FUN=me)

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





