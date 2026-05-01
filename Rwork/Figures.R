setwd("/Users/hyli0001/Documents/wd/Rosinedal_2018/Rwork")
source('Plot_biomass.R')

##Weather

plot(vpd~year,wdd,type='o')
plot(ppt~year,wdd,type='o')
plot(airT~year,wdd,type='o')
plot(airT.6~year,wdd,type='o')
plot(airT.7~year,wdd,type='o')
plot(airT.8~year,wdd,type='o')

plot(vpd.6~year,wdd,type='o')
plot(vpd.7~year,wdd,type='o')
plot(vpd.8~year,wdd,type='o')

plot(ppt.6~year,wdd,type='o')
plot(ppt.7~year,wdd,type='o')
plot(ppt.8~year,wdd,type='o')



quartz()
par(mfrow=c(2,3))
# par(lwd=0.3)
x<-inb$d[inb$plot==3&inb$year==2005&!is.na(inb$d)]
h<-hist(x,breaks=25, xlab="Diameter at 1.3 m (m)",main="2005",lwd=.3,lty=2,ylim=c(0,40),xlim=c(.06,.32)) 
xfit<-seq(min(x),max(x),length=100) 
y<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-y*diff(h$mids[1:2])*length(x) 
lines(xfit,yfit, col=1,lwd=1.5,lty=2)

x<-inb$d[inb$plot==2&inb$year==2005&!is.na(inb$d)]
h<-hist(x,breaks=25,lwd=.3,lty=1,add=TRUE,col=8) 
xfit<-seq(min(x),max(x),length=100) 
y<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-y*diff(h$mids[1:2])*length(x) 
lines(xfit,yfit, col=1,lwd=1.5)

x<-inb$d[inb$plot==3&inb$year==2011&!is.na(inb$d)]
h<-hist(x,breaks=25, xlab="Diameter at 1.3 m (m)",main="2011",lwd=.3,lty=2,ylim=c(0,40),xlim=c(.06,.32)) 
xfit<-seq(min(x),max(x),length=100) 
y<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-y*diff(h$mids[1:2])*length(x) 
lines(xfit,yfit, col=1,lwd=1.5,lty=2)

x<-inb$d[inb$plot==2&inb$year==2011&!is.na(inb$d)]
h<-hist(x,breaks=25,lwd=.3,lty=1,add=TRUE,col=8) 
xfit<-seq(min(x),max(x),length=100) 
y<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-y*diff(h$mids[1:2])*length(x) 
lines(xfit,yfit, col=1,lwd=1.5)

x<-inb$d[inb$plot==3&inb$year==2018&!is.na(inb$d)]
h<-hist(x,breaks=25, xlab="Diameter at 1.3 m (m)",main="2018",lwd=.3,lty=2,ylim=c(0,40),xlim=c(.06,.32)) 
xfit<-seq(min(x),max(x),length=100) 
y<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-y*diff(h$mids[1:2])*length(x) 
lines(xfit,yfit, col=1,lwd=1.5,lty=2)

x<-inb$d[inb$plot==2&inb$year==2018&!is.na(inb$d)]
h<-hist(x,breaks=25,lwd=.3,lty=1,add=TRUE,col=8) 
xfit<-seq(min(x),max(x),length=100) 
y<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit<-y*diff(h$mids[1:2])*length(x) 
lines(xfit,yfit, col=1,lwd=1.5)

hm.tb<-read.table("d_h_model.txt",sep="\t",head=TRUE)	#h~c*yr+1.3+(d/(a+b*d))^(2)
dhm<-read.table("sample.tree.measurement.txt",head=TRUE,sep='\t')
dhm$bg<-ifelse(dhm$plot==2,8,'white')
for (i in c(2005,2011,2018)){
	df<-dhm[dhm$year==i,]
	plot(NA,ylim=c(10,24),xlim=c(.08,.32),xlab='Diameter at 1.3 m (m)',ylab='Height (m)')
	for (j in 2:3) for (k in 1:3){
		df1<-df[df$plot==j&df$rep==k,]
		points(h~d,df1,pch=21,bg=bg)
			pa<-as.numeric(hm.tb[hm.tb$plt==j&hm.tb$rpl==k,3:5])
		curve(pa[3]*(i-2005)+1.3+(x/(pa[1]+pa[2]*x))^(2),add=TRUE,xlim=range(df1$d))}}


##### Size relative growth for three periods, 2002-2005; 2006-2011; 2012-2018
dhi<-read.table("Ros_dba_dh_sample_trees_11.13.18.txt",sep="\t",head=TRUE)
srg1<-unique(dhi[,c('Snr','plot')])
srg<-data.frame(lapply(srg1,rep,3))
srg$prd<-rep(1:3,each=60)
srg<-srg[order(srg$Snr),]
srg$inc<-'in'
srg$inc[srg$prd==3&srg$Snr%out%1:12]<-'out'

plot(dh~h,dhi[dhi$year%in%2002:2018,],col=0,type='o',ylim=c(0.1,0.7))
for(i in 1:24){
	points(dh~h,dhi[dhi$Snr==i&dhi$year%in%2002:2018,],col=plot,type='o')
}
for(i in 1:24){
	points(dh~h,dhi[dhi$Snr==i&dhi$year%in%2006:2018,],bg=plot,type='o',pch=21)
}

tail(dhi[dhi$year%in%2002:2018,])
dhi$rdh<-dhi$dh/dhi$h
head(dhi,20)
plot(rdh~year,dhi[dhi$year%in%2002:2018,],col=0,type='o')
for(i in 1:24){
	points(rdh~year,dhi[dhi$Snr==i&dhi$year%in%2002:2018,],col=plot,type='o')
}
for(i in 1:24){
	points(rdh~year,dhi[dhi$Snr==i&dhi$year%in%2006:2018,],bg=plot,type='o',pch=21)
}

plot(dh~year,dhi[dhi$year%in%2002:2018,],col=0,type='o')
for(i in 1:24){
	points(dh~year,dhi[dhi$Snr==i&dhi$year%in%2002:2018,],col=plot,type='o')
}
for(i in 1:24){
	points(dh~year,dhi[dhi$Snr==i&dhi$year%in%2006:2018,],bg=plot,type='o',pch=21)
}



for (i in 1:60) for (j in 1:3){
	if(j==1){
		k<-2002:2005}else{
			if(j==2){k<-2006:2011}else{
			k<-2012:2018}}
	srg$bais[srg$Snr==i&srg$prd==j]<-mean(dhi$bai[dhi$Snr==i&dhi$year%in%k],na.rm=TRUE)*10^4
	srg$dhs[srg$Snr==i&srg$prd==j]<-mean(dhi$dh[dhi$Snr==i&dhi$year%in%k],na.rm=TRUE)
	srg$bam[srg$Snr==i&srg$prd==j]<-mean(dhi$ba[dhi$Snr==i&dhi$year%in%k],na.rm=TRUE)*10^4
	srg$hm[srg$Snr==i&srg$prd==j]<-mean(dhi$h[dhi$Snr==i&dhi$year%in%k],na.rm=TRUE)}

irg<-srg[srg$inc=='in',]
irg$inc<-NULL
summary(lm(log(bais)~log(bam)+as.factor(plot),srg[srg$prd==1,]))
par(mfrow=c(2,3))
for (i in 1:3){
plot(bais~bam,irg[irg$prd==i,],col=plot,xlim=c(0,500),ylim=c(0,20),xlab='Basal area (cm2)',ylab='Basal area increment (cm2 yr-1)',main=ifelse(i==1,'2002-2005',ifelse(i==2,'2006-2011','2012-2018')))}
for (i in 1:3){
plot(dhs~hm,irg[irg$prd==i,],col=plot,xlim=c(9,21),ylim=c(0,.5),xlab='tree height (m)',ylab='Height increment (m yr-1)')}
legend('bottomleft',c('R','F'),col=c(2,3),pch=1)


quartz()
par(mfrow=c(2,3))
plot(NA,xlim=c(2005,2018),ylim=c(0.1,3.1),xlab='Year',ylab='Basal area (m2 ha-1)')
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$ba.m[i]+c(-1,1)*stdm$ba.se[i])}
points(ba.m~year,stdm[stdm$plot==3,],bg=plot,type='o',pch=21)
points(ba.m~year,stdm[stdm$plot==2,],bg=plot,type='o',pch=21)
abline(a=1.5,b=0,lty=2)

par(new = T)
plot(NA,xlim=c(2005,2018),ylim=c(120,520),xlab="",ylab="",xaxt="n",yaxt="n")
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$Vst.m[i]+c(-1,1)*stdm$Vst.se[i])}
points(Vst.m~year,stdm[stdm$plot==3,],pch=24,bg=3,cex=.75,type='o')
points(Vst.m~year,stdm[stdm$plot==2,],pch=24,bg=2,cex=.75,type='o')
axis (4,seq(0,320,by=100),tck=.02,mgp=c(0,0,0),cex.axis=12/12,lwd=.3)
mtext(expression(paste('Standing volume',' (m'^3,' ha'^-1,')')),4,line=1.2,cex=.8)


plot(sd.m~year,stdm,col=0,ylim=c(700,1200),xlab='Year',ylab='Stand density (stem ha-1)')
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$sd.m[i]+c(-1,1)*stdm$sd.se[i])}
points(sd.m~year,stdm[stdm$plot==3,],bg=plot,type='o',pch=21)
points(sd.m~year,stdm[stdm$plot==2,],bg=plot,type='o',pch=21)

rsd.df<-stdm[stdm$year%in%c(2005,2011,2018),]
plot(log.ba.m~log.sd.m,rsd.df,col=0,xlim=c(2.8,3.6),ylim=c(-2,-1.4),ylab='log [mean basal area at 1.3 m (m2)]',xlab='log [stand density (stems ha-1)]')
for (i in 1:6){
lines(rep(rsd.df$log.sd.m[i],2),rsd.df$log.ba.m[i]+c(-1,1)*rsd.df$log.ba.se[i])
lines(rsd.df$log.sd.m[i]+c(-1,1)*rsd.df$log.sd.se[i],rep(rsd.df$log.ba.m[i],2))	
}
points(log.ba.m~log.sd.m,rsd.df[rsd.df$plot==3,],bg=plot,type='o',pch=21)
points(log.ba.m~log.sd.m,rsd.df[rsd.df$plot==2,],bg=plot,type='o',pch=21)
abline(a=1.98,b=-1.12)

quartz()
par(mfrow=c(2,3))
plot(NA,xlim=c(2005,2018),ylim=c(100,800),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(a)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$Bf.m[i]+c(-1,1)*stdm$Bf.se[i])}
points(Bf.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(Bf.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,1000,by=200),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Foliage biomass',' (g dw m'^-2,')')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,300),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(b)',line=-1,adj=0.05,font=2,cex=8/12)
lfyf<-lfy[lfy$year<2019,]
for (i in 1:nrow(lfyf)){
lines(rep(lfyf$year[i],2),lfyf$needle.m[i]+c(-1,1)*lfyf$needle.se[i])}
points(needle.m~year,lfyf[lfyf$plot==3,],pch=21,bg=3,type='o')
points(needle.m~year,lfyf[lfyf$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,400,by=100),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Foliage litterfall (g dw m-2 yr-1)')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(1,4),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(c)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$lai.s.m[i]+c(-1,1)*stdm$lai.s.se[i])}
points(lai.s.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(lai.s.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,5,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Leaf area index')),2,line=1,cex=.8)
legend('bottomright',c('Reference','Fertilized'),pt.bg=c(3,2),pch=21,col=1,cex=.8,box.col=0)

plot(NA,xlim=c(2005,2018),ylim=c(0,3),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(d)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:24){
lines(rep(fnm$year[i],2),fnm$N.pct.m[i]+c(-1,1)*fnm$N.pct.se[i])}
points(N.pct.m~year,fnm[fnm$plot==3,],pch=21,bg=3,type='o')
points(N.pct.m~year,fnm[fnm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,3,by=1),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Foliage [N] (%)')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,.3),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(e)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:24){
lines(rep(fnm$year[i],2),fnm$P.pct.m[i]+c(-1,1)*fnm$P.pct.se[i])}
points(P.pct.m~year,fnm[fnm$plot==3,],pch=21,bg=3,type='o')
points(P.pct.m~year,fnm[fnm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,.3,by=.1),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Foliage [P] (%)')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,.2),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(f)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:24){
lines(rep(fnm$year[i],2),fnm$PN.m[i]+c(-1,1)*fnm$PN.se[i])}
points(PN.m~year,fnm[fnm$plot==3,],pch=21,bg=3,type='o')
points(PN.m~year,fnm[fnm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,.3,by=.05),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Foliage PN ratio')),2,line=1,cex=.8)

quartz()
par(mfrow=c(2,3))
plot(NA,xlim=c(2005,2018),ylim=c(0,12),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(a)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$dVsw.m[i]+c(-1,1)*stdm$dVsw.se[i])}
points(dVsw.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(dVsw.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,15,by=5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Stem volume increment (m3 ha-1 yr-1)')),2,line=1,cex=.8)
legend('bottomright',c('Reference','Fertilized'),pt.bg=c(3,2),pch=21,col=1,cex=.8,box.col=0)

plot(NA,xlim=c(2005,2018),ylim=c(0,12),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(b)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$dVswe.m[i]+c(-1,1)*stdm$dVswe.se[i])}
points(dVswe.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(dVswe.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,15,by=5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Earlywood volume increment (m3 ha-1 yr-1)')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,12),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(c)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$dVswl.m[i]+c(-1,1)*stdm$dVswl.se[i])}
points(dVswl.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(dVswl.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,15,by=5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Latewood volume increment (m3 ha-1 yr-1)')),2,line=1,cex=.8)
head(stdm)

plot(NA,xlim=c(2005,2018),ylim=c(0,230),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(d)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPPsw.m[i]+c(-1,1)*stdm$NPPsw.se[i])}
points(NPPsw.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(NPPsw.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,200,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP of stem wood (g C m-2 yr-1)')),2,line=1,cex=.8)
legend('bottomright',c('Reference','Fertilized'),pt.bg=c(3,2),pch=21,col=1,cex=.8,box.col=0)

plot(NA,xlim=c(2005,2018),ylim=c(0,230),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(e)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPPswe.m[i]+c(-1,1)*stdm$NPPswe.se[i])}
points(NPPswe.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(NPPswe.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,200,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP of earlywood (g C m-2 yr-1)')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,230),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(f)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPPswl.m[i]+c(-1,1)*stdm$NPPswl.se[i])}
points(NPPswl.m~year,stdm[stdm$plot==3,],pch=21,bg=3,type='o')
points(NPPswl.m~year,stdm[stdm$plot==2,],pch=21,bg=2,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,200,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP of latewood (g C m-2 yr-1)')),2,line=1,cex=.8)



stt<-stw[stw$year>2008,]

quartz()
par(mfrow=c(2,3))
plot(dVsw~vpd,stt,col=plot)
plot(swd~vpd,stt,col=plot)
plot(NPPsw~vpd,stt,col=plot)

plot(dVsw~ppt,stt,col=plot)
plot(swd~ppt,stt,col=plot)
plot(NPPsw~ppt,stt,col=plot)

quartz()
par(mfrow=c(2,3))
plot(dVer~vpd,stt,col=plot)
plot(swer~vpd,stt,col=plot)
plot(NPPer~vpd,stt,col=plot)

plot(dVer~ppt,stt,col=plot)
plot(swer~ppt,stt,col=plot)
plot(NPPer~ppt,stt,col=plot)

quartz()
par(mfrow=c(2,3))
plot(dVswe~vpd,stt,col=plot)
plot(swe~vpd,stt,col=plot)
plot(NPPswe~vpd,stt,col=plot)

plot(dVswe~ppt,stt,col=plot)
plot(swe~ppt,stt,col=plot)
plot(NPPswe~ppt,stt,col=plot)

quartz()
par(mfrow=c(2,3))
plot(dVswl~vpd,stt,col=plot)
plot(swl~vpd,stt,col=plot)
plot(NPPswl~vpd,stt,col=plot)

plot(dVswl~ppt,stt,col=plot)
plot(swl~ppt,stt,col=plot)
plot(NPPswl~ppt,stt,col=plot)


quartz()
par(mfrow=c(2,3))
plot(dVswl~vpd,stt,col=plot)
plot(swl~vpd,stt,col=plot)
plot(NPPswl~vpd,stt,col=plot)

plot(dVswl~ppt,stt,col=plot)
plot(swl~ppt,stt,col=plot)
plot(NPPswl~ppt,stt,col=plot)

stdf<-stdm[stdm$plot==2,]
quartz()
par(mfrow=c(3,3))
plot(r.dVsw~year,stdf,type='o')
plot(r.dVswe~year,stdf,type='o')
plot(r.dVswl~year,stdf,type='o')
plot(r.swd~year,stdf,type='o')
plot(r.swe~year,stdf,type='o')
plot(r.swl~year,stdf,type='o')
plot(r.NPPsw~year,stdf,type='o')
plot(r.NPPswe~year,stdf,type='o')
plot(r.NPPswl~year,stdf,type='o')

quartz()
par(mfrow=c(3,3))
stdff<-stdm[stdm$plot==2&stdm$year>2008,]
plot(r.dVsw~vpd,stdff)
plot(r.dVswe~vpd,stdff)
plot(r.dVswl~vpd,stdff)
plot(r.swd~vpd,stdff)
plot(r.swe~vpd,stdff)
plot(r.swl~vpd,stdff)
plot(r.NPPsw~vpd,stdff)
plot(r.NPPswe~vpd,stdff)
plot(r.NPPswl~vpd,stdff)

quartz()
par(mfrow=c(1,3))
plot(r.NPPw~year,stdf,ylim=c(1,2),type='o')
plot(r.NPPw~vpd,stdff,ylim=c(1.4,2))
plot(r.NPPw~ppt,stdff,ylim=c(1.4,2))

# par(mfrow=c(2,1))
# plot(NA,xlim=c(2005,2018),ylim=c(0,12),xlab="",ylab="",xaxt="n",yaxt="n")
# mtext('(a)',line=-1,adj=0.05,font=2,cex=8/12)
# for (i in 1:28){
# lines(rep(stdm$year[i],2),stdm$dVsw.m[i]+c(-1,1)*stdm$dVsw.se[i])
# lines(rep(stdm$year[i],2),stdm$dVswe.m[i]+c(-1,1)*stdm$dVswe.se[i])
# lines(rep(stdm$year[i],2),stdm$dVswl.m[i]+c(-1,1)*stdm$dVswl.se[i])
# }
# for (i in 2:3){
# points(dVsw.m~year,stdm[stdm$plot==i,],pch=21,bg=i,type='o')
# points(dVswe.m~year,stdm[stdm$plot==i,],pch=24,bg=i,type='o')
# points(dVswl.m~year,stdm[stdm$plot==i,],pch=23,bg=i,type='o')
# }
# axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
# axis (2,seq(0,15,by=5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
# mtext(expression(paste('Stem volume increment (m3 ha-1 yr-1)')),2,line=1,cex=.8)
# legend('topright',c('Reference','Fertilized'),pt.bg=c(3,2),pch=21,col=1,cex=.8,box.col=0)
# 
# 
# plot(NA,xlim=c(2005,2018),ylim=c(0,250),xlab="",ylab="",xaxt="n",yaxt="n")
# mtext('(a)',line=-1,adj=0.05,font=2,cex=8/12)
# for (i in 1:28){
# lines(rep(stdm$year[i],2),stdm$NPPsw.m[i]+c(-1,1)*stdm$NPP.se[i])
# lines(rep(stdm$year[i],2),stdm$NPPswe.m[i]+c(-1,1)*stdm$NPPswe.se[i])
# lines(rep(stdm$year[i],2),stdm$NPPswl.m[i]+c(-1,1)*stdm$NPPswl.se[i])
# }
# for (i in 2:3){
# points(NPPsw.m~year,stdm[stdm$plot==i,],pch=21,bg=i,type='o')
# points(NPPswe.m~year,stdm[stdm$plot==i,],pch=24,bg=i,type='o')
# points(NPPswl.m~year,stdm[stdm$plot==i,],pch=23,bg=i,type='o')
# }
# axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
# axis (2,seq(0,15,by=5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
# mtext(expression(paste('Stem volume increment (m3 ha-1 yr-1)')),2,line=1,cex=.8)
# legend('topright',c('Reference','Fertilized'),pt.bg=c(3,2),pch=21,col=1,cex=.8,box.col=0)
# 




