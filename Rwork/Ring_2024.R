source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd('/Users/hyli0001/Documents/wd/Rosinedal_2018/Rwork')
rwdrr<-read.table('Ring_2024.txt',header=TRUE,sep='\t')
rwdr<-melt(rwdrr,id=colnames(rwdrr)[1:8])
rwdr$year<-as.integer(substr(rwdr$variable,2,5))
rwdr$wradi<-rwdr$value
rwdr$d2024<-(rwdr$d1+rwdr$d2)/2
rwdr$Snr<-paste(rwdr$scan_nr,rwdr$scan_order,sep='-')
rwdr$trt1<-rwdr$treatment
rwdr$trt2<-ifelse(rwdr$plot<4,'C','P')
rwdr<-rwdr[order(rwdr$trt1,rwdr$trt2,rwdr$plot,rwdr$Tnr1,rwdr$Tnr2,-rwdr$year),]
rwdr$cwradi<-ave(rwdr$wradi,rwdr$Snr,FUN=cumsum)
rwdr$bg<-ifelse(rwdr$trt1=='C',4,2)
rwdr$pch<-ifelse(rwdr$trt2=='C',21,24)
rwdr$col<-ifelse(rwdr$trt2=='C',1,rwdr$bg)

rwd<-rwdr[,c('trt1','trt2','plot','bg','pch','col','Tnr1','Tnr2','Snr','d2024','year','wradi','cwradi')]
rwd$d.1<-(rwd$d2024-(rwd$cwradi*2))/10
rwd$d<-(rwd$d.1+(rwd$wradi*2/10))
rwd$ba<-BA(rwd$d)
rwd$ba.1<-BA(rwd$d.1)
rwd$bai<-rwd$ba-rwd$ba.1
(rwd[rwd$bai>25,])
rwd$log.rbai<-log(rwd$ba/rwd$ba.1)
rwd$rbai<-rwd$bai/(rwd$ba)
head(rwdm)
rwdmm<-summaryBy(rbai+ba+bai~trt1+trt2+plot+year+bg+pch+col,rwd,FUN=mean,keep.names=TRUE)
rwdm<-summaryBy(rbai+ba+bai~trt1+trt2+year+bg+pch+col,rwdmm,FUN=me)

plot(rbai.m~year,rwdm,bg=bg,pch=pch,col=col)
plot(ba.m~year,rwdm,bg=bg,pch=pch,col=col)
plot(bai.m~year,rwdm,bg=bg,pch=pch,col=col)


quartz(w=4,h=4)
plot(NA,xlim=c(180,310),ylim=c(2,10),xlab='',ylab='',xaxt='n',yaxt='n')
mtext('Rosinedal 2001-2005',3,line=0,adj=1,cex=0.9)
mtext('Pinus sylvestris',3,line=0,adj=0,font=3,cex=0.9)

df<-rwdm[rwdm$year%in%2001:2005,]
for (i in 1:nrow(df)){
	lines(rep(df$ba.m[i],2),df$bai.m[i]+c(-1,1)*df$bai.se[i],lwd=0.3)
	lines(df$ba.m[i]+c(-1,1)*df$ba.se[i],rep(df$bai.m[i],2),lwd=0.3)
	}
points(bai.m~ba.m,df[df$trt1=='C'&df$trt2=='C',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$trt1=='C'&df$trt2=='P',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$trt1=='F'&df$trt2=='C',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$trt1=='F'&df$trt2=='P',],bg=bg,pch=pch,col=col,type='o')
axis(1,seq(150,800,by=50),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)
axis(2,seq(0,10,by=3),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)

mtext(expression(paste('Basal area (cm'^2,' tree'^-1,')',sep='')),1,line=1.2)
mtext(expression(paste('Basal area increment',sep='')),2,line=2)
mtext(expression(paste('(cm'^2,' tree'^-1, ' y'^-1,')',sep='')),2,line=.8)


quartz(w=7,h=4)
par(mfrow=c(1,2))

plot(NA,xlim=c(180,480),ylim=c(2,17),xlab='',ylab='',xaxt='n',yaxt='n')
mtext('Pinus sylvestris',3,line=0,adj=0,font=3,cex=1)

df<-rwdm[rwdm$year%in%2001:2024,]
for (i in 1:nrow(df)){
	lines(rep(df$ba.m[i],2),df$bai.m[i]+c(-1,1)*df$bai.se[i],lwd=0.3)
	lines(df$ba.m[i]+c(-1,1)*df$ba.se[i],rep(df$bai.m[i],2),lwd=0.3)
	}
points(bai.m~ba.m,df[df$trt1=='C'&df$trt2=='C',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$trt1=='C'&df$trt2=='P',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$trt1=='F'&df$trt2=='C',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$trt1=='F'&df$trt2=='P',],bg=bg,pch=pch,col=col,type='o')

points(bai.m~ba.m,df[df$year<2006&df$trt1=='C'&df$trt2=='C',],bg='white',pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$year<2006&df$trt1=='C'&df$trt2=='P',],bg='white',pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$year<2006&df$trt1=='F'&df$trt2=='C',],bg='white',pch=pch,col=col,type='o')
points(bai.m~ba.m,df[df$year<2006&df$trt1=='F'&df$trt2=='P',],bg='white',pch=pch,col=col,type='o')

axis(1,seq(0,800,by=100),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)
axis(2,seq(0,20,by=5),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)

mtext(expression(paste('Basal area (cm'^2,' tree'^-1,')',sep='')),1,line=1.2)
mtext(expression(paste('Basal area increment',sep='')),2,line=2)
mtext(expression(paste('(cm'^2,' tree'^-1, ' y'^-1,')',sep='')),2,line=.8)



plot(NA,xlim=c(2001,2024),ylim=c(2,17),xlab='',ylab='',xaxt='n',yaxt='n')
mtext('Rosinedal 2001-2024',3,line=0,adj=1,cex=1)

for (i in 1:nrow(df)){
	lines(rep(df$year[i],2),df$bai.m[i]+c(-1,1)*df$bai.se[i],lwd=0.3)
	}
points(bai.m~year,df[df$trt1=='C'&df$trt2=='C',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~year,df[df$trt1=='C'&df$trt2=='P',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~year,df[df$trt1=='F'&df$trt2=='C',],bg=bg,pch=pch,col=col,type='o')
points(bai.m~year,df[df$trt1=='F'&df$trt2=='P',],bg=bg,pch=pch,col=col,type='o')

points(bai.m~year,df[df$year<2006&df$trt1=='C'&df$trt2=='C',],bg='white',pch=pch,col=col,type='o')
points(bai.m~year,df[df$year<2006&df$trt1=='C'&df$trt2=='P',],bg='white',pch=pch,col=col,type='o')
points(bai.m~year,df[df$year<2006&df$trt1=='F'&df$trt2=='C',],bg='white',pch=pch,col=col,type='o')
points(bai.m~year,df[df$year<2006&df$trt1=='F'&df$trt2=='P',],bg='white',pch=pch,col=col,type='o')

axis(1,seq(1990,2030,by=5),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)
axis(2,seq(0,20,by=5),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)

mtext(expression(paste('Year',sep='')),1,line=1.2)
mtext(expression(paste('Basal area increment',sep='')),2,line=2)
mtext(expression(paste('(cm'^2,' tree'^-1, ' y'^-1,')',sep='')),2,line=.8)

legend('topleft',c('FC','FP','RC','RP'),pt.bg=c(2,2,4,4),pch=c(21,24,21,24),col=c(1,2,1,4),box.col=0)






