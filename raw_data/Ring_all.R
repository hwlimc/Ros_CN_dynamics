source('../functions.R')
library('collapse')
arir<-read.table('../raw_data/arsring.txt',header=TRUE,sep='\t')
ari<-collap(arir,radius+wid.e+wid.l+wg+wg.mn+wg.mx+wg.e+wg.l~sample.yr+plot+subplot+Snr+d.msrd+year,FUN=fmean,na.rm=TRUE)

ari<-ari[order(ari$sample.yr,ari$plot,ari$subplot,ari$Snr,-ari$year),]

ari$crad<-ave(ari$radius,ari$sample.yr,ari$plot,ari$subplot,ari$Snr,FUN=cumsum)

ari$d.1<-(ari$d.msrd-(ari$crad*2))/10
ari$d<-(ari$d.1+(ari$radius*2/10))
# write.table(ari,'../annual_diameter_cores.txt',quote=FALSE,row.names=FALSE,sep='\t')

ari$ba<-(ari$d)^2/4*pi
ari$ba.1<-(ari$d.1)^2/4*pi
ari$bai<-ari$ba-ari$ba.1
ari$log.rbai<-log(ari$ba/ari$ba.1)
ari$rbai<-ari$bai/(ari$ba)
arimm<-collap(ari,ba+bai+rbai~sample.yr+plot+subplot+Snr+year,FUN=fmean,na.rm=TRUE)
arimm$pho<-ifelse(arimm$subplot%in%4:6,'P','C')
arim<-collap(arimm,ba+bai+rbai~plot+pho+year,FUN=list(m=function(x){fmean(x,na.rm=TRUE)},se=function(x){fsd(x,na.rm=TRUE)/sqrt(fnobs(x))}),give.names=TRUE)
arim$subplot<-arim$pho
arim$bg<-ifelse(arim$plot==3,4,2)
arim$pch<-ifelse(arim$subplot=='C',21,24)
arim$col<-ifelse(arim$subplot=='C',1, arim$bg)

quartz(w=4,h=4)

df<-arim[arim$year%in%2001:2024,]

plot(NA,xlim=c(2001,2024),ylim=c(2,17),xlab='',ylab='',xaxt='n',yaxt='n')
mtext('Rosinedal 2001-2024',3,line=0,adj=1,cex=1)

for (i in 1:nrow(df)){
	lines(rep(df$year[i],2),df$m.bai[i]+c(-1,1)*df$se.bai[i],lwd=0.3)
	}
points(m.bai~year,df[df$plot==3&df$subplot=='C',],bg=bg,pch=pch,col=col,type='o')
points(m.bai~year,df[df$plot==3&df$subplot=='P',],bg=bg,pch=pch,col=col,type='o')
points(m.bai~year,df[df$plot==2&df$subplot=='C',],bg=bg,pch=pch,col=col,type='o')
points(m.bai~year,df[df$plot==2&df$subplot=='P',],bg=bg,pch=pch,col=col,type='o')

axis(1,seq(1990,2030,by=5),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)
axis(2,seq(0,20,by=5),tck=.02,label=TRUE,mgp=c(0,0,0),lwd=.3)

mtext(expression(paste('Year',sep='')),1,line=1.2)
mtext(expression(paste('Basal area increment',sep='')),2,line=2)
mtext(expression(paste('(cm'^2,' tree'^-1, ' y'^-1,')',sep='')),2,line=.8)

legend('topleft',c('FC','FP','RC','RP'),pt.bg=c(2,2,4,4),pch=c(21,24,21,24),col=c(1,2,1,4),box.col=0)






