source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd('/Users/hyli0001/Documents/Sweden/Rosinedal 2018/Rwork')
rg1<-read.table("Ring_width_Ros_2011_rawdata.txt",head=TRUE,sep='\t')
rg2<-rg1[,c('Snr','disk','path',paste('X',1:71,sep=''))]
head(rg)
rg<-melt(rg2,id=c('Snr','disk','path'))
rg$wd<-rg$value
rg$nr<-as.integer(substr(rg$variable,2,3))
rg$dh<-rg$disk_location
rw<-rg[!is.na(rg$wd),c('Snr','disk','path','nr','wd')]

rw$rn<-abs(rw$nr-ave(rw$wd,rw$Snr,rw$disk,rw$path,FUN=n)-1)
wr<-rw[rw$rn<31,]
wr$wds<-ave(wr$wd,wr$Snr,wr$disk,wr$path,FUN=sum,na.rm=TRUE)
wr$rwd<-wr$wd/wr$wds


par (mfrow=c(3,4))
for (i in 1:12){
plot(rwd~rn,wr[wr$Snr==i&wr$disk==1.3,],col=path,main=i)}

# disk 1.3 -- nr 5,6,7,8,9,10,11
wr$nn[wr$disk==1.3&wr$Snr%in%5:11]<-wr$rn[wr$disk==1.3&wr$Snr%in%5:11]-1
wr$nn[wr$disk==1.3&wr$Snr%out%5:11]<-wr$rn[wr$disk==1.3&wr$Snr%out%5:11]

for (i in 1:12){
plot(rwd~nn,wr[wr$Snr==i&wr$disk==1.3&wr$nn!=0,],col=path,main=i)}

wr[wr$Snr==7&wr$disk==1.3&wr$nn==3&wr$path==3,'wd']<-NA


for (i in 1:12){
plot(rwd~rn,wr[wr$Snr==i&wr$disk!=1.3,],col=path,main=i)}
head(wr)
# disk base of crown -- nr 5,6,7,8, ,10,11,12
wr$nn[wr$disk!=1.3&wr$Snr%in%c(5:8,10:12)]<-wr$rn[wr$disk!=1.3&wr$Snr%in%c(5:8,10:12)]-1
wr$nn[wr$disk!=1.3&wr$Snr%out%c(5:8,10:12)]<-wr$rn[wr$disk!=1.3&wr$Snr%out%c(5:8,10:12)]

for (i in 1:12){
plot(rwd~nn,wr[wr$Snr==i&wr$disk!=1.3&wr$nn!=0,],col=path,main=i)}
wr[wr$Snr==11&wr$disk!=1.3&wr$nn==3&wr$path==3,'wd']<-NA
wr$year<-2012-wr$nn
rev_width<-wr[wr$year!=2012,c('Snr','disk','path','wd','year','rwd')]
for (i in 1:12){
plot(rwd~year,rev_width[rev_width$Snr==i&rev_width$disk!=1.3,],col=path,main=i)}
# write.table(rev_width,'Ring_width_2011_corrected.txt',sep='\t',quote=FALSE,row.names=FALSE)
