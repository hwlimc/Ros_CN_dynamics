setwd("/Users/hyli0001/Documents/Sweden/Rosinedal 2018/Rwork")
ecr<-read.table("Ros_EC_gCm2.txt",head=TRUE)
ecm<-melt(ecr,id=c('Date'))
ecm$var<-substr(ecm$variable,1,3)
ecm$plot<-substr(ecm$variable,5,5)
ecm$variable<-NULL
ec<-data.frame(cast(ecm,plot+Date~var,value='value'))
ec$year<-as.numeric(substr(ec$Date,1,4))
ec$month<-as.numeric(substr(ec$Date,6,7))
ec$day<-as.numeric(substr(ec$Date,9,10))
ec$doy<-as.numeric(strftime(ec$Date, format = "%j"))
ec$CUE<-(-ec$NEE)/ec$GPP



par(mfrow=c(2,3))
for (i in c(2006,2015:2019)){
plot(GPP~doy,ec,col=0,ylab='GPP')
points(GPP~doy,ec[ec$year==i,],col=plot,lwd=.3)}

par(mfrow=c(2,3))
for (i in c(2006,2015:2019)){
plot(NEE~doy,ec,col=0,ylab='NEE')
points(NEE~doy,ec[ec$year==i,],col=plot,lwd=.3)}

par(mfrow=c(2,3))
for (i in c(2006,2015:2019)){
plot(CUE~doy,ec,col=0,ylab='CUE',ylim=c(-2,2))
points(CUE~doy,ec[ec$year==i,],col=plot,lwd=.3)}

