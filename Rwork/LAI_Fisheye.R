source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd('/Users/hyli0001/Documents/Sweden/Rosinedal 2018/Rwork')

####	 SLA analysis
fs<-read.table('Fisheye_LAI.txt',sep='\t',head=TRUE)
head(fs)
str(fs)
fs$year<-as.numeric(substr(fs$Date,1,4))
fs$month<-as.numeric(substr(fs$Date,6,7))
fs$season<-ifelse(fs$month>7,2,1)
fs$y.s<-fs$year+(as.numeric(fs$season)-1)*0.5
fs<-fs[order(fs$y.s,fs$plot,fs$loc),]

fsm<-summaryBy(LAI_ALEX+LAI_2015~plot+y.s+year+season,fs,FUN=me)
plot(LAI_ALEX.m~y.s,fsm,col=0,type='o')
points(LAI_ALEX.m~y.s,fsm[fsm$season==1&fsm$plot==2,],col=plot,type='o')
points(LAI_2015.m~y.s,fsm[fsm$season==1&fsm$plot==2,],bg=plot,pch=21,type='o')

plot(LAI_ALEX.m~y.s,fsm,col=0,type='o')
points(LAI_ALEX.m~y.s,fsm[fsm$season==1&fsm$plot==3,],col=1,type='o')
points(LAI_2015.m~y.s,fsm[fsm$season==1&fsm$plot==3,],bg=1,pch=21,type='o')

plot(LAI_ALEX.m~LAI_2015.m,fsm)

points(LAI_ALEX.m~y.s,fsm[fsm$season==2&fsm$plot==2,],col=plot,pch=2,type='o')
points(LAI_ALEX.m~y.s,fsm[fsm$season==2&fsm$plot==3,],col=1,pch=2,type='o')

fsm[fsm$y.s%in%c(2006,2011.5,2018.5),]

par(mfrow=c(3,4))
loc<-unique(fs$loc)
for (i in 1:12){
plot(LAI_ALEX~y.s,fs[fs$loc==loc[i],],col=0,type='o')
points(LAI_ALEX~y.s,fs[fs$season==1&fs$loc==loc[i]&fs$plot==2,],col=plot,type='o')
points(LAI_ALEX~y.s,fs[fs$season==1&fs$loc==loc[i]&fs$plot==3,],col=1,type='o')}