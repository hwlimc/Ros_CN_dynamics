source('/Users/hlim/Documents/Rwork/functions.R')
setwd("/Users/hlim/Documents/Sweden/1_NPPw_Rosinedal/Rwork")
toff<-read.table("DH.increment_2011_Ro.txt",header=TRUE,sep="\t")
head(toff)
toff<-melt(toff,id=c("Snr","H","D","trt"))
colnames(toff)[5]<-c("year")
toff$year<-as.numeric(substr(toff$year,3,6))
toff<-join(toff[1:120,],toff[121:240,],by=c("year","trt","Snr","H","D"))
colnames(toff)[6:7]<-c("dH","dD")
toff$HD<-toff$dH/toff$dD


ws<-read.table("degero_flux_data.txt",header=TRUE,sep="\t")
ws$ws<-as.numeric(ws$WindSpeed_mPerS)
ws$Date<-as.Date(ws$Date)
ws$year<-substr(ws$Date,1,4)
ws$month<-substr(ws$Date,6,7)
ws$day<-substr(ws$Date,9,10)


ws<-ws[ws$Date>as.Date("2002-04-20"),]
ws<-ws[!(ws$Date>as.Date("2002-09-17")&ws$Date<as.Date("2003-05-10")),]
ws<-ws[!(ws$Date>as.Date("2003-09-25")&ws$Date<as.Date("2004-04-30")),]
ws<-ws[!(ws$Date>as.Date("2004-10-06")&ws$Date<as.Date("2005-05-20")),]
ws<-ws[!(ws$Date>as.Date("2005-10-14")&ws$Date<as.Date("2006-04-24")),]
ws<-ws[!(ws$Date>as.Date("2006-10-08")&ws$Date<as.Date("2007-05-17")),]
ws<-ws[!(ws$Date>as.Date("2007-10-07")&ws$Date<as.Date("2008-04-27")),]
ws<-ws[!(ws$Date>as.Date("2008-10-13")&ws$Date<as.Date("2009-04-30")),]
ws<-ws[!(ws$Date>as.Date("2009-09-27")&ws$Date<as.Date("2010-05-12")),]
ws<-ws[!(ws$Date>as.Date("2010-10-09")&ws$Date<as.Date("2011-04-21")),]
ws<-ws[!(ws$Date>as.Date("2011-10-06")&ws$Date<as.Date("2012-05-15")),]
ws<-ws[!(ws$Date>as.Date("2012-10-07")&ws$Date<as.Date("2013-05-12")),]
ws<-ws[!(ws$Date>as.Date("2013-10-13")&ws$Date<as.Date("2014-05-15")),]
ws<-ws[!(ws$Date>as.Date("2014-10-06")&ws$Date<as.Date("2015-05-05")),]
ws<-ws[!(ws$Date>as.Date("2015-10-04")),]

ws.m<-aggregate(ws~year,ws,mean,na.rm=TRUE)
ws.sd<-aggregate(ws~year,ws,sd,na.rm=TRUE)
ws.y<-join(ws.m,ws.sd,by="year")
NPP<-read.table("/Users/hlim/Documents/Sweden/2_N-deposition/Rwork/NPP_ÅRÅ.txt",header=TRUE)
NPP<-NPP[NPP$site=="Ro",]
NPP$year<-NPP$year+2005
NPP<-join(NPP,ws.y,by="year")
NPP$N<-as.factor(NPP$N)
coeT<-summary(lm(NPPw~T+N,NPP))
NPP$Res.t<-coeT$res
coeP<-summary(lm(Res.t~P*N,NPP[NPP$N!=0,]))
NPP$Res.p[NPP$N!=0]<-coeP$res
NPP$Res.p[NPP$N==0]<-NPP$Res.t[NPP$N==0]
coeW<-summary(lm(Res.p~ws,NPP[NPP$N==8.75,]))
NPPmRo<-summaryBy(NPPw+Res.t+Res.p~year+N+T+P+ws,NPP,FUN=function(x){c(m=mean(x),sd=sd(x))})

for (i in c(0,2,8.75)){
	print(i)
print(summary(lm(NPPw~T,NPP[NPP$N==i,]))$r.s)
print(summary(lm(Res.t~P,NPP[NPP$N==i,]))$r.s)
print(summary(lm(Res.p~ws,NPP[NPP$N==i,]))$r.s)}


par(mfrow=c(1,3))
plot(NA,xlim=c(9.2,13.4),ylim=c(100,380),xlab="Temperature during the growing season (°C)",ylab="wood NPP (g C m-2 yr-1)")
for(i in 1:24){
	lines(rep(NPPmRo$T[i],2), NPPmRo$NPPw.m[i]+c(-1,1)*NPPmRo$NPPw.sd[i],lwd=0.3)}
for(i in c(0,2,8.75)){
points(NPPw.m~T,NPPmRo[NPPmRo$N==i,],bg=ifelse(i==0,"white",ifelse(i==2,8,1)),pch=21,lwd=0.5,cex=1.2)
curve(coeT$coe[1]+ifelse(i==0,0,ifelse(i==2,coeT$coe[3],coeT$coe[4]))+coeT$coe[2]*x,xlim=c(min(NPP$T),max(NPP$T)),add=TRUE)}
text(10,350,"R2=0.34 for C  ")
text(10,330,"R2=0.49 for LF")
text(10,310,"R2=0.22 for HF")

plot(NA,xlim=c(300,430),ylim=c(-100,100),xlab="Precipitation during the growing season (mm)",ylab="Residuals (g C m-2 yr-1)")
for(i in 1:24){
	lines(rep(NPPmRo$P[i],2), NPPmRo$Res.t.m[i]+c(-1,1)*NPPmRo$Res.t.sd[i],lwd=0.3)}
for(i in c(0,2,8.75)){
points(Res.t.m~P,NPPmRo[NPPmRo$N==i,],bg=ifelse(i==0,"white",ifelse(i==2,8,1)),pch=21,lwd=.5,cex=1.2)
curve(ifelse(i==0,0,ifelse(i==2,coeP$coe[1],coeP$coe[1]+coeP$coe[3]))+ifelse(i==0,0,ifelse(i==2,coeP$coe[2],coeP$coe[2]+coeP$coe[4]))*x,xlim=c(min(NPP$P),max(NPP$P)),add=TRUE)}
text(320,80,"R2=0.51 for LF")
text(320,70,"R2=0.49 for HF")

plot(NA,xlim=c(2.1,2.5),ylim=c(-100,100),xlab="Mean wind speed (m s-1)",ylab="Residuals (g C m-2 yr-1)")
for(i in 1:24){
	lines(rep(NPPmRo$ws[i],2), NPPmRo$Res.p.m[i]+c(-1,1)*NPPmRo$Res.p.sd[i],lwd=0.3)}
for(i in c(0,2,8.75)){
points(Res.p.m~ws,NPPmRo[NPPmRo$N==i,],bg=ifelse(i==0,"white",ifelse(i==2,8,1)),pch=21,lwd=.5,cex=1.2)
curve(ifelse(i==8.75,coeW$coe[1],0)+ifelse(i==8.75,coeW$coe[2],0)*x,xlim=c(min(NPP$ws),max(NPP$ws)),add=TRUE)}
text(2.16,80,"R2=0.34 for HF")


par(mfrow=c(1,2))
for(i in c("Ro","Åh")){
plot(NPPw~ws,NPP[NPP$site==i,],bg=N,pch=21)}


NPP$N<-as.factor(NPP$N)
for(i in c("Ro","Åh")){
print(summary(lm(NPPw~T+N,NPP[NPP$site==i,])))
Res<-summary(lm(NPPw~T+N,NPP[NPP$site==i,]))$res
print(summary(lm(Res~P*N,NPP[NPP$site==i,])))
}

for(i in c("Ro","Åh")){
print(summary(lm(NPPw~T+N,NPP[NPP$site==i,])))
Res<-summary(lm(NPPw~T+N,NPP[NPP$site==i,]))$res
print(summary(lm(Res~ws*N,NPP[NPP$site==i,])))
}

par(mfrow=c(1,3))
plot(ws~P,NPP[c(1+3*(0:7)),])
plot(ws~T,NPP[c(1+3*(0:7)),])
plot(P~T,NPP[c(1+3*(0:7)),])





toff<-join(toff,met,by="year")
plot(P~ws,toff,col=trt)
plot(dH~dD,toff,col=0)
for (i in 1:12){
points(dH~dD,toff[toff$Snr==i,],col=trt,type='o')}

toff.m<-summaryBy(dH+dD+HD+ws+P+T+GS~trt+year,toff,FUN=function(x){c(m=mean(x),se=se(x))})
toff.m$dD.m<-toff.m$dD.m*1000	#mm
toff.m$dD.se<-toff.m$dD.se*1000	#mm


DHf<-read.table("/Users/hlim/Documents/Sweden/3_Soil warming_Flakaliden/Rwork/harvest_2013_D.H.txt",header=TRUE,sep="\t")
DHf$D.1<-c(rep(NA,24), DHf$D[1:(552-24)])
DHf$H.1<-c(rep(NA,24), DHf$H[1:(552-24)])
DHf$dH<-DHf$H-DHf$H.1
DHf$dD<-DHf$D-DHf$D.1
DHf$BAI<-((DHf$D/2)^2-(DHf$D.1/2)^2)*pi


plot(dH~dD, DHf,col=0)
for (i in 1:24){
points(dH~dD, DHf[DHf$Snr==i,],col=fer,type='o')}
ftoff.m<-summaryBy(dH+dD+BAI~fer+year,DHf,FUN=function(x){c(m=mean(x),se=se(x))})
ftoff.m$dD.m<-ftoff.m$dD.m*1000	#mm
ftoff.m$dD.se<-ftoff.m$dD.se*1000	#mm

par(mfrow=c(1,2))
plot(NA,xlim=c(0.15,2),ylim=c(0.16,0.5),xlab="Diameter increment at 1.3 m (mm)",ylab="Height increment (cm)")
for (i in 1:20){
	lines(rep(toff.m$dD.m[i],2),toff.m$dH.m[i]+c(-1,1)*toff.m$dH.se[i],lwd=0.4)}
for (i in 1:20){
	lines(toff.m$dD.m[i]+c(-1,1)*toff.m$dD.se[i],rep(toff.m$dH.m[i],2),lwd=0.4)}
points(dH.m~dD.m,toff.m,bg=ifelse(toff.m$trt=="F",1,"white"),pch=21,lwd=0.45)
mtext("Pinus sylvestris",side=3,adj=0,line=0,font=4,cex=0.9)
mtext("Rosinedal, Sweden 2002-2011",side=3,adj=1,line=0,font=2,cex=0.9)  

plot(dH.m~dD.m,ftoff.m,bg=ifelse(ftoff.m$fer=="I","white",1),pch=21,xlim=c(1.8,5.6),ylim=c(0.18,0.58),xlab="Diameter increment at 1.3 m (mm)",ylab="Height increment (cm)")
for (i in 1:72){
	lines(rep(ftoff.m$dD.m[i],2),ftoff.m$dH.m[i]+c(-1,1)*ftoff.m$dH.se[i],lwd=0.4)}
for (i in 1:72){
	lines(ftoff.m$dD.m[i]+c(-1,1)*ftoff.m$dD.se[i],rep(ftoff.m$dH.m[i],2),lwd=0.4)}
points(dH.m~dD.m,ftoff.m,bg=ifelse(ftoff.m$fer=="I","white",1),pch=21,lwd=0.45)
mtext("Picea abies",side=3,adj=0,line=0,font=4,cex=0.9)
mtext("Flakaliden, Sweden 1996-2013",side=3,adj=1,line=0,font=2,cex=0.9)  

