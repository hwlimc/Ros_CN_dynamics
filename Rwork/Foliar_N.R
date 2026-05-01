source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd("/Users/hyli0001/Documents/wd/Rosinedal_2018/Rwork")

wth<-read.table("Svartberget_flux_data.txt",head=TRUE,sep='\t')
fn1<-read.table("Foliar_N.txt",head=TRUE)
fn<-join(wth,fn1,by=c('year'))
fn$pt<-'N'
fn$pt[fn$year>2015]<-'C'
fn$pt[fn$rep>3]<-'T'


summary(lm(N~P*as.factor(plot)*pt+as.factor(year),fn[fn$year>2015,]))
summary(lm(P~pt,fn[fn$year==2018&fn$plot==2,]))
summary(lm(P~pt,fn[fn$year==2018&fn$plot==3,]))

fnm<-summaryBy(N~year+plot+precipitation,fn,FUN=me)
fpm<-summaryBy(P~year+plot+pt,fn[fn$year>2015,],FUN=me)

par(mfrow=c(1,2))
plot(N.m~year,fnm,col=0,ylab="Foliar [N] (mg/g)",ylim=c(5,26),xlim=c(2006,2018))
for (i in 1:length(fnm[,1])){
	lines(c(-1,1)*N.se[i]+N.m[i]~rep(year[i],2),fnm)}
points(N.m~year,fnm[fnm$plot=="2",],bg=1,pch=21,type='o')
points(N.m~year,fnm[fnm$plot=="3",],bg='white',pch=21,type='o')
#text(2013.5,25,paste("sampled in May ->"))

par(mfrow=c(1,2))
plot(N.m~precipitation,fnm,col=0,ylab="Foliar [N] (mg/g)",ylim=c(5,26),xlim=c(300,430))
for (i in 1:length(fnm[,1])){
	lines(c(-1,1)*N.se[i]+N.m[i]~rep(precipitation[i],2),fnm)}
points(N.m~ precipitation,fnm[fnm$plot=="2",],bg=1,pch=21)
points(N.m~ precipitation,fnm[fnm$plot=="3",],bg='white',pch=21)


par(mfrow=c(1,2))
plot(N.m~year,fnm,col=0,ylab="Foliar [N] (mg/g)",ylim=c(5,26))
for (i in 1:length(fnm[,1])){
	lines(c(-1,1)*N.se[i]+N.m[i]~rep(year[i],2),fnm)}
points(N.m~year,fnm[fnm$plot=="2",],bg=1,pch=21)
points(N.m~year,fnm[fnm$plot=="3",],bg='white',pch=21)
#text(2013.5,25,paste("sampled in May ->"))

par(mfrow=c(1,2))
plot(N.m~precipitation,fnm,col=0,ylab="Foliar [N] (mg/g)",ylim=c(5,26),xlim=c(300,430))
for (i in 1:length(fnm[,1])){
	lines(c(-1,1)*N.se[i]+N.m[i]~rep(precipitation[i],2),fnm)}
points(N.m~ precipitation,fnm[fnm$plot=="2",],bg=1,pch=21)
points(N.m~ precipitation,fnm[fnm$plot=="3",],bg='white',pch=21)



plot(P.m~year,fpm,col=0,ylab="Foliar [P] (mg/g)",ylim=c(.5,2),xlim=c(2015,2019))
for (i in 1:length(fpm[,1])){
	lines(c(-1,1)*P.se[i]+P.m[i]~rep(year[i],2),fpm)}
points(P.m~year,fpm[fpm$plot=="2"&fpm$pt=='C',],bg=1,pch=21)
points(P.m~year,fpm[fpm$plot=="3"&fpm$pt=='C',],bg='white',pch=21)
points(P.m~year,fpm[fpm$plot=="2"&fpm$pt=='T',],bg=1,pch=24)
points(P.m~year,fpm[fpm$plot=="3"&fpm$pt=='T',],bg='white',pch=24)
text(2018,1.9,paste("*"),cex=1.5)