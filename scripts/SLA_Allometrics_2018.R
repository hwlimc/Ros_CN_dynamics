source('../functions.R')

getwd()
#### Foliar N analysis
fn11<-read.table("../data/Foliar_CN_2011_harvests.txt",head=TRUE,sep='\t')
# fn<-read.table("../foliage_nutrients.txt",head=TRUE,sep='\t')
# fn$PN<-fn$P/fn$N
# fnm<-summaryBy(N+P+PN~year+plot,fn,FUN=me)

####	 SLA analysis -- samples from 2018 harvests
scan.la<-read.table("../data/AREA_sample_needle_scan_Ro_2018.txt",head=TRUE)
scan.la$area.cm<-scan.la$area_in*(2.54*2.54)

sla0<-summaryBy(area.cm~Snr+Brnr+age,scan.la,FUN=sum,keep.names=TRUE)
sla1<-summaryBy(area.cm~Snr+Brnr+age,scan.la,FUN=n)
sla1$rn<-sla1$area.cm.n
sla1$area.cm.n<-NULL

br.lm<-read.table("../data/DW_sample_needle_scan_branch_base_Ro_2018.txt",head=TRUE,sep='\t')
sla2<-melt(br.lm[,c('Snr','strata','Brnr','scan.c0','scan.c1','scan.c2','scan.c3')],id=c('Snr','strata','Brnr'))
sla2$age<-as.numeric(substr(sla2$variable,7,7))
sla2$dw.g<-sla2$value
sla2$variable<-NULL
sla2$value<-NULL

sla3<-melt(br.lm[,c('Snr','strata','Brnr','rm.c0','rm.c1','rm.c2','rm.c3')],id=c('Snr','strata','Brnr'))
sla3$age<-as.numeric(substr(sla3$variable,5,5))
sla3$rm.g<-sla3$value
sla3$variable<-NULL
sla3$value<-NULL

sla<-merge(sla3,merge(sla2,merge(sla1,sla0,by=c('Snr','Brnr','age')),by=c('Snr','Brnr','age')),by=c('Snr','strata','Brnr','age'))
sla<-sla[order(sla$Snr,sla$Brnr,sla$age),]
sla$trt[sla$Snr%in%1:6]<-'F'
sla$trt[sla$Snr%in%7:12]<-'C'
sla$age<-as.factor(sla$age)
sla$strata<-as.factor(sla$strata)
sla$rm.g[is.na(sla$rm.g)]<-0
sla$sla<-sla$area.cm/sla$dw.g
sla$dw.samples<-sla$rm.g+sla$dw.g
sla$area.n<-sla$area.cm/sla$rn
sla$dw.n<-sla$dw.g/sla$rn
sla$age.class<-ifelse(sla$age==0,0,'>0')

# SLA for 2011 and 2018
sla2011<-read.table("../data/Ros_SLA_2011.txt",head=TRUE)
head(sla2011)
sla2011$area.cm<-sla2011$area.mm2/100
sla2011$sla<-sla2011$area.cm/sla2011$dw.g
sla2011$area.mm2<-NULL
sla2011$area.n<-sla2011$area.cm/sla2011$rn
sla2011$dw.n<-sla2011$dw.g/sla2011$rn

sla11<-sla2011
sla11$age<-ifelse(sla11$age==4,3,sla11$age)
sla11$yr<-11
sla$plot<-ifelse(sla$trt=='C',3,2)
sla$yr<-18
sla18<-sla[,colnames(sla11)]
if(is.factor(sla18$age)){
	sla18$age<-as.numeric(sla18$age)-1}

slaf<-rbind(sla18,sla11)
slaf$ag<-ifelse(slaf$age>1,2,slaf$age)
slaf$bg<-ifelse(slaf$plot==3,'white',1)
slaf$pch<-ifelse(slaf$yr==11,21,24)
# # write.table(slaf,'Ros_specific_leaf_area_2018.txt',quote=FALSE,sep='\t',row.names=TRUE)
# summary(lm(area.cm~dw.g*as.factor(ag)*factor(plot),slaf[slaf$strata==1,]))
# summary(lm(area.cm~dw.g*as.factor(ag)*factor(plot),slaf[slaf$strata==2,]))
# summary(lm(area.cm~dw.g*as.factor(ag)*factor(plot),slaf[slaf$strata==3,]))
# summary(lm(area.cm~dw.g*as.factor(ag)*factor(plot),slaf[slaf$strata==4,]))
# summary(lm(area.cm~dw.g*as.factor(ag)*factor(plot),slaf[slaf$strata==5,]))
# 
# quartz()
# par(mfrow=c(3,5))
# for (i in 1:5) {
# sdf<-slaf[slaf$yr==11&slaf$strata==i,]
# rg<-range(sdf$dw.n)
# breaks <- seq(rg[1],rg[2],length.out=50)
# mse<-1:50
# for(k in 1:50){
#  piecewise<-lm(area.n~as.factor(plot)*dw.n*(dw.n < breaks[k]) + as.factor(plot)*dw.n*(dw.n>=breaks[k])+0,sdf)
#  if(summary(piecewise)$coe[4,4]<0.05){
#  	mse[k] <- summary(piecewise)[6]}else{
#  		mse[k]<-NA}
# }
# mse<-as.numeric(mse)
# mb<-data.frame(mse,breaks)
# mb<-mb[!is.na(mb$mse),]
# mn.br<-max(mb$breaks[mb$mse==min(mb$mse)])
# s.st<-ifelse(n(mb$breaks[mb$breaks<mn.br])<n(mb$breaks[mb$breaks>=mn.br]),2,1)
# if(s.st==2){
# 	slaf[slaf$yr==11&slaf$strata==i&slaf$dw.n>=mn.br,'pwr']<-'Y'}else{
# 		if(s.st==1){
# 		slaf[slaf$yr==11&slaf$strata==i&slaf$dw.n<=mn.br,'pwr']<-'Y'}}
# slaf[slaf$yr==11&slaf$strata==i,]
# plot(area.n~dw.n,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.003,.033),ylim=c(.2,1))
# points(area.n~dw.n,slaf[slaf$yr==11&slaf$strata==i&slaf$pwr=='Y',],col=2,cex=.3)
# lines(c(mn.br, mn.br),c(0,50))
# mtext(paste('stratum',i,'2011'),cex=.8)
# }
# for (i in 1:5) {
# sdf<-slaf[slaf$yr==18&slaf$strata==i,]
# rg<-range(sdf$dw.n)
# breaks <- seq(rg[1],rg[2],length.out=50)
# mse<-1:50
# for(k in 1:50){
#  piecewise<-lm(area.n~as.factor(plot)*dw.n*(dw.n < breaks[k]) + as.factor(plot)*dw.n*(dw.n>=breaks[k])+0,sdf)
#  if(summary(piecewise)$coe[4,4]<0.05){
#  	mse[k] <- summary(piecewise)[6]}else{
#  		mse[k]<-NA}
# }
# mse<-as.numeric(mse)
# mb<-data.frame(mse,breaks)
# mb<-mb[!is.na(mb$mse),]
# mn.br<-max(mb$breaks[mb$mse==min(mb$mse)])
# slaf[slaf$yr==18&slaf$strata==i&slaf$dw.n<=mn.br,'pwr']<-'Y'
# plot(area.n~dw.n,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.003,.033),ylim=c(.2,1))
# points(area.n~dw.n,slaf[slaf$yr==18&slaf$strata==i&slaf$pwr=='Y',],col=4,cex=.3)
# lines(c(mn.br, mn.br),c(0,50))
# mtext(paste('stratum',i,'2018'),cex=.8)
# }
# 
# for (i in 1:5) {
# sdf<-slaf[!is.na(slaf$pwr)&slaf$strata==i,]
# plot(area.n~dw.n,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.003,.033),ylim=c(.2,1))
# mtext(paste('stratum',i,'2011 and 2018'),cex=.8)
# }
# 
# 
# for (i in 1:5) {
# sdf<-slaf[!is.na(slaf$pwr)&slaf$strata==i,]
# print(summary(lm(area.n~dw.n*as.factor(yr)+0,sdf)))}
# 
# ###
# 
# quartz()
# par(mfrow=c(3,5))
# for (i in 1:5) {
# sdf<-slaf[slaf$yr==11&slaf$strata==i,]
# rg<-range(sdf$dw.g)
# breaks <- seq(rg[1],rg[2],length.out=50)
# mse<-1:50
# for(k in 1:50){
#  piecewise<-lm(area.cm~as.factor(plot)*dw.g*(dw.g < breaks[k]) + as.factor(plot)*dw.g*(dw.g>=breaks[k])+0,sdf)
#  if(summary(piecewise)$coe[4,4]<0.05){
#  	mse[k] <- summary(piecewise)[6]}else{
#  		mse[k]<-NA}
# }
# mse<-as.numeric(mse)
# mb<-data.frame(mse,breaks)
# mb<-mb[!is.na(mb$mse),]
# mn.br<-max(mb$breaks[mb$mse==min(mb$mse)])
# s.st<-ifelse(n(mb$breaks[mb$breaks<mn.br])<n(mb$breaks[mb$breaks>=mn.br]),2,1)
# if(s.st==2){
# 	slaf[slaf$yr==11&slaf$strata==i&slaf$dw.g>=mn.br,'pwr']<-'Y'}else{
# 		if(s.st==1){
# 		slaf[slaf$yr==11&slaf$strata==i&slaf$dw.g<=mn.br,'pwr']<-'Y'}}
# slaf[slaf$yr==11&slaf$strata==i,]
# plot(area.cm~dw.g,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.03,1.4),ylim=c(.2,75))
# points(area.cm~dw.g,slaf[slaf$yr==11&slaf$strata==i&slaf$pwr=='Y',],col=2,cex=.3)
# lines(c(mn.br, mn.br),c(0,50))
# mtext(paste('stratum',i,'2011'),cex=.8)
# }
# for (i in 1:5) {
# sdf<-slaf[slaf$yr==18&slaf$strata==i,]
# rg<-range(sdf$dw.g)
# breaks <- seq(rg[1],rg[2],length.out=50)
# mse<-1:50
# for(k in 1:50){
#  piecewise<-lm(area.cm~as.factor(plot)*dw.g*(dw.g < breaks[k]) + as.factor(plot)*dw.g*(dw.g>=breaks[k])+0,sdf)
#  if(summary(piecewise)$coe[4,4]<0.05){
#  	mse[k] <- summary(piecewise)[6]}else{
#  		mse[k]<-NA}
# }
# mse<-as.numeric(mse)
# mb<-data.frame(mse,breaks)
# mb<-mb[!is.na(mb$mse),]
# mn.br<-max(mb$breaks[mb$mse==min(mb$mse)])
# slaf[slaf$yr==18&slaf$strata==i&slaf$dw.g<=mn.br,'pwr']<-'Y'
# plot(area.cm~dw.g,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.03,1.4),ylim=c(.2,75))
# points(area.cm~dw.g,slaf[slaf$yr==18&slaf$strata==i&slaf$pwr=='Y',],col=4,cex=.3)
# lines(c(mn.br, mn.br),c(0,50))
# mtext(paste('stratum',i,'2018'),cex=.8)
# }
# 
# for (i in 1:5) {
# sdf<-slaf[!is.na(slaf$pwr)&slaf$strata==i,]
# plot(area.cm~dw.g,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.03,1.4),ylim=c(.2,75))
# mtext(paste('stratum',i,'2011 and 2018'),cex=.8)
# }
# 
# for (i in 1:5) {
# sdf<-slaf[!is.na(slaf$pwr)&slaf$strata==i,]
# print(summary(lm(area.cm~dw.g*as.factor(yr)+0,sdf)))}


# quartz()
# par(mfrow=c(4,5))
# par(mai=c(.3,.3,.3,.1))
# for (i in 1:5) {
# sdf<-slaf[slaf$strata==i,]
# plot(area.n~dw.n,sdf,col=yr,bg=(plot-2),pch=21,xlim=c(.003,.033),ylim=c(.2,1))
# mtext(paste('stratum',i,'Linear model'),cex=.8)
# }
# for (i in 1:5) {
# sdf<-slaf[slaf$strata==i,]
# m1<-lm(area.n~dw.n*as.factor(plot)+0,sdf)
# sdf$fit<-m1$fit
# sdf$res<-m1$res
# plot(res~fit,sdf,col=yr,bg=(plot-2),pch=21,main=round(summary(m1)$r.s,2))
# }
# 
# for (i in 1:5) {
# sdf<-slaf[slaf$strata==i,]
# plot(log(area.n)~log(dw.n),sdf,col=yr,bg=(plot-2),pch=21,xlim=c(-5.5,-3.4),ylim=c(-1.5,0))
# mtext(paste('stratum',i,'Log-Log model'),cex=.8)
# }
# for (i in 1:5) {
# sdf<-slaf[slaf$strata==i,]
# m1<-lm(log(area.n)~log(dw.n)*as.factor(plot)+0,sdf)
# sdf$fit<-exp(m1$fit)
# sdf$res<-sdf$area.n-sdf$fit
# plot(res~fit,sdf,col=yr,bg=(plot-2),pch=21,main=round(summary(m1)$r.s,2))
# }
# 

sla.merge<-summaryBy(dw.n~plot+strata,slaf,FUN=me,keep.names=TRUE)
sla.merge$dw.n<-sla.merge$dw.n.m
sla.merge$dw.n.m<-NULL
for (i in 1:5) {
sdf<-slaf[slaf$strata==i,]
m1<-lm(area.n~dw.n+as.factor(plot),sdf)
sla.merge$area.n[sla.merge$strata==i]<-predict(m1,sla.merge[sla.merge$strata==i,],se.fit=TRUE)$fit
sla.merge$area.se[sla.merge$strata==i]<-predict(m1,sla.merge[sla.merge$strata==i,],se.fit=TRUE)$se.fit
}
sla.merge$sla.m<-sla.merge$area.n/sla.merge$dw.n
sla.merge$sla.mse<-sd.mtom(sla.merge$area.n,sla.merge$area.se,sla.merge$dw.n,sla.merge$dw.n.se)
sla.mg<-sla.merge[,c('plot','strata','sla.m','sla.mse')]
# plot(sla.m~strata,sla.mg[sla.mg$plot==2,],ylim=c(30,45))
# plot(sla.m~strata,sla.mg[sla.mg$plot==3,],ylim=c(30,45))
####	Drymass of sample branches
Brbs<-br.lm[,c('Snr','strata','Brnr','base.dw.g','D.br.d1','D.br.d2')]
Brbs$d.brd<-(Brbs$D.br.d1+Brbs$D.br.d2)/2
Brbs$D.br.d1<-NULL
Brbs$D.br.d2<-NULL

Sbr<-read.table("../data/DW_sample_branches_Ros_2018.txt",head=TRUE,sep='\t')
Sbr$d.brf<-(Sbr$F.br.d1+Sbr$F.br.d2)/2
Sbr$F.br.d1<-NULL
Sbr$F.br.d2<-NULL

Br<-merge(Sbr,Brbs,by=c('Snr','strata','Brnr'))
Fl<-cast(sla,Snr+strata+Brnr~age,value='dw.samples')
colnames(Fl)<-c('Snr','strata','Brnr','s0','s1','s2','s3')
Cr<-merge(Br,Fl,by=c('Snr','strata','Brnr'))
Cr[is.na(Cr)]<-0
Cr$Dbr<-Cr$br.dw.g+Cr$base.dw.g	#DW sample branches
Cr$Df0<-Cr$c0+Cr$s0
Cr$Df1<-Cr$c1+Cr$s1
Cr$Df2<-Cr$c2+Cr$s2
Cr$Df3<-Cr$c3+Cr$s3
Cr$Df<-Cr$Df0+Cr$Df1+Cr$Df2+Cr$Df3+Cr$remaining
Cr$Dcn<-Cr$cones
Cr$Ddb<-Cr$db.dw.g
Cr$plot<-ifelse(Cr$Snr<7,2,3)
sample.crw<-Cr[,c('Snr','strata','Brnr','d.brf','d.brd','Dbr','Df','Df0','Df1','Df2','Df3','Ddb','Dcn')]

smp.crw1<-sample.crw[sample.crw$Brnr%in%seq(1,9,2),]
colnames(smp.crw1)<-mcolname(sample.crw,3,0,'1')
smp.crw2<-sample.crw[sample.crw$Brnr%in%seq(2,10,2),]
colnames(smp.crw2)<-mcolname(sample.crw,3,0,'2')
smp.crw<-merge(smp.crw1,smp.crw2,by=c('Snr','strata'))
smp.crw$Brnr.x<-NULL
smp.crw$Brnr.y<-NULL

	## folage fraction in each age and strata
ffa<-smp.crw[,c("Snr","strata","Df1","Df01","Df11","Df21","Df31","Df2","Df02","Df12","Df22","Df32")]
ffa$Bf<-ave(ffa$Df1+ffa$Df2,ffa$Snr,FUN=sum)
ffa$f0<-ffa$Df01+ffa$Df02
ffa$f1<-ffa$Df11+ffa$Df12
ffa$f2<-ffa$Df21+ffa$Df22
ffa$f3<-ffa$Df1+ffa$Df2-(ffa$f0+ffa$f1+ffa$f2)
ffa$rf0<-ffa$f0/ffa$Bf
ffa$rf1<-ffa$f1/ffa$Bf
ffa$rf2<-ffa$f2/ffa$Bf
ffa$rf3<-ffa$f3/ffa$Bf

ff<-melt(ffa[,c('Snr','strata','rf0','rf1','rf2','rf3')],id=c('Snr','strata'))
ff$age<-substr(ff$variable,3,3)
ff$fr<-ff$value
ff$strata<-as.factor(ff$strata)
ff$variable<-NULL
ff$value<-NULL
ff$plot[ff$Snr%in%1:6]<-2
ff$plot[ff$Snr%in%7:12]<-3
ff<-ff[order(ff$plot,ff$Snr,ff$strata),]
fm<-summaryBy(fr~Snr+age+plot,ff,FUN=sum)
fm.age<-summaryBy(fr.sum~age+plot,fm,FUN=me)
fm$lgv<-1/fm$fr.sum
summaryBy(lgv~plot,fm[fm$age==0,],FUN=me)
head(ff)
ff11<-read.table('../data/Ros_Foliage_fraction_2011.txt',head=TRUE)
ff1<-melt(ff11,id=c('plot','Snr'))
ff1$strata<-substr(ff1$variable,3,3)
ff1$age<-substr(ff1$variable,8,8)
ff1$Bf<-ff1$value/1000
ff1$variable<-NULL
ff1$value<-NULL
ff1$fr<-ff1$Bf/ave(ff1$Bf,ff1$Snr,FUN=sum)
f1m<-summaryBy(fr~Snr+age+plot,ff1,FUN=sum)
f1m$lgv3<-1/f1m$fr.sum
summaryBy(lgv3~plot,f1m[f1m$age==0,],FUN=me)

ffm18<-summaryBy(fr~strata+plot,summaryBy(fr~Snr+strata+plot,ff,FUN=sum,keep.names=TRUE),FUN=me)
ffm11<-summaryBy(fr~strata+plot,summaryBy(fr~Snr+strata+plot,ff1,FUN=sum,keep.names=TRUE),FUN=me)
ffm18$year<-2018
ffm11$year<-2011
ffm<-rbind(ffm11,ffm18)

# SLA for litterfall 2018 and 2011
for (i in 1:12) for (j in 0:3) for (k in 1:5){
	ff$d.ft[ff$age==j&ff$Snr==i&ff$strata==k]<-ff$fr[ff$age==(j)&ff$Snr==i&ff$strata==k]-ifelse(j==3,0,ff$fr[ff$age==j+1&ff$Snr==i&ff$strata==k])}
ff$d.ft<-ifelse(ff$d.ft<0,0,ff$d.ft)
ff$fr.fall<-ff$d.ft/ave(ff$d.ft,ff$Snr,FUN=sum)
fr.f18<-summaryBy(fr.fall~plot+Snr+strata,ff,FUN=sum,keep.names=TRUE)
fr.f18$year<-2018

for (i in 1:12) for (j in 0:4) for (k in 1:5){
	ff1$d.ft[ff1$age==j&ff1$Snr==i&ff1$strata==k]<-ff1$fr[ff1$age==(j)&ff1$Snr==i&ff1$strata==k]-ifelse(j==4,0,ff1$fr[ff1$age==j+1&ff1$Snr==i&ff1$strata==k])}
ff1$d.ft<-ifelse(ff1$d.ft<0,0,ff1$d.ft)
ff1$fr.fall<-ff1$d.ft/ave(ff1$d.ft,ff1$Snr,FUN=sum)
fr.f11<-summaryBy(fr.fall~plot+Snr+strata,ff1,FUN=sum,keep.names=TRUE)
fr.f11$year<-2011

fr.f<-rbind(fr.f11,fr.f18)
fr.fm<-summaryBy(fr.fall~year+plot+strata,fr.f,FUN=me)


fsla<-merge(merge(fr.fm,sla.mg,by=c('strata','plot'),all=TRUE),ffm,by=c('year','strata','plot'))
fsla$fsla<-fsla$fr.fall.m*fsla$sla.m
fsla$fsla.se<-sd.mtom(fsla$fr.fall.m,fsla$fr.fall.se,fsla$sla.m,fsla$sla.mse)
fsla$sla<-fsla$fr.m*fsla$sla.m
fsla$sla.se<-sd.mtom(fsla$fr.m,fsla$fr.se,fsla$sla.m,fsla$sla.mse)

fsla.m<-summaryBy(fsla+sla~year+plot,fsla,FUN=sum,keep.names=TRUE)
fsla.se<-summaryBy(fsla.se+sla.se~year+plot,fsla,FUN=sd.sum,keep.names=TRUE)
fslam<-merge(fsla.m,fsla.se,by=c('year','plot'))

#############
sla.y<-data.frame(lapply(unique(fsla[,c('plot','strata')]),rep,5))
sla.y$plot<-as.numeric(sla.y$plot)
sla.y$f.y<-rep(0:4,each=10)
sla.y$sla<-as.numeric(NA)
sla.y$fsla<-as.numeric(NA)
sla.y[sla.y$plot==3,c('sla','fsla')]<-fsla[fsla$plot==3&fsla$year==2011,c('sla','fsla')]

for (i in 0:4){
	df<-fsla[fsla$year==2011,]
	sla.y[sla.y$f.y==i&sla.y$plot==2,'sla']<-df[df$plot==3,'sla']+(df[df$plot==2,'sla']-df[df$plot==3,'sla'])/4*i
	sla.y[sla.y$f.y==i&sla.y$plot==2,'fsla']<-df[df$plot==3,'fsla']+(df[df$plot==2,'fsla']-df[df$plot==3,'fsla'])/4*i
	}

sla.y$year<-sla.y$f.y+2005
sla.y$f.y<-NULL
s.yr<-data.frame(lapply(sla.y[sla.y$year==2009,],rep,8))
s.yr$year<-rep(2010:2017,each=10)
sla.yr<-rbind(rbind(sla.y,s.yr),fsla[fsla$year==2018,colnames(s.yr)])
sla.yrm<-summaryBy(sla+fsla~plot+year,sla.yr,FUN=sum,keep.names=TRUE)

fslam$fsla.ser<-fslam$fsla.se/fslam$fsla
fslam$sla.ser<-fslam$sla.se/fslam$sla

for (i in 2:3){
	sla.yrm$sla.se[sla.yrm$plot==i&sla.yrm$year==2018]<-sla.yrm$sla[sla.yrm$plot==i&sla.yrm$year==2018]*fslam$sla.ser[fslam$plot==i&fslam$year==2018]
	sla.yrm$fsla.se[sla.yrm$plot==i&sla.yrm$year==2018]<-sla.yrm$fsla[sla.yrm$plot==i&sla.yrm$year==2018]*fslam$fsla.ser[fslam$plot==i&fslam$year==2018]
	sla.yrm$sla.se[sla.yrm$plot==i&sla.yrm$year!=2018]<-sla.yrm$sla[sla.yrm$plot==i&sla.yrm$year!=2018]*fslam$sla.ser[fslam$plot==i&fslam$year!=2018]
	sla.yrm$fsla.se[sla.yrm$plot==i&sla.yrm$year!=2018]<-sla.yrm$fsla[sla.yrm$plot==i&sla.yrm$year!=2018]*fslam$fsla.ser[fslam$plot==i&fslam$year!=2018]
}
##### sla.yrm : live SLA and literfall SLA

	#Dead branches
sample.crw0<-summaryBy(db.dw.g~Snr,Sbr[Sbr$strata==0,],FUN=sum,na.rm=TRUE)
colnames(sample.crw0)<-c('Snr','Ddb')

#### Fresh weight of sample branches
fw.sbr<-read.table("../data/FW_branch_Ros_2018.txt",head=TRUE,sep='\t')
fw.db<-fw.sbr[fw.sbr$strata==0,c('Snr','strata','db.sample','db.remaining')]

db<-merge(fw.db,sample.crw0,by=c('Snr'))
db$f.db<-db$Ddb/db$db.sample
db$Bdb<-db$Ddb/db$db.sample*(db$db.sample+db$db.remaining)
Bdb<-db[,c('Snr','strata','f.db','Bdb')]

fw.br<-fw.sbr[fw.sbr$strata!=0,c('Snr','strata','br.sample1','br.sample2','br.remaining','db.remaining')]

crw<-merge(fw.br,smp.crw,by=c('Snr','strata'))
crw[is.na(crw)]<-0
crw<-crw[order(crw$Snr,crw$strata),]

crw$f.br1<-crw$Dbr1/crw$br.sample1
crw$f.br2<-crw$Dbr2/crw$br.sample2

crw$f.f01<-crw$Df01/crw$br.sample1
crw$f.f11<-crw$Df11/crw$br.sample1
crw$f.f21<-crw$Df21/crw$br.sample1
crw$f.f31<-crw$Df31/crw$br.sample1
crw$f.f1<-crw$Df1/crw$br.sample1

crw$f.f02<-crw$Df02/crw$br.sample2
crw$f.f12<-crw$Df12/crw$br.sample2
crw$f.f22<-crw$Df22/crw$br.sample2
crw$f.f32<-crw$Df32/crw$br.sample2
crw$f.f2<-crw$Df2/crw$br.sample2

crw$f.cn1<-crw$Dcn1/crw$br.sample1
crw$f.cn2<-crw$Dcn2/crw$br.sample2

crw$f.db1<-crw$Ddb1/crw$br.sample1
crw$f.db2<-crw$Ddb2/crw$br.sample2

crw$dw.br.sample1<-crw$Dbr1+crw$Df1+crw$Dcn1+crw$Ddb1
crw$dw.br.sample2<-crw$Dbr2+crw$Df2+crw$Dcn2+crw$Ddb2
crw$f.smp.br1<-crw$dw.br.sample1/crw$br.sample1
crw$f.smp.br2<-crw$dw.br.sample2/crw$br.sample2


crw$ff.age<-crw$f.f01+crw$f.f11

## correcting fresh mass

crw[crw$Snr==3,2:3][2,]<-c(3,1130)
crw[crw$Snr==3,2:3][3,]<-c(2,615)
crw[crw$Snr==3,]

# plot(crw$dw.br.sample1~crw$br.sample1)
# points(crw$dw.br.sample2~crw$br.sample2)
# points(dw.br.sample1~br.sample1,crw[crw$f.smp.br1<0.4|crw$f.smp.br1>0.7,],col=2)
# points(dw.br.sample2~br.sample2,crw[crw$f.smp.br2<0.4|crw$f.smp.br2>0.7,],col=2)

## Biomass of each component
crw$Bbr<-(crw$Dbr1+crw$Dbr2)/(crw$br.sample1+crw$br.sample2)*(crw$br.sample1+crw$br.sample2+crw$br.remaining)
crw$Bf<-(crw$Df1+crw$Df2)/(crw$br.sample1+crw$br.sample2)*(crw$br.sample1+crw$br.sample2+crw$br.remaining)
crw$Bcn<-(crw$Dcn1+crw$Dcn2)/(crw$br.sample1+crw$br.sample2)*(crw$br.sample1+crw$br.sample2+crw$br.remaining)
crw$Bdb<-crw$db.remaining*db$f.db

tree<-read.table("../data/Tree.info_Ros_2018.txt",head=TRUE,sep='\t')
tree$d<-(tree$d1+tree$d2)/2
tree$d1<-NULL
tree$d2<-NULL

crn<-merge(crw,tree,by=c('Snr'))
crwnB<-summaryBy(Bbr+Bf+Bcn+Bdb~trt+Snr+d+h+crown.base,crn,FUN=sum,keep.names=TRUE)
Brmt18<-crn[crn$strata==1,c('Snr','trt','d','h','crown.base','Bbr')]
Brmt18$Br.trnv<-Brmt18$Bbr/((Brmt18$h-Brmt18$crown.base)/5)	# Br turnover per 1 m


####	Stem volume (m3)
dlg<-read.table("../data/Diameters_stem_Ros_2018.txt",head=TRUE,sep='\t')
dlg$dia<-(dlg$D1+dlg$D2)/2
dlg$D1<-NULL
dlg$D2<-NULL

lg<-cast(dlg,Snr+height~variable,value='dia')
colnames(lg)<-c('Snr','height','bark','dia')
v<-merge(lg,tree[,c('Snr','h','stump')],by='Snr')
v<-v[!is.na(v$dia),]
v$ba<-BA(v$dia/1000)
v$ba.sw<-BA((v$dia-v$bark)/1000)
v$top<-ifelse((v$h-v$height)<=2,'y','n')

for (i in 1:length(v$h)){
v$v[i]<-(v$ba[i]+ifelse(v$top[i]=='n',v$ba[i+1],0))/2*(ifelse(v$top[i]=='n',v$height[i+1],v$h[i])-v$height[i])
v$v.sw[i]<-(v$ba.sw[i]+ifelse(v$top[i]=='n',v$ba.sw[i+1],0))/2*(ifelse(v$top[i]=='n',v$height[i+1],v$h[i])-v$height[i])
}

vol<-summaryBy(v+v.sw~Snr,v,FUN=sum)
colnames(vol)<-c('Snr','Vst','Vsw')

## Stem disks
logfl<-read.table('../data/FW_log_Ros_2018.txt',head=TRUE,sep='\t')
dkf<-read.table('../data/FW_disk_Ros_2018.txt',head=TRUE,sep='\t')
dkd<-read.table('../data/DW_disk_Ros_2018.txt',head=TRUE,sep='\t')
dkd$v<-BA(dkd$D1+dkd$D2/2)*(dkd$depth1.1+dkd$depth1.2+dkd$depth2.1+dkd$depth2.2)
dkd$dw<-(dkd$Bsw+dkd$Bsb)/dkd$v

lgf<-logfl[logfl$variable=='FW',]
lgf$FW.log<-rowSums(lgf[,-c(1:3)],na.rm=TRUE)	# without stump

dd1<-dkd[,c('Snr','disk.position','Bsw','Bsb','v')]
df1<-melt(dkf,id='Snr')
colnames(df1)<-c('Snr','disk.position','FW')
dfd<-merge(dd1,df1,by=c('Snr','disk.position'))
dfd$r.sw<-dfd$Bsw/dfd$FW
dfd$r.sb<-dfd$Bsb/dfd$FW

rwb<-summaryBy(Bsw+Bsb+FW~Snr,dfd,FUN=sum,na.rm=TRUE,keep.names=TRUE)
rwb$rsw<-rwb$Bsw/rwb$FW
rwb$rsb<-rwb$Bsb/rwb$FW
bst<-merge(lgf[,c('Snr','FW.log')],rwb[,c('Snr','FW','rsw','rsb')],by='Snr')
bst$smp.ratio<-bst$FW/(bst$FW.log*1000)
bst$Fst<-bst$FW.log*1000+bst$FW
bst$Bst<-bst$Fst*(bst$rsb+bst$rsw)
bst$Bsw<-bst$Fst*bst$rsw

st181<-merge(crwnB,vol,by='Snr')
st182<-merge(st181,bst[,c('Snr','FW','Bst','Bsw')],by='Snr')
st18<-merge(st182,Brmt18[,c('Snr','Br.trnv')],by='Snr')
st18$lc<-st18$h-st18$crown.base
st18$year<-2018
st18$Bcr<-NA
st18$bwd<-(st18$Bsw/1000)/st18$Vsw

st11<-read.table("../data/Ro_allometrics_2006_11.txt",head=TRUE,sep='\t')
st11$year[st11$year==2006]<-2005
st<-rbind(st11,st18[,colnames(st11)])
st<-st[st$trt!='LF',]
st$d<-st$d/1000
st$ba<-BA(st$d)
st$d2h<-st$d^2*st$h
st$dh<-st$d*st$h
st$dlc<-st$d*st$lc
st[,c('Bst','Bsw','Bbr','Bf','Bcn','Bdb','Bcr','Br.trnv')]<-st[,c('Bst','Bsw','Bbr','Bf','Bcn','Bdb','Bcr','Br.trnv')]/1000
st$Bsb<-st$Bst-st$Bsw
st$Vsb<-st$Vst-st$Vsw
st$bwd<-st$Bsw/st$Vsw
st$plot<-ifelse(st$trt=='F',2,ifelse(st$trt=='C',3,1))
# write.table(st,"Ros_processed_harvested_trees_2006_2018.txt",sep="\t",quote=FALSE,row.names=FALSE)
st$Bst<-st$Bsw+st$Bsb
summary(lm(log(Bst)~log(d2h),st))

## Wood density disks--	ring width: only one-side
wg1<-read.table('../data/Ring_density_Ros_2018.txt',head=TRUE,sep='\t')
wg1$Snr<-as.numeric(substr(wg1$Sample,1,ifelse(nchar(as.character(wg1$Sample))==2,1,2)))
wg1$dr<-substr(wg1$Sample,nchar(as.character(wg1$Sample)),nchar(as.character(wg1$Sample)))
wg1$year<-2019-wg1$Ring.fr.bark

# a1<-summaryBy(Ring.width~Sample,wg,FUN=sum,na.rm=TRUE)
# a2<-summaryBy(Distance.fr.pith~Sample,wg,FUN=max,na.rm=TRUE)
# a<-merge(a1,a2,by='Sample')
# plot(a[,2]~a[,3])
# abline(a=0,b=1)
wg1<-wg1[,c('Snr','dr','year','Ring.fr.pith','Ring.width','EW.width','LW.width','Density','Min.density','Max.density','EW.density','LW.density')]
colnames(wg1)<-c('Snr','dr','year','age','wid','wid.e','wid.l','wg','wg.mn','wg.mx','wg.e','wg.l')
wg<-summaryBy(wid+wid.e+wid.l+wg+wg.mn+wg.mx+wg.e+wg.l~Snr+year+age,wg1,FUN=mean,keep.names=TRUE)
wg$trt<-ifelse(wg$Snr<7,'F','C')
wg$bg<-ifelse(wg$trt=='F',2,4)

wg$cul.wid[wg$age==1]<-wg$wid[wg$age==1]
wg$bai<-BA(wg$cul.wid*2)
for (i in 1:12){
	ss<-n(wg$year[wg$Snr==i])
	for (k in 1:c(ss-1)){
		wg$cul.wid[wg$Snr==i][k+1]<-wg$cul.wid[wg$Snr==i][k]+wg$wid[wg$Snr==i][k+1]
		wg$bai[wg$Snr==i][k+1]<-BA(wg$cul.wid[wg$Snr==i][k+1]*2)-BA(wg$cul.wid[wg$Snr==i][k]*2)
		wg$bai.l[wg$Snr==i][k+1]<-BA(wg$cul.wid[wg$Snr==i][k+1]*2)-BA((wg$cul.wid[wg$Snr==i][k]+wg$wid.e[wg$Snr==i][k+1])*2)
		wg$bai.e[wg$Snr==i][k+1]<-BA((wg$cul.wid[wg$Snr==i][k]+wg$wid.e[wg$Snr==i][k+1])*2)-BA(wg$cul.wid[wg$Snr==i][k]*2)
		}}
wg$ba<-BA(wg$cul.wid*2)
wg$rba<-wg$bai/wg$ba
# plot(NA,xlim=c(0,7),ylim=c(0,7))
# abline(a=0,b=1)
# for (i in 1:12) {
 # points(cul.wid~wid,wg[wg$Snr==i&wg$year==min(wg$year[wg$Snr==i],na.rm=TRUE),])}
# a1<-summaryBy(bai~Snr,wg,FUN=sum)
# a2<-summaryBy(cul.wid~Snr,wg,FUN=function(x){BA(max(x)*2)})
# a<-merge(a1,a2,by=c('Snr','dr'))
# lm(a[,3]~a[,4])

#### for 2018
for (i in 1:12) {
	ss<-n(wg$year[wg$Snr==i])
	for (k in 1:ss){
wg$rbai.t[wg$Snr==i][k]<-wg$bai[wg$Snr==i][k]/BA(wg$cul.wid[wg$Snr==i][ss]*2)}}
wg$rwg<-wg$wg*wg$rbai.t*1000
bwg18<-summaryBy(rwg~Snr+dr+trt,wg,FUN=sum)
bwg18$year<-2018
bwg<-merge(bwg18,st[st$year==2018,c('Snr','bwd')],by='Snr')
wg.coe<-summary(lm(bwd~rwg.sum,bwg))$coe[,1]		## correcting wd using basic wood density
wg$c.wg<-wg$wg*1000*wg.coe[2]+wg.coe[1]
wg$c.wg.e<-wg$wg.e*1000*wg.coe[2]+wg.coe[1]
wg$c.wg.l<-wg$wg.l*1000*wg.coe[2]+wg.coe[1]



# wd1<-read.table("Ring_width_Ros_2018.txt",head=TRUE,sep='\t')
# wd2<-melt(wd1,id=c('Snr','route','diameter'))
# wd2$year<-substr(wd2$variable,2,5)
# wd2$wd<-wd2$value
# wd2$variable<-NULL
# wd2$value<-NULL
# wd<-merge(wd2[wd2$route==1,c('Snr','diameter','wd','year')],wd2[wd2$route==2,c('Snr','diameter','wd','year')],by=c('Snr','diameter','year'))
# wd$wd<-(wd$wd.x+wd$wd.y)/1000
# wd$d<-wd$diameter/1000	### diameter in m
# wd$d[wd$year!=2018]<-NA
# a<-merge(wd,wg,by=c('Snr','year'))
# a$wid<-a$wid*2/1000
# plot(wid~wd,a)

swg<-wg[,c('Snr','trt','year','age','wid','wid.e','wid.l','c.wg','c.wg.e','c.wg.l','ba','bai','bai.e','bai.l')]
colnames(swg)<-c('Snr','trt','year','age','wd','wde','wdl','swg','wge','wgl','ba','bai','baie','bail')
# write.table(swg,"Ros_processed_width_denstiy.txt",sep="\t",quote=FALSE,row.names=FALSE)
swm<-summaryBy(swg~trt+year,swg,FUN=mean,keep.names=TRUE)
swg$plot<-ifelse(swg$trt=='F',2,3)
swg$p.baie<-swg$baie/swg$bai
swg$p.wge<-swg$wge/swg$swg
swg$p.bail<-swg$bail/swg$bai
wdf<-swg[swg$year>2004,]
wdf$y18<-ifelse(wdf$year==2018,'y','n')

### Early-, latewood, wood density analysis

# quartz()
# par(mfrow=c(3,5))
# for (i in 2004:2018){
# plot(swg~p.baie,wdf[wdf$year==i,],col=plot,ylim=c(320,480),xlim=c(.3,1))
# }
# quartz()
# par(mfrow=c(3,5))
# for (i in 2004:2018){
# plot(baie~bai,wdf[wdf$year==i,],col=plot,ylim=c(0,1400),xlim=c(100,1900))
# points(bail~bai,wdf[wdf$year==i,],col=plot,pch=2)
# }
# quartz()
# par(mfrow=c(3,5))
# for (i in 2004:2018){
# plot(wge~baie,wdf[wdf$year==i,],col=plot,ylim=c(280,390),xlim=c(40,1400))
# }
# quartz()
# par(mfrow=c(3,5))
# for (i in 2004:2018){
# plot(wgl~bail,wdf[wdf$year==i,],col=plot,ylim=c(420,580),xlim=c(40,550))
# }

# quartz()
# par(mfrow=c(2,3))
# plot(baie~bai,wdf,col=plot,xlab='Basal area increment (mm2 yr-1)',ylab='Earlywood increment (mm2 yr-1)')
# plot(wge~baie,col=plot,wdf,xlab='Earlywood increment (mm2 yr-1)',ylab='Earlywood density (kg m-3 )')
# plot(swg~p.baie,wdf,col=plot,xlab='Propotion of earlywood growth',ylab='Annual wood density (kg m-3 )')
# 
# plot(bail~bai,wdf,col=plot,xlab='Basal area increment (mm2 yr-1)',ylab='Latewood increment (mm2 yr-1)')
# plot(wgl~bail,col=plot,wdf,xlab='Latewood increment (mm2 yr-1)',ylab='Latewood density (kg m-3 )')
# plot(swg~p.bail,wdf,col=plot,xlab='Propotion of latewood growth',ylab='Annual wood density (kg m-3 )')


#1 Early wood increment ~ BAI
baie.bai<-lm(baie~bai*as.factor(plot)+as.factor(year)+0,wdf)
summary(baie.bai)
wdf$pre.baie<-predict(baie.bai,wdf)
wdf$res.baie<-wdf$pre.baie-wdf$baie
wdf$rr.baie<-wdf$res.baie/wdf$pre.baie

baie.bai.log<-lm(log(baie)~log(bai)*as.factor(plot)+as.factor(year)+0,wdf)
summary(baie.bai.log)
wdf$pre.baie.log<-exp(predict(baie.bai.log,wdf))
wdf$res.baie.log<-wdf$pre.baie.log-wdf$baie
wdf$rr.baie.log<-wdf$res.baie.log/wdf$pre.baie.log

# quartz()
# par(mfrow=c(2,3))
# plot(baie~pre.baie,wdf,col=plot,xlim=c(0,1400))
# abline(a=0,b=1)
# plot(res.baie~pre.baie,wdf,col=plot,ylim=c(-150,150),xlim=c(0,1400))
# plot(rr.baie~pre.baie,wdf,col=plot,ylim=c(-0.34,.34),xlim=c(0,1400))
# 
# plot(baie~pre.baie.log,wdf,col=plot,xlim=c(0,1400))
# abline(a=0,b=1)
# plot(res.baie.log~pre.baie.log,wdf,col=plot,ylim=c(-150,150),xlim=c(0,1400))
# plot(rr.baie.log~pre.baie.log,wdf,col=plot,ylim=c(-0.34,.34),xlim=c(0,1400))
ls()
#2 Earlywood density ~ earlywood growth
wge.baie<-lm(wge~baie+y18,wdf)
summary(wge.baie)
wdf$pre.wge<-predict(wge.baie,wdf)
wdf$res.wge<-wdf$pre.wge-wdf$wge
wdf$rr.wge<-wdf$res.wge/wdf$pre.wge

wge.baie.log<-lm(log(wge)~log(baie)*as.factor(plot)+(y18),wdf)
summary(wge.baie.log)
wdf$pre.wge.log<-exp(predict(wge.baie.log,wdf))
wdf$res.wge.log<-wdf$pre.wge.log-wdf$wge
wdf$rr.wge.log<-wdf$res.wge.log/wdf$pre.wge.log

# quartz()
# par(mfrow=c(2,3))
# plot(wge~pre.wge,wdf,col=plot,xlim=c(280,380))
# abline(a=0,b=1)
# plot(res.wge~pre.wge,wdf,col=plot,ylim=c(-32,32),xlim=c(280,380))
# plot(rr.wge~pre.wge,wdf,col=plot,ylim=c(-0.1,0.1),xlim=c(280,380))
# 
# plot(wge~pre.wge.log,wdf,col=plot,xlim=c(280,380))
# abline(a=0,b=1)
# plot(res.wge.log~pre.wge.log,wdf,col=plot,ylim=c(-32,32),xlim=c(280,380))
# plot(rr.wge.log~pre.wge.log,wdf,col=plot,ylim=c(-0.1,0.1),xlim=c(280,380))

#3 Annual wood density ~ Earlywood proportion
swg.p.baie<-lm(swg~p.baie+as.factor(plot)*as.factor(year),wdf)
summary(swg.p.baie)
wdf$pre.swg<-predict(swg.p.baie,wdf)
wdf$res.swg<-wdf$pre.swg-wdf$swg
wdf$rr.swg<-wdf$res.swg/wdf$pre.swg
head(wdf)
wdfm<-summaryBy(swg~trt+year,wdf,FUN=me)
plot(swg.m~year,wdfm,col=0,ylim=c(330,420))
for (i in c(1:13,15:27)){
lines(rep(wdfm$year[i],2),wdfm$swg.m[i]+c(-.5,.5)*wdfm$swg.se[i])}
points(swg.m~year,wdfm[wdfm$trt=='C'&wdfm$year<2018,],type='o',pch=21,bg='white')
points(swg.m~year,wdfm[wdfm$trt=='F'&wdfm$year<2018,],type='o',bg=1,pch=21)


# # quartz()
# par(mfrow=c(2,3))
# plot(swg~p.baie,wdf,col=plot)
# plot(res.swg~pre.swg,wdf,col=plot)
# plot(rr.swg~pre.swg,wdf,col=plot)

##### Allometrics
st$pch<-ifelse(st$trt=='LF',1,ifelse(st$trt=='F',2,3))

# plot(Bf~d,st[st$year==2005,],col=1,pch=pch,ylim=c(0,15),ylab='Folage biomass (kg per tree)',xlab='Diameter at 1.3 m (m)')
# points(Bf~d,st[st$year==2011,],col=2,pch=pch)
# points(Bf~d,st[st$year==2018,],col=3,pch=pch)
# legend(.12,15,c('Ref 2005','Low Fer 2005','Fer 2005','Ref 2011','Fer 2011','Ref 2018','Fer 2018'),pch=c(1,2,3,1,2,1,2),col=c(1,1,1,2,2,3,3),pt.bg=c(0,1,0,2),cex=1,bty='n')

# ## Foliage biomass modelling Separating between treatments was the best

# Bf.prd<-lm(log(Bf)~log(d)+log(lc)+trt+as.integer(year),st)
# summary(Bf.prd)
# st$Bf.prd<-exp(predict(Bf.prd,st))
# st$res<-(st$Bf-st$Bf.prd)/st$Bf.prd
# sqrt(sum((st$Bf-st$Bf.prd)^2))
# par(mfrow=c(1,3))
# plot(res~Bf.prd,st,ylim=c(-.4,.4),xlim=c(1,14))
# points(res~Bf.prd,st[st$trt=='C',],bg=1,pch=21)
# points(res~Bf.prd,st[st$trt=='F',],bg=2,pch=21)
# abline(a=0,b=0,lty=2,col=8)
# abline(lm(res~Bf.prd,st[st$trt=='F',]),lty=1,col=2)
# abline(lm(res~Bf.prd,st[st$trt=='C',]),lty=1,col=1)
# 
# 
# Bf.prdF<-lm(log(Bf)~log(d)+log(lc)+as.integer(year),st[st$trt=='F',])
# summary(Bf.prdF)
# st$Bf.prd[st$trt=='F']<-exp(predict(Bf.prdF,st[st$trt=='F',]))
# Bf.prdC<-lm(log(Bf)~log(d)+log(lc)+as.integer(year),st[st$trt=='C',])
# summary(Bf.prdC)
# st$Bf.prd[st$trt=='C']<-exp(predict(Bf.prdC,st[st$trt=='C',]))
# st$res<-(st$Bf-st$Bf.prd)/st$Bf.prd
# sqrt(sum((st$Bf-st$Bf.prd)^2))
# 
# plot(res~Bf.prd,st,ylim=c(-.4,.4),xlim=c(1,14))
# points(res~Bf.prd,st[st$trt=='C',],bg=1,pch=21)
# points(res~Bf.prd,st[st$trt=='F',],bg=2,pch=21)
# abline(a=0,b=0,lty=2,col=8)
# abline(lm(res~Bf.prd,st[st$trt=='F',]),lty=1,col=2)
# abline(lm(res~Bf.prd,st[st$trt=='C',]),lty=1,col=1)
# 
# 
# Bf.prd0511<-lm(log(Bf)~log(d)+log(lc)+trt*as.integer(year),st[st$year!=2018,])
# Bf.prd1218<-lm(log(Bf)~log(d)*log(lc)+trt+as.integer(year),st[st$year!=2005,])
# summary(Bf.prd0511)
# summary(Bf.prd1218)
# 
# st$Bf.prd[st$year!=2018]<-exp(predict(Bf.prd0511,st[st$year!=2018,]))
# st$Bf.prd[st$year==2018]<-exp(predict(Bf.prd1218,st[st$year==2018,]))
# st$res.all<-(st$Bf-st$Bf.prd.all)/st$Bf.prd.all
# st$res<-(st$Bf-st$Bf.prd)/st$Bf.prd
# sqrt(sum((st$Bf-st$Bf.prd)^2))
# 
# plot(res~Bf.prd,st,ylim=c(-.4,.4),xlim=c(1,14))
# points(res~Bf.prd,st[st$trt=='C',],bg=1,pch=21)
# points(res~Bf.prd,st[st$trt=='F',],bg=2,pch=21)
# abline(a=0,b=0,lty=2,col=8)
# abline(lm(res~Bf.prd,st[st$trt=='F',]),lty=1,col=2)
# abline(lm(res~Bf.prd,st[st$trt=='C',]),lty=1,col=1)
# 

Bf.prdF<-lm(log(Bf)~log(d)+log(lc)+as.integer(year),st[st$trt=='F',])
Bf.prdC<-lm(log(Bf)~log(d)+log(lc)+as.integer(year),st[st$trt=='C',])

summary(lm(log(Bbr)~log(d)*log(lc)+year,st))
Bbr.prd<-lm(log(Bbr)~log(d)*log(lc),st)
st$Bbr.prd<-exp(predict(Bbr.prd,st))

summary(lm(log(Vst)~log(d)+log(h)+factor(year),st))
Vst.prd<-lm(log(Vst)~log(d)+log(h),st)
st$Vst.prd<-exp(predict(Vst.prd,st))

summary(lm(log(Vsw)~log(d)+log(h),st))
Vsw.prd<-lm(log(Vsw)~log(d)+log(h),st)
st$Vsw.prd<-exp(predict(Vsw.prd,st))

summary(lm(log(Bsb)~log(d)+log(h),st))
Bsb.prd<-lm(log(Bsb)~log(d)+log(h),st)
st$Bsb.prd<-exp(predict(Bsb.prd,st))

summary(lm(log(Bcr)~log(d),st))
Bcr.prd<-lm(log(Bcr)~log(d),st)
st$Bcr.prd<-exp(predict(Bcr.prd,st))

summary(lm(log(Bdb)~log(d)+lc+as.factor(plot),st))
Bdb.prd<-lm(log(Bdb)~log(d)+lc+as.factor(plot),st)
st$Bdb.prd<-exp(predict(Bdb.prd,st))

summary(lm(log(Bcn)~log(d)+as.factor(plot),st[st$Bcn>0&!is.na(st$Bcn),]))
Bcn.prd<-lm(log(Bcn)~log(d)+as.factor(plot),st[st$Bcn>0&!is.na(st$Bcn),])
st$Bcn.prd[st$Bcn>0&!is.na(st$Bcn)]<-exp(predict(Bcn.prd,st[st$Bcn>0&!is.na(st$Bcn),]))

summary(lm(log(Br.trnv)~log(d)+log(h),st))	# branch turnover per 1 m
Br.trnv.prd<-lm(log(Br.trnv)~log(d)+log(h),st)
st$Br.trnv.prd<-exp(predict(Br.trnv.prd,st))

brd<-merge(Cr,st[st$year==2018,c('Snr','plot','d','h','lc')])
summary(lm(log(Dbr)~log(d.brd)+as.factor(strata),brd))
summary(lm(log(Dbr)~log(d.brd),brd[brd$strata<4,]))
sample.br.prd<-lm(log(Dbr)~log(d.brd),brd[brd$strata<4,])


st$Vst.res<-(st$Vst-st$Vst.prd)/st$Vst.prd
st$Vsw.res<-(st$Vsw-st$Vsw.prd)/st$Vsw.prd
st$Bsb.res<-(st$Bsb-st$Bsb.prd)/st$Bsb.prd
st$Bbr.res<-(st$Bbr-st$Bbr.prd)/st$Bbr.prd
st$Bcn.res<-(st$Bcn-st$Bcn.prd)/st$Bcn.prd
st$Bbd.res<-(st$Bdb-st$Bdb.prd)/st$Bdb.prd

# Vst.prd
# Vsw.prd
# Bsb.prd
# Bbr.prd
# Bf.prd	# Year as covariate
# Bcn.prd
# Bdb.prd
# Br.trnv.prd

sa<-read.table("../data/sapwood.txt",head=TRUE)
sa$plot<-ifelse(sa$trt=='C',3,2)
# summary(lm(sapwood_area~ba_under_bark*trt,sa[sa$year==2018,]))
# summary(lm(sapwood_area~ba_under_bark*trt,sa[sa$year==2011,]))
# plot(sapwood_area~ba_under_bark,sa,col=0)
# points(sapwood_area~ba_under_bark,sa[sa$year==2018&sa$trt=='F',],bg=1,pch=24)
# points(sapwood_area~ba_under_bark,sa[sa$year==2018&sa$trt=='C',],bg='white',pch=24)
# points(sapwood_area~ba_under_bark,sa[sa$year==2011&sa$trt=='F',],bg=1,pch=21)
# points(sapwood_area~ba_under_bark,sa[sa$year==2011&sa$trt=='C',],bg='white',pch=21)

leafsap<-merge(st[st$year%in%c(2011,2018),c('year','d','ba','h','lc','Snr','Bf','plot')],sa,by=c('year','Snr','plot'))


lsa<-merge(leafsap,sla.yrm[,c('year','plot','sla','sla.se')],by=c('year','plot'))
lsa$plot<-as.integer(ifelse(lsa$trt=='F',2,3))
lsa$leaf.area<-lsa$sla*lsa$Bf/10	#m2
d.sap<-lm(log(sapwood_area)~log(d),lsa)
lsa$Hv<-lsa$sapwood_area/lsa$leaf.area
summaryBy(Hv~trt+year,lsa,FUN=me)
# summary(d.sap)
# par(mfrow=c(2,2))
# plot(d.sap)
# plot(log(sapwood_area)~log(ba),lsa,col=0)
# points(log(sapwood_area)~log(ba),lsa[lsa$year==2011&lsa$trt=='F',],bg=1,pch=24)
# points(log(sapwood_area)~log(ba),lsa[lsa$year==2011&lsa$trt=='C',],bg='white',pch=24)
# points(log(sapwood_area)~log(ba),lsa[lsa$year==2018&lsa$trt=='F',],bg=1,pch=21)
# points(log(sapwood_area)~log(ba),lsa[lsa$year==2018&lsa$trt=='C',],bg='white',pch=21)

# 
# plot(sapwood_area~leaf.area,lsa,col=0)
# points(sapwood_area~ leaf.area,lsa[lsa$year==2011&lsa$trt=='F',],bg=1,pch=24)
# points(sapwood_area~ leaf.area,lsa[lsa$year==2011&lsa$trt=='C',],bg='white',pch=24)
# points(sapwood_area~ leaf.area,lsa[lsa$year==2018&lsa$trt=='F',],bg=1,pch=21)
# points(sapwood_area~ leaf.area,lsa[lsa$year==2018&lsa$trt=='C',],bg='white',pch=21)


