source('litterfall.R')
ind<-read.table("../data/inv.tree.dimensions.txt",head=TRUE,sep='\t')
ind<-ind[order(ind$year,ind$plot,ind$rep,ind$nr),]
ind$trt<-as.factor(ifelse(ind$plot==2,'F','C'))
ind$cb<-ind$h-ind$lc
ind$rlc<-(ind$bc-ind$bc.1)/ind$lc.1
ind$ba<-BA(ind$d)
ind$Vst<-exp(predict(Vst.prd,ind))
ind$Vsw<-exp(predict(Vsw.prd,ind))
ind$Bsb<-exp(predict(Bsb.prd,ind))
ind$Bbr<-exp(predict(Bbr.prd,ind))
ind$Bcr<-exp(predict(Bcr.prd,ind))
ind$Bcn<-exp(predict(Bcn.prd,ind))
ind$Bdb<-exp(predict(Bdb.prd,ind))
ind$Bf[ind$trt=='F']<-exp(predict(Bf.prdF,ind[ind$trt=='F',]))
ind$Bf[ind$trt=='C']<-exp(predict(Bf.prdC,ind[ind$trt=='C',]))
ind$sa<-exp(predict(d.sap,ind))


vari<-c('d','h','cb','ba','Vst','Vsw','Bsb','Bbr','Bf','Bcr','Bcn')
spatem<-c('plot','rep','year')
for (i in 2006:2018){
	for (j in vari){
	ind[ind$year==i,paste('d',j,sep='')]<-ind[ind$year==i,j]-ind[ind$year==(i-1),j]
	}}

#1 Early wood increment ~ BAI
ind$y18<-ifelse(ind$year==2018,'y','n')
ind$bai<-ind$dba*1000000
ind$baie<-exp(predict(baie.bai.log,ind))
ind$dbae<-ind$baie/1000000
ind$p.baie<-ind$dbae/ind$dba
ind$dbal<-ind$dba*(1-ind$p.baie)
ind$de<-revD(ind$ba-ind$dbal)
ind$hbar<-ind$dh/ind$dba
ind$hbare<-ind$dh/ind$dbae
#2 Earlywood density ~ earlywood growth
ind$wge<-exp(predict(wge.baie.log,ind))

#3 Annual wood density ~ Earlywood proportion
ind$swg<-predict(swg.p.baie,ind)
ind$Vswe<-exp(predict(Vsw.prd,data.frame(d=ind$de,h=ind$h)))

#4 Wood mass and volume growth
ind$dVswe<-ind$Vswe-(ind$Vsw-ind$dVsw)
ind$dVswl<-ind$dVsw-ind$dVswe
ind$dBsw<-ind$dVsw*ind$swg
ind$dBswe<-ind$dVswe*ind$wge
ind$dBswl<-ind$dBsw-ind$dBswe

# Branch turnover
Bbr.pdf<-function(rlc,plot){
	a<-ifelse(plot==2,8.251,8.0417)
	b<-ifelse(plot==2,-15.873,-16.2086)
	c<-ifelse(plot==2,7.276,7.7132)
	a*(rlc-1)+b*(rlc^2-1)+c*(rlc^3-1)}
inbr<-ind[!is.na(ind$rlc),]
inbr$brt<-mapply(function(x,y){integrate(Bbr.pdf,plot=y,0,x)$value},inbr$rlc,inbr$plot)*(inbr$Bbr-inbr$dBbr)

inb<-merge(ind,inbr[,c('plot','rep','nr','year','d','brt')],by=c('plot','rep','nr','year','d'),all.x=TRUE)


#################
####	Stand scale
#################
std1<-summaryBy(ba+sa+Vst+Vsw+Bsb+Bbr+Bcr+Bcn+Bdb+Bf+dba+dh+hbar+hbare+dVst+dVsw+dVswe+dVswl+dBsw+dBswe+dBswl+dBsb+dBbr+dBf+dBcr+dBcn+brt~plot+rep+year,inb,FUN=sum,na.rm=TRUE,keep.names=TRUE)
std1[,c('Vst','Vsw','dVst','dVsw','dVswe','dVswl','ba','sa')]<-std1[,c('Vst','Vsw','dVst','dVsw','dVswe','dVswl','ba','sa')]*10
plot(ba~sa,std1,col=year)
abline(a=0,b=1)

#	BA in m2 ha-1; volume in m3 ha-1; biomass in g DW m-2
grw.var<-colnames(std1)[-c(1:13)]
std2<-summaryBy(d~plot+rep+year,inb,FUN=qdr.m,keep.names=TRUE)
std3<-summaryBy(h+lc~plot+rep+year,inb,FUN=mean,na.rm=TRUE,keep.names=TRUE)
std4<-summaryBy(d~plot+rep+year,inb,FUN=n)
std4$sd<-std4$d.n*10

## Maximum size-density relationship log(BA) = -1.12*log(std) + 1.98
# lfy literfall in g dw m-2 yr-1
# sla.yr SLA for live foliage and litterfall
# Winter 2013, 43 trees were blown-over
std<-merge(merge(merge(merge(merge(std2,std3,by=spatem),std4[,c(spatem,'sd')],by=spatem),std1,by=spatem),lfy,by=c('plot','year'),all.x=TRUE),sla.yrm,by=c('plot','year'))

std$msd<-10^(((1.98-log(BA(std$d),10))/1.12))
std$rsd<-std$sd/std$msd
std$log.ba<-log(BA(std$d),10)
std$log.sd<-log(std$sd,10)
std[std$dba==0,paste(grw.var)]<-NA
std$lai.a<-(std$Bf*std$sla)/10000		#m2
std$lai.s<-std$lai.a+(std$smlf*std$fsla)/10000		#m2
std$NPPsw<-std$dBsw*.52366
std$NPPswe<-std$dBswe*.52366
std$NPPswl<-std$NPPsw-std$NPPswe
std$dVer<-std$dVswe/std$dVsw
std$NPPer<-std$NPPswe/std$NPPsw
std$NPPsb<-std$dBsb*litter.other.C*0.01
std$NPPbr<-(std$dBbr+std$brt)*.52366
std$NPPf<-std$dBf*.507+std$needle.m*litter.leaf.C*0.01
std$NPPcr<-std$dBcr*.52366
std$NPPmis<-(std$sum.m-std$needle.m)*litter.other.C*0.01
std$ANPP<-std$NPPsw+std$NPPsb+std$NPPbr+std$NPPf+std$NPPmis+std$cone.m*litter.other.C*0.01
std$NPPw<-std$NPPsw+std$NPPbr+std$NPPcr+std$cone.m*litter.other.C*0.01
std$NPP<-std$ANPP+std$NPPcr
std$ar.NPPBr<-std$NPPbr/std$ANPP
std$sr.NPPBr<-std$NPPbr/std$NPPsw
std$swe<-std$dBswe/std$dVswe*10
std$swl<-std$dBswl/std$dVswl*10
std$swd<-std$dBsw/std$dVsw*10
std$swer<-std$swe/std$swd
std$hbv.s<-(std$sa)/std$lai.s	#E-4
std$hbv.a<-(std$sa)/std$lai.a	#E-4
plot(sa~year,std[std$year%in%2011:2018,],col=plot)
hbv<-std[std$year%in%2015:2018,c('year','plot','hbv.s','hbv.a','lai.s','lai.a','sa')]
# write.table(hbv,'/Users/hyli0001/Desktop/huber_values.txt',sep="\t",quote=FALSE,row.names=FALSE)
hbm<-summaryBy(hbv.s+hbv.a+lai.s+lai.a+sa~year+plot,hbv,FUN=me)
hbm$bg<-ifelse(hbm$plot==2,1,'white')

# # par(mfrow=c(2,2))
# plot(NA,xlim=c(2014.5,2018.5),ylim=c(1.5,2.8),xlab='Year',ylab='LAI during summer')
# for (i in 1:8){
	# lines(rep(hbm$year[i],2),hbm$lai.s.m[i]+c(-1,1)*hbm$lai.s.se[i])
# }
# points(lai.s.m~year,hbm,bg=bg,pch=21)
# mean(hbm$lai.s.m[hbm$plot==3])/mean(hbm$lai.s.m[hbm$plot==2])
# legend('bottomleft',c('C','F'),pt.bg=c('white',1),pch=21,col=1,box.col=FALSE)

# plot(NA,xlim=c(2014.5,2018.5),ylim=c(1.5,2.8),xlab='Year',ylab='LAI during autumn')
# for (i in 1:8){
	# lines(rep(hbm$year[i],2),hbm$lai.a.m[i]+c(-1,1)*hbm$lai.a.se[i])
# }
# points(lai.a.m~year,hbm,bg=bg,pch=21)
# mean(hbm$lai.a.m[hbm$plot==3])/mean(hbm$lai.a.m[hbm$plot==2])

# plot(NA,xlim=c(2014.5,2018.5),ylim=c(19,25),xlab='Year',ylab='Sapwood area at 1.3 m')
# for (i in 1:8){
	# lines(rep(hbm$year[i],2),hbm$sa.m[i]+c(-1,1)*hbm$sa.se[i])
# }
# points(sa.m~year,hbm,bg=bg,pch=21)
# mean(hbm$sa.m[hbm$plot==3])/mean(hbm$sa.m[hbm$plot==2])


# plot(NA,xlim=c(2014.5,2018.5),ylim=c(7.5,10.5),xlab='Year',ylab='Sapwood area / LAI (10^-4)')
# for (i in 1:8){
	# lines(rep(hbm$year[i],2),hbm$hbv.s.m[i]+c(-1,1)*hbm$hbv.s.se[i])
# }
# points(hbv.s.m~year,hbm,bg=bg,pch=21)
# mean(hbm$hbv.s.m[hbm$plot==3])/mean(hbm$hbv.s.m[hbm$plot==2])

#######################
wth<-read.table('Svartberget_flux_data.txt',head=TRUE,sep='\t')
colnames(wth)<-c('year','start','end','gs','hsum','hr20','hr0','ppt','rad')
wth.d1<-read.table('ClimData_Svartbergets.txt',head=TRUE,sep='\t')
wth.d<-wth.d1
colnames(wth.d)<-c('date','airT','Tmax','Tmin','soilT','ppt','gr','ovp','hr20','hr0','gs')
wth.d$yr<-as.numeric(substr(wth.d$date,1,4))
wth.d$month<-as.numeric(substr(wth.d$date,6,7))
wth.d$day<-as.numeric(substr(wth.d$date,9,10))
wth.d$doy<-as.numeric(strftime(wth.d$date, format = "%j"))
wth.d$svp<-(6.1078 * exp(17.269 * wth.d$airT / (237.3 + wth.d$airT)))
# plot(svp~ airT,wth.d)
# points(ovp ~ airT,wth.d,col=2)
wth.d$vpd<-(wth.d$svp-wth.d$ovp)/10
wth.d$vpdmn<-vpdtxn(wth.d$Tmax,wth.d$Tmin)/10
# plot(vpd~ airT,wth.d)
# plot(wth.d$vpd[wth.d$yr%in%2018:2018])
wdg<-wth.d[wth.d$gs=='Ja',]
wdg$pptc<-ifelse(!is.na(wdg$ppt)&wdg$ppt>1,1,0)

wdth.mm<-summaryBy(Tmax+Tmin+airT+vpdmn~yr+month, wth.d,FUN=mean,na.rm=TRUE,keep.names=TRUE)
wdth.ms<-summaryBy(ppt+gr~yr+month, wth.d,FUN=sum,na.rm=TRUE,keep.names=TRUE)
wdth.m<-merge(wdth.ms, wdth.mm,by=c('yr','month'))

# write.table(wdth.m,'/Users/hyli0001/Documents/Presentations/2023/Ecophysi/Ros_stand_Weather.txt',sep="\t",quote=FALSE,row.names=FALSE)

# write.table(stdm,'/Users/hyli0001/Documents/Presentations/2023/Ecophysi/Ros_stand_stock.csv',sep="\t",quote=FALSE,row.names=FALSE)

# quartz()
# par(mfrow=c(3,4))
# for(i in 2007:2018){
# plot(ppt~doy,wdg[wdg$yr==i,],type='o',main=i,ylim=c(0,60),cex=0)}
# 
# quartz()
# par(mfrow=c(3,4))
# for(i in 2007:2018){
# plot(vpd~doy,wdg[wdg$yr==i,],type='o',main=i,ylim=c(0,1.5),cex=0)}
# 
# mean(summaryBy(airT.mean~yr,summaryBy(airT~yr+month,wdg,FUN=mean,na.rm=TRUE),FUN=mean)[,2])
# mean(summaryBy(ppt.sum~yr,summaryBy(ppt~yr+month,wdg,FUN=sum,na.rm=TRUE),FUN=sum)[,2])

wdth.m<-summaryBy(airT+vpd+vpdmn~yr,wdg,FUN=mean,na.rm=TRUE,keep.names=TRUE)

wdth.s.6<-summaryBy(ppt+gr~yr,wth.d[wth.d$month==6,],FUN=sum,na.rm=TRUE,keep.names=TRUE)
wdth.m.6<-summaryBy(airT+vpd+vpdmn~yr,wth.d[wth.d$month==6,],FUN=mean,na.rm=TRUE,keep.names=TRUE)
wdth.6<-merge(wdth.s.6,wdth.m.6,by='yr')
colnames(wdth.6)<-mcolname(wdth.6,1,0,'.6')

wdth.s.7<-summaryBy(ppt+gr~yr,wth.d[wth.d$month==7,],FUN=sum,na.rm=TRUE,keep.names=TRUE)
wdth.m.7<-summaryBy(airT+vpd+vpdmn~yr,wth.d[wth.d$month==7,],FUN=mean,na.rm=TRUE,keep.names=TRUE)
wdth.7<-merge(wdth.s.7,wdth.m.7,by='yr')
colnames(wdth.7)<-mcolname(wdth.7,1,0,'.7')

wdth.s.8<-summaryBy(ppt+gr~yr,wth.d[wth.d$month==8,],FUN=sum,na.rm=TRUE,keep.names=TRUE)
wdth.m.8<-summaryBy(airT+vpd+vpdmn~yr,wth.d[wth.d$month==8,],FUN=mean,na.rm=TRUE,keep.names=TRUE)
wdth.8<-merge(wdth.s.8,wdth.m.8,by='yr')
colnames(wdth.8)<-mcolname(wdth.8,1,0,'.8')

wdth.678<-merge(merge(wdth.6,wdth.7,by='yr'),wdth.8,by='yr')

wdth<-merge(wdth.m,wdth.678,by='yr')
wdth$year<-wdth$yr
wdth$yr<-NULL
wdd<-merge(wdth,wth,by='year')
stw<-merge(merge(std,wdd,by='year'),fn,by=c('year','plot','rep'),all.x=TRUE)

mxp<-max(wth$ppt[wth$year>2005])
mnp<-min(wth$ppt[wth$year>2005])

stw$apar<-(1-exp(-0.47*stw$lai.s))*stw$rad*0.5
stw$luea<-stw$ANPP/stw$apar
stw$lue<-stw$NPP/stw$apar
# write.table(stw,'Ros_stand_NPP_2021.08.05.txt',sep="\t",quote=FALSE,row.names=FALSE)
stdm<-summaryBy(luea+lue+apar+lai.s+NPP+ANPP+NPPw+NPPf+NPPsw+NPPswe+NPPswl+Vst+Vsw+dVst+dVsw+dVswe+dVswl+dVer+NPPer+swd+swe+swl+swer+ba+sd+dba+log.ba+log.sd+Bf+N.pct+P.pct+ar.NPPBr+sr.NPPBr~plot+year+gs+ppt+airT+rad+hsum+hr20+hr0+vpd,stw,FUN=me)
stdm$p.v<-stdm$ppt/stdm$vpd
stdm$r.dVst<-stdm$dVst.m/stdm$dVst.m[stdm$plot==3]
stdm$r.dVsw<-stdm$dVsw.m/stdm$dVsw.m[stdm$plot==3]
stdm$r.dVswe<-stdm$dVswe.m/stdm$dVswe.m[stdm$plot==3]
stdm$r.dVswl<-stdm$dVswl.m/stdm$dVswl.m[stdm$plot==3]
stdm$r.dVer<-stdm$dVer.m/stdm$dVer.m[stdm$plot==3]
stdm$r.NPPer<-stdm$NPPer.m/stdm$NPPer.m[stdm$plot==3]
stdm$r.NPP<-stdm$NPP.m/stdm$NPP.m[stdm$plot==3]
stdm$r.NPPw<-stdm$NPPw.m/stdm$NPPw.m[stdm$plot==3]
stdm$r.NPPsw<-stdm$NPPsw.m/stdm$NPPsw.m[stdm$plot==3]
stdm$r.NPPswe<-stdm$NPPswe.m/stdm$NPPswe.m[stdm$plot==3]
stdm$r.NPPswl<-stdm$NPPswl.m/stdm$NPPswl.m[stdm$plot==3]
stdm$r.swd<-stdm$swd.m/stdm$swd.m[stdm$plot==3]
stdm$r.swe<-stdm$swe.m/stdm$swe.m[stdm$plot==3]
stdm$r.swl<-stdm$swl.m/stdm$swl.m[stdm$plot==3]

stdm$r.dVst.se[stdm$plot==2]<-sd.mtom(stdm$dVst.m[stdm$plot==2],stdm$dVst.se[stdm$plot==2],stdm$dVst.m[stdm$plot==3],stdm$dVst.se[stdm$plot==3])
stdm$r.dVsw.se[stdm$plot==2]<-sd.mtom(stdm$dVsw.m[stdm$plot==2],stdm$dVsw.se[stdm$plot==2],stdm$dVsw.m[stdm$plot==3],stdm$dVsw.se[stdm$plot==3])
stdm$r.dVswe.se[stdm$plot==2]<-sd.mtom(stdm$dVswe.m[stdm$plot==2],stdm$dVswe.se[stdm$plot==2],stdm$dVswe.m[stdm$plot==3],stdm$dVswe.se[stdm$plot==3])
stdm$r.dVswl.se[stdm$plot==2]<-sd.mtom(stdm$dVswl.m[stdm$plot==2],stdm$dVswl.se[stdm$plot==2],stdm$dVswl.m[stdm$plot==3],stdm$dVswl.se[stdm$plot==3])
stdm$r.dVer.se[stdm$plot==2]<-sd.mtom(stdm$dVer.m[stdm$plot==2],stdm$dVer.se[stdm$plot==2],stdm$dVer.m[stdm$plot==3],stdm$dVer.se[stdm$plot==3])

stdm$r.NPP.se[stdm$plot==2]<-sd.mtom(stdm$NPP.m[stdm$plot==2],stdm$NPP.se[stdm$plot==2],stdm$NPP.m[stdm$plot==3],stdm$NPP.se[stdm$plot==3])
stdm$r.NPPw.se[stdm$plot==2]<-sd.mtom(stdm$NPPw.m[stdm$plot==2],stdm$NPPw.se[stdm$plot==2],stdm$NPPw.m[stdm$plot==3],stdm$NPPw.se[stdm$plot==3])
stdm$r.NPPsw.se[stdm$plot==2]<-sd.mtom(stdm$NPPsw.m[stdm$plot==2],stdm$NPPsw.se[stdm$plot==2],stdm$NPPsw.m[stdm$plot==3],stdm$NPPsw.se[stdm$plot==3])
stdm$r.NPPswe.se[stdm$plot==2]<-sd.mtom(stdm$NPPswe.m[stdm$plot==2],stdm$NPPswe.se[stdm$plot==2],stdm$NPPswe.m[stdm$plot==3],stdm$NPPswe.se[stdm$plot==3])
stdm$r.NPPswl.se[stdm$plot==2]<-sd.mtom(stdm$NPPswl.m[stdm$plot==2],stdm$NPPswl.se[stdm$plot==2],stdm$NPPswl.m[stdm$plot==3],stdm$NPPswl.se[stdm$plot==3])
stdm$r.NPPer.se[stdm$plot==2]<-sd.mtom(stdm$NPPer.m[stdm$plot==2],stdm$NPPer.se[stdm$plot==2],stdm$NPPer.m[stdm$plot==3],stdm$NPPer.se[stdm$plot==3])

stdm$r.swd.se[stdm$plot==2]<-sd.mtom(stdm$swd.m[stdm$plot==2],stdm$swd.se[stdm$plot==2],stdm$swd.m[stdm$plot==3],stdm$swd.se[stdm$plot==3])
stdm$r.swe.se[stdm$plot==2]<-sd.mtom(stdm$swe.m[stdm$plot==2],stdm$swe.se[stdm$plot==2],stdm$swe.m[stdm$plot==3],stdm$swe.se[stdm$plot==3])
stdm$r.swl.se[stdm$plot==2]<-sd.mtom(stdm$swl.m[stdm$plot==2],stdm$swl.se[stdm$plot==2],stdm$swl.m[stdm$plot==3],stdm$swl.se[stdm$plot==3])

stdm$bg<-ifelse(stdm$plot==2,1,'white')

plot(NPP~N.pct,stw[stw$year>2008,],col=plot)
plot(NPP~vpd,stw[stw$year>2008,],col=plot,xlim=c(.2,.7))	
plot(lue~vpd,stw[stw$year>2008,],col=plot,xlim=c(.2,.7))	
plot(lue~ppt,stw[stw$year>2008,],col=plot)
plot(lue~airT,stw[stw$year>2008,],col=plot)
plot(N.pct~vpd,stw[stw$year>2008,],col=plot)
summary(lm(N.pct~as.factor(plot)*vpd,stw[stw$year>2008,]))

par(mfrow=c(2,3))
plot(NA,xlim=c(2005,2018),ylim=c(0,500),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(a)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPP.m[i]+c(-1,1)*stdm$NPP.se[i])}
points(NPP.m~year,stdm[stdm$plot==3,],pch=21,bg='white',cex=.75,type='o')
points(NPP.m~year,stdm[stdm$plot==2,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,900,by=200),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP',' (g C m'^-2,' yr'^-1,')')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(00,500),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(b)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPPw.m[i]+c(-1,1)*stdm$NPPw.se[i])}
points(NPPw.m~year,stdm[stdm$plot==3,],pch=21,bg='white',cex=.75,type='o')
points(NPPw.m~year,stdm[stdm$plot==2,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,900,by=200),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPPw',' (g C m'^-2,' yr'^-1,')')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(00,450),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(c)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPPf.m[i]+c(-1,1)*stdm$NPPf.se[i])}
points(NPPf.m~year,stdm[stdm$plot==3,],pch=21,bg='white',cex=.75,type='o')
points(NPPf.m~year,stdm[stdm$plot==2,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(0,900,by=200),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP of foliage',' (g C m'^-2,' yr'^-1,')')),2,line=1,cex=.8)


plot(NA,xlim=c(2005,2018),ylim=c(.5,2.5),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(d)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 2:14){
lines(rep(stdm$year[i],2),stdm$r.NPP[i]+c(-1,1)*stdm$r.NPP.se[i])}
points(r.NPP~year,stdm[2:14,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Relative NPP to reference')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,4),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(e)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$lai.s.m[i]+c(-1,1)*stdm$lai.s.se[i])}
points(lai.s.m~year,stdm[stdm$plot==3,],pch=21,bg='white',cex=.75,type='o')
points(lai.s.m~year,stdm[stdm$plot==2,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,5,by=1),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Leaf area index')),2,line=1,cex=.8)

plot(NA,xlim=c(2005,2018),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n")
mtext('(f)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$lue.m[i]+c(-1,1)*stdm$lue.se[i])}
points(lue.m~year,stdm[stdm$plot==3,],pch=21,bg='white',cex=.75,type='o')
points(lue.m~year,stdm[stdm$plot==2,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.2),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Light use efficiency (g C MJ'^-1,')')),2,line=1,cex=.8)

#########
plot(ar.NPPBr.m*1.2~year,stdm[stdm$plot==2,],pch=21,bg='white',cex=.75,type='l',ylim=c(0.08,0.5),col=2)
points(sr.NPPBr.m*1.2~year,stdm[stdm$plot==2,],pch=21,bg='white',cex=.75,type='l',col=1)

par(mfrow=c(2,3))

plot(ppt~year,stdm[stdm$plot==2,],pch=22,bg='white',lwd=.75,type='o',xlab="Year",ylab="",yaxt="n",ylim=c(150,450))
axis (2,seq(0,400,by=50),tck=.02,mgp=c(0,0,0),lwd=.3)
mtext('(a)',line=-1,adj=0.05,font=2,cex=8/12)
mtext(expression(paste('Precipiatation (mm growing season-1)')),2,line=1,cex=.8)

par(new=TRUE)
plot(rad~year,stdm[stdm$plot==2,],pch=23,bg=1,lwd=.75,type='o',axes=FALSE,xlab="",ylab="",ylim=c(1800,3000))
axis (4,seq(1000,3000,by=500),tck=.02,mgp=c(0,0,0),lwd=.3,label=TRUE)
mtext(expression(paste('Global radiation',' (MJ m'^-2,' yr'^-1,')')),4,line=1.5,cex=.8)
legend(2014,3000,c('PPT','GR'),pt.bg=c('white',1),pch=c(22,23),col=1,lwd=.8,box.col=FALSE)


plot(NA,xlim=c(2005,2018),ylim=c(100,600),xlab="Year",ylab="",yaxt="n")
mtext('(c)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 1:28){
lines(rep(stdm$year[i],2),stdm$NPP.m[i]+c(-1,1)*stdm$NPP.se[i])}
points(NPP.m~year,stdm[stdm$plot==3,],pch=21,bg='white',lwd=.75,type='o')
points(NPP.m~year,stdm[stdm$plot==2,],pch=21,bg=1,lwd=.75,type='o')
points(NPP.m~year,stdm[stdm$year<2009&stdm$plot==2,],pch=21,bg=8,lwd=.75)
axis (2,seq(0,900,by=200),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP',' (g C m'^-2,' yr'^-1,')')),2,line=1,cex=.8)
legend(2012,170,c('Reference','Fertilized'),pt.bg=c('white',1),pch=21,col=1,lwd=.8,box.col=FALSE)


plot(NA,xlim=c(2005,2018),ylim=c(.5,2.5),xlab="Year",ylab="",yaxt="n")
mtext('(e)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 2:14){
lines(rep(stdm$year[i],2),stdm$r.NPP[i]+c(-1,1)*stdm$r.NPP.se[i])}
points(r.NPP~year,stdm[2:14,],pch=21,bg=1,lwd=.75,type='o')
points(r.NPP~year,stdm[2:4,],pch=21,bg=8,lwd=.75,type='o')
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Relative NPP to reference')),2,line=1,cex=.8)

plot(gs~year,stdm[stdm$plot==2,],pch=24,bg='white',lwd=.75,type='o',xlab="Year",ylab="",yaxt="n",ylim=c(140,230))
axis (2,seq(0,300,by=20),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext('(b)',line=-1,adj=0.05,font=2,cex=8/12)
mtext(expression(paste('Length of annual growing season')),2,line=1,cex=.8)

par(new=TRUE)
plot(airT~year,stdm[stdm$plot==2,],pch=25,bg=1,lwd=.75,type='o',axes=FALSE,xlab="",ylab="",ylim=c(7,14))
axis (4,seq(2,15,by=2),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
mtext(expression(paste('Temperature during growing season (°C)')),4,line=1.5,cex=.8)
legend(2007,14,c('LGS','Temp'),pt.bg=c('white',1),pch=c(24,25),col=1,lwd=.8,box.col=FALSE)


plot(NA,xlim=c(140,170),ylim=c(100,350),xlab="",ylab="",yaxt="n")
mtext('(d)',line=-1,adj=0.05,font=2,cex=8/12)
points(NPPw~gs,stw[stw$plot==3,],pch=21,bg='white',lwd=.75)
points(NPPw~gs,stw[stw$plot==2,],pch=21,bg=1,lwd=.75)
points(NPPw~gs,stw[stw$year<2009&stw$plot==2,],pch=21,bg=8,lwd=.75)
axis (2,seq(0,800,by=100),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('NPP',' (g C m'^-2,' yr'^-1,')')),2,line=1,cex=.8)
mtext(expression(paste('Length of growing season (day yr'^-1,')')),1,line=2.5,cex=.7,at=180)
abline(lm(NPPw~gs,stw[stw$year>2008&stw$plot==2,]))


plot(NA,xlim=c(140,170),ylim=c(.5,2.5),xlab="",ylab="",yaxt="n")
mtext('(f)',line=-1,adj=0.05,font=2,cex=8/12)
for (i in 2:14){
lines(rep(stdm$year[i],2),stdm$r.NPP[i]+c(-1,1)*stdm$r.NPP.se[i])}
points(r.NPP~gs,stdm[stdm$plot==2,],pch=21,bg=1,lwd=.75)
points(r.NPP~gs,stdm[stdm$year<2009&stdm$plot==2,],pch=21,bg=8,lwd=.75)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
mtext(expression(paste('Relative NPP to reference')),2,line=1,cex=.8)
abline(lm(r.NPP~gs,stdm[stdm$year>2008&stdm$plot==2,]))



################



plot(NPP~lai.s,std,col=plot)
stk<-read.table("statistikkorten.txt",head=TRUE,sep='\t')
head(stk)
head(stw)
stk$dvt<-stk$dv+stk$dead.v
stk$plot<-substr(stk$avd,1,1)
stk$rep<-substr(stk$avd,2,2)
sk<-merge(stw[,c('plot','rep','year','d','h','sd','ba','Vst','dba','dVst')],stk[,c('plot','rep','year','std','month','dag','d','meanH','ba','v','dba','dv')],by=c('year','plot','rep'))
sk$ba.x<-sk$ba.x*10
sk$dba.x<-sk$dba.x*10

skm<-summaryBy(Vst+v+dVst+dv+ba.x+ba.y+dba.x+dba.y~plot+rep,sk,FUN=mean,na.rm=TRUE,,keep.names=TRUE)

par(mfrow=c(2,2))
plot(ba.x~ba.y,sk,ylab='BA corrected (m2 ha-1)',xlab='BA from stat-card (m2 ha-1)')
abline(a=0,b=1)
points(ba.x~ba.y,sk[sk$plot==2,],col=2)
points(ba.x~ba.y,sk[sk$year==2005,],cex=1.2,pch=21,bg=1)
points(ba.x~ba.y,sk[sk$year==2005&sk$plot==2,],cex=1.2,pch=21,bg=2)

plot(Vst~v,sk,ylab='Volume corrected (m3 ha-1)',xlab='Volume from stat-card')
abline(a=0,b=1)
points(Vst~v,sk[sk$plot==2,],col=2)
points(Vst~v,sk[sk$year==2005,],cex=1.2,pch=21,bg=1)
points(Vst~v,sk[sk$year==2005&sk$plot==2,],cex=1.2,pch=21,bg=2)

plot(dba.x~dba.y,sk,ylab='BAI corrected (m2 ha-1 yr-1)',xlab='BAI from stat-card)')
abline(a=0,b=1)
points(dba.x~dba.y,sk[sk$plot==2,],col=2)
points(dba.x~dba.y,skm,cex=1.2,pch=21,bg=1)
points(dba.x~dba.y,skm[skm$plot==2,],cex=1.2,pch=21,bg=2)

plot(dVst~dv,sk,ylab='CAI corrected (m3 ha-1 yr-1)',xlab='CAI from stat-card')
abline(a=0,b=1)
points(dVst~dv,sk[sk$plot==2,],col=2)
points(dVst~dv,skm,cex=1.2,pch=21,bg=1)
points(dVst~dv,skm[skm$plot==2,],cex=1.2,pch=21,bg=2)



par(mfrow=c(2,2))
plot(NA,xlim=c(2006,2018),ylim=c(1,2.2),xlab="Year",ylab="F/C",xaxt="n",yaxt="n")
mtext('(a)',line=-1,adj=0.05,font=2,cex=10/12)
for (i in 2:14){
lines(rep(stdm$year[i],2),stdm$r.dVst[i]+c(-1,1)*stdm$r.dVst.se[i])}
points(r.dVst~year,stdm[2:14,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(r.dVst~year,stdm[stdm$plot==2&stdm$year%in%c(2014,2018),],pch=21,bg=2,cex=.75)

plot(NA,xlim=c(mnp,mxp),ylim=c(1,2.2),xlab="Precipitation",ylab="",xaxt="n",yaxt="n")
mtext('(b)',line=-1,adj=0.05,font=2,cex=10/12)
for (i in 2:14){
lines(rep(stdm$ppt[i],2),stdm$r.dVst[i]+c(-1,1)*stdm$r.dVst.se[i])}
points(r.dVst~ppt,stdm[2:14,],pch=21,bg=1,cex=.75)
axis (1,seq(250,420,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(r.dVst~ppt,stdm[stdm$plot==2&stdm$year%in%c(2014,2018),],pch=21,bg=2,cex=.75)

plot(NA,xlim=c(2006,2018),ylim=c(1.5,2.8),xlab="Year",ylab="rF/C",xaxt="n",yaxt="n")
mtext('(a)',line=-1,adj=0.05,font=2,cex=10/12)
for (i in 2:14){
lines(rep(stdm$year[i],2),stdm$r.rV[i]+c(-1,1)*stdm$r.rV.se[i])}
points(r.rV~year,stdm[2:14,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(r.rV~year,stdm[stdm$plot==2&stdm$year%in%c(2014,2018),],pch=21,bg=2,cex=.75)

plot(NA,xlim=c(mnp,mxp),ylim=c(1.5,2.8),xlab="Precipitation",ylab="",xaxt="n",yaxt="n")
mtext('(b)',line=-1,adj=0.05,font=2,cex=10/12)
for (i in 2:14){
lines(rep(stdm$ppt[i],2),stdm$r.rV[i]+c(-1,1)*stdm$r.rV.se[i])}
points(r.rV~ppt,stdm[2:14,],pch=21,bg=1,cex=.75)
axis (1,seq(250,420,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(r.rV~ppt,stdm[stdm$plot==2&stdm$year%in%c(2014,2018),],pch=21,bg=2,cex=.75)


plot(NA,xlim=c(2006,2018),ylim=c(1,2.2),xlab="Year",ylab="F/C",xaxt="n",yaxt="n")
mtext('(a)',line=-1,adj=0.05,font=2,cex=10/12)
for (i in 2:14){
lines(rep(stdm$year[i],2),stdm$r.dBw[i]+c(-1,1)*stdm$r.dBw.se[i])}
points(r.dBw~year,stdm[2:14,],pch=21,bg=1,cex=.75,type='o')
axis (1,seq(2003,2020,by=3),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(r.dBw~year,stdm[stdm$plot==2&stdm$year%in%c(2014,2018),],pch=21,bg=2,cex=.75)

plot(NA,xlim=c(mnp,mxp),ylim=c(1,2.2),xlab="Precipitation",ylab="",xaxt="n",yaxt="n")
mtext('(b)',line=-1,adj=0.05,font=2,cex=10/12)
for (i in 2:14){
lines(rep(stdm$ppt[i],2),stdm$r.dBw[i]+c(-1,1)*stdm$r.dBw.se[i])}
points(r.dBw~ppt,stdm[2:14,],pch=21,bg=1,cex=.75)
axis (1,seq(250,420,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(r.dBw~ppt,stdm[stdm$plot==2&stdm$year%in%c(2014,2018),],pch=21,bg=2,cex=.75)



cof<-summary(lm(dBw~ppt,stw[stw$plot==2,]))$coe[,1]
coc<-mean(stw$dBw[stw$plot==3],na.rm=TRUE)
par(mfrow=c(1,2))
par(mai=c(1,1,1,.1))
plot(NA,ylim=c(200,650),xlim=c(250,420),ylab=expression(paste('Woody tissue increment (g m'^-2,'yr'^-1,')')),xlab=expression(paste('Precipitation (mm growing season'^-1,')')))
	df<-stdm[stdm$year!=2005,]
	for (i in 1:26){
		lines(rep(df$ppt[i],2),df$dBw.m[i]+c(-1,1)*df$dBw.se[i])}
	points(dBw.m~ppt,df,pch=21,bg=bg,cex=.75)
# axis (1,seq(250,420,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
# axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(dBw.m~ppt,df[df$plot==2&df$year%in%c(2017,2018),],pch=21,bg=2,cex=.75)
points(dBw.m~ppt,df[df$plot==3&df$year%in%c(2017,2018),],pch=21,bg=3,cex=.75)
curve(cof[1]+x*cof[2],xlim=c(mnp,mxp),add=TRUE)
curve(coc+x*0,xlim=c(mnp,mxp),add=TRUE,lty=2)
text(256,350,'2018',cex=.8)
text(390,380,'2017',cex=.8)


plot(NA,ylim=c(200,650),xlim=c(9,13.5),ylab=expression(paste('Woody tissue increment (g m'^-2,'yr'^-1,')')),xlab=expression(paste('Temperature during growing season (°C)')))
	df<-stdm[stdm$year!=2005,]
	for (i in 1:26){
		lines(rep(df$airT[i],2),df$dBw.m[i]+c(-1,1)*df$dBw.se[i])}
	points(dBw.m~airT,df,pch=21,bg=bg,cex=.75)
# axis (1,seq(250,420,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
# axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(dBw.m~airT,df[df$plot==2&df$year%in%c(2017,2018),],pch=21,bg=2,cex=.75)
points(dBw.m~airT,df[df$plot==3&df$year%in%c(2017,2018),],pch=21,bg=3,cex=.75)
curve(cof[1]+x*cof[2],xlim=c(mnp,mxp),add=TRUE)
curve(coc+x*0,xlim=c(mnp,mxp),add=TRUE,lty=2)
text(256,350,'2018',cex=.8)
text(390,380,'2017',cex=.8)

plot(NA,ylim=c(200,650),xlim=c(800,1300),ylab=expression(paste('Woody tissue increment (g m'^-2,'yr'^-1,')')),xlab=expression(paste('Heat sum (°C)')))
	df<-stdm[stdm$year!=2005,]
	for (i in 1:26){
		lines(rep(df$airT[i],2),df$dBw.m[i]+c(-1,1)*df$dBw.se[i])}
	points(dBw.m~hsum,df,pch=21,bg=bg,cex=.75)
# axis (1,seq(250,420,by=50),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3,label=TRUE)
# axis (2,seq(-1,3,by=.5),tck=.02,mgp=c(0,0,0),cex.axis=10/12,lwd=.3)
points(dBw.m~hsum,df[df$plot==2&df$year%in%c(2017,2018),],pch=21,bg=2,cex=.75)
points(dBw.m~hsum,df[df$plot==3&df$year%in%c(2017,2018),],pch=21,bg=3,cex=.75)
curve(cof[1]+x*cof[2],xlim=c(mnp,mxp),add=TRUE)
curve(coc+x*0,xlim=c(mnp,mxp),add=TRUE,lty=2)
plot(dBw~year,stw,col=plot)
plot(dBw~gs,stw[stw$year>2009,],col=plot)


hsum.pre<-(lm(dBw~I(hsum^2)+hsum+as.factor(plot),stw))
stw$res.dBw<-predict(hsum.pre,stw)
plot(res.dBw~ppt,stw,col=plot)
plot(dBw~ppt,stw,col=plot)


par(mai=c(1,.1,1,1))
plot(NA,ylim=c(200,650),xlim=c(2006,2018),yaxt='n',ylab='',xlab=expression(paste('Year')))
	df<-stdm[stdm$year!=2005,]
	for (i in 1:26){
		lines(rep(df$year[i],2),df$dBw.m[i]+c(-1,1)*df$dBw.se[i])}
	points(dBw.m~year,df[df$plot==2,],pch=21,bg=bg,cex=.75,type='o')
	points(dBw.m~year,df[df$plot==3,],pch=21,bg=bg,cex=.75,type='o')




############################
	########Fisheye photos#######
#	fs<-read.table('Fisheye_LAI.txt',sep='\t',head=TRUE)
# fs$year<-as.numeric(substr(fs$Date,1,4))
# fs$month<-as.numeric(substr(fs$Date,6,7))
# fs$season<-ifelse(fs$month>7,2,1)
# fs$y.s<-fs$year+(as.numeric(fs$season)-1)*0.5
# fs<-fs[order(fs$y.s,fs$plot,fs$loc),]
# 
# fsm<-summaryBy(LAI_ALEX+LAI_2015~plot+y.s+year+season,fs,FUN=me)
# 
# plot(LAI_ALEX.m~y.s,fsm,col=0,type='o')
# points(LAI_ALEX.m~y.s,fsm[fsm$season==1&fsm$plot==2,],col=plot,type='o')
# points(LAI_2015.m~y.s,fsm[fsm$season==1&fsm$plot==2,],bg=plot,pch=21,type='o')
# 
# plot(LAI_ALEX.m~y.s,fsm,col=0,type='o')
# points(LAI_ALEX.m~y.s,fsm[fsm$season==1&fsm$plot==3,],col=1,type='o')
# points(LAI_2015.m~y.s,fsm[fsm$season==1&fsm$plot==3,],bg=1,pch=21,type='o')
# 
# plot(LAI_ALEX.m~LAI_2015.m,fsm)
# 
# points(LAI_ALEX.m~y.s,fsm[fsm$season==2&fsm$plot==2,],col=plot,pch=2,type='o')
# points(LAI_ALEX.m~y.s,fsm[fsm$season==2&fsm$plot==3,],col=1,pch=2,type='o')
# fmc<-fsm[fsm$y.s%in%c(2006,2011.5,2018.5),]
# fmc$year[fmc$year==2006]<-2005
# fc<-merge(fmc,std1[,c('year','plot','Bfh')])
# plot(LAI_ALEX.m~Bfh,fc,bg=plot,pch=21,ylab="Fisheye",xlab='Leaf biomass')
# points(LAI_2015.m~Bfh,fc,col=plot,pch=1)
# 
# 
# par(mfrow=c(3,4))
# loc<-unique(fs$loc)
# for (i in 1:12){
# plot(LAI_ALEX~y.s,fs[fs$loc==loc[i],],col=0,type='o')
# points(LAI_ALEX~y.s,fs[fs$season==1&fs$loc==loc[i]&fs$plot==2,],col=plot,type='o')
# points(LAI_ALEX~y.s,fs[fs$season==1&fs$loc==loc[i]&fs$plot==3,],col=1,type='o')}

###################