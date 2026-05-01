source('SLA_Allometrics_2018.R')
lf<-read.table("../data/litterfall_Ro.txt",head=TRUE,sep='\t')
lf$trt<-ifelse(lf$plot==2,'F','C')
lf$season<-as.factor(lf$season)
lf2006.1<-summaryBy(needle+cone+twig+bark+mis~year+plot+loc,lf[lf$year==2006&lf$season!=3,],FUN=sum,keep.names=TRUE)
lf2006.1$season<-1
lf<-lf[!(lf$year==2006&lf$season==2),]
lf[lf$year==2006&lf$season==3,'season']<-2
lf[lf$year==2006&lf$season==1,colnames(lf2006.1)]<-lf2006.1
lf$sum<-lf$needle+lf$cone+lf$twig+lf$bark+lf$mis

summer.lf<-summaryBy(needle~year+plot,lf[lf$year!=2020&lf$season==1&lf$note!='broken_trap',],FUN=function(x){me(x)/0.25})	#g dw m-2
colnames(summer.lf)<-c('year','plot','smlf','smlf.se')

lfn<-summaryBy(needle~year+loc+plot,lf[lf$year!=2020&lf$note!='broken_trap',],FUN=n)
lfa1<-merge(lf,lfn,by=c('year','loc','plot'),all.y=TRUE)
lfa<-lfa1[lfa1$needle.n==2,]
lfa$needle.n<-NULL

lfs<-summaryBy(needle+cone+twig+bark+mis+sum~year+season+plot,lfa,FUN=function(x){me(x)/0.25})	#g dw m-2
lfs$y.s<-lfs$year+(as.numeric(lfs$season)-1)*0.5
# write.table(lfs,"Ros_seasonal_litterfall.txt",sep="\t",quote=FALSE,row.names=FALSE)

lfy1<-summaryBy(needle+cone+twig+bark+mis+sum~year+plot+loc,lfa,FUN=sum,keep.names=TRUE)
lfy2<-summaryBy(needle+cone+twig+bark+mis+sum~year+plot,lfy1,FUN=function(x){me(x)/0.25})	#g dw m-2
lfy<-merge(lfy2, summer.lf,by=c('year','plot'))

# plot(needle.m~y.s, lfs[lfs$season==1&lfs$plot==2,],bg=2,pch=21,type='o',ylim=c(0,210))
# points(needle.m~y.s, lfs[lfs$season==1&lfs$plot==3,],bg=1,pch=21,type='o')
# points(needle.m~y.s, lfs[lfs$season==2&lfs$plot==2,],col=2,type='o')
# points(needle.m~y.s, lfs[lfs$season==2&lfs$plot==3,],col=1,type='o')
# for (i in 1:nrow(lfs)){
# 	lines(rep(lfs$y.s[i],2),lfs$needle.m[i]+c(-1,1)*lfs$needle.se[i])}
# 
# plot(needle.m~year, lfy[lfy $plot==2,],col=2,type='o')
# points(needle.m~year, lfy[lfy $plot==3,],col=1,type='o')

lcn<-read.table("../data/LItterfall_CN.txt",head=TRUE,sep='\t')
summary(lm(dC~as.factor(plot)*as.factor(year),lcn[lcn$org=='Needle',]))
summary(lm(C.conc~as.factor(plot)*as.factor(year),lcn[lcn$org=='Needle',]))
summary(lm(C.conc~as.factor(plot)*as.factor(year),lcn[lcn$org=='Others',]))
summary(lm(dC~as.factor(plot)*as.factor(year),lcn[lcn$org=='Others',]))


lcm<-summaryBy(dC+C.conc+dN+N.conc~plot+org+year+season,lcn,FUN=me)
lcm$ns<-ifelse(lcm$season=='F',2,1)
lcm$yr<-ifelse(lcm$year==2013,2013.2,ifelse(lcm$year==2014,2013.8,ifelse(lcm$year==2017,2017.2,2017.8)))
lcm$yr<-lcm$year+ifelse(lcm$org=='Needle',0,.2)
lcl<-lcm[lcm$org=='Needle',]
litter.leaf.C<-mean(lcl$C.conc.m)
lco<-lcm[lcm$org=='Others',]
litter.other.C<-mean(lco$C.conc.m)

# par(mfrow=c(2,2))
# plot(C.conc.m~yr,lcl,ylim=c(53,56),xlim=c(2012.5,2018.5),col=0,ylab='C concentration (%)',xlab='')
# for (i in 1:nrow(lcl)){
	# lines(rep(lcl$yr[i],2),lcl$C.conc.m[i]+c(-1,1)*lcl$C.conc.se[i])}
# points(C.conc.m ~yr,lcl[lcl$plot==3,],bg='white',pch=21,type='o')
# points(C.conc.m ~yr,lcl[lcl$plot==2,],bg=1,pch=21,type='o')

# plot(dC.m~yr,lcl,ylim=c(-29.8,-28.5),xlim=c(2012.5,2018.5),col=0,ylab='d13C (‰)',xlab='')
# for (i in 1:nrow(lcl)){
	# lines(rep(lcl$yr[i],2),lcl$dC.m[i]+c(-1,1)*lcl$dC.se[i])}
# points(dC.m~yr,lcl[lcl$plot==3,],bg='white',pch=21,type='o')
# points(dC.m~yr,lcl[lcl$plot==2,],bg=1,pch=21,type='o')
# # points(dC.m~yr,lco[lco$plot==3,],bg='white',pch=24,type='o')
# # points(dC.m~yr,lco[lco$plot==2,],bg=1,pch=24,type='o')

# plot(N.conc.m~yr,lcl,ylim=c(0.3,1.8),xlim=c(2012.5,2018.5),col=0,ylab='N concentration (%)',xlab='Year')
# for (i in 1:nrow(lcl)){
	# lines(rep(lcl$yr[i],2),lcl$N.conc.m[i]+c(-1,1)*lcl$N.conc.se[i])}
# points(N.conc.m ~yr,lcl[lcl$plot==3,],bg='white',pch=21,type='o')
# points(N.conc.m ~yr,lcl[lcl$plot==2,],bg=1,pch=21,type='o')

# plot(dN.m~yr,lcl,ylim=c(-8,2),xlim=c(2012.5,2018.5),col=0,ylab='d13N (‰)',xlab='Year')
# for (i in 1:nrow(lcl)){
	# lines(rep(lcl$yr[i],2),lcl$dN.m[i]+c(-1,1)*lcl$dN.se[i])}
# points(dN.m~yr,lcl[lcl$plot==3,],bg='white',pch=21,type='o')
# points(dN.m~yr,lcl[lcl$plot==2,],bg=1,pch=21,type='o')
# # points(dC.m~yr,lco[lco$plot==3,],bg='white',pch=24,type='o')
# # points(dC.m~yr,lco[lco$plot==2,],bg=1,pch=24,type='o')



