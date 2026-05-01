getwd()

tree<-read.table("../data/Tree.info_Ros_2018.txt",head=TRUE,sep='\t')
tree$d<-(tree$d1+tree$d2)/2
tree$d1<-NULL
tree$d2<-NULL
tree$lc<-tree$h-tree$crown.base

hi<-read.table("../raw_data/harv_11-22_height_increment.txt",head=TRUE,sep='\t')
hi<-hi[order(hi$hav.yr, hi$plot, hi$Snr, -hi$year), ]
hi$dh0<-hi$h.d
hi$dh0[is.na(hi$dh0)] <- 0
grp <- interaction(hi$hav.yr, hi$plot, hi$Snr, drop = TRUE)
hi$h.est <- hi$h - ave(hi$dh0, grp, FUN = function(x) c(0, cumsum(x)[-length(x)]))
hi$dh0<-NULL
hi$h.est<-ifelse(is.na(hi$h.d),NA,hi$h.est)


#########################
####################### 260430



wd1.8<-read.table("../data/Ros_processed_width_denstiy.txt",head=TRUE,sep='\t')[,c('Snr','wd','ba','bai','year')]
wd18<-wd1.8
wd18$d<-revD(wd1.8$ba)
wd18$dd<-wd1.8$wd*2/1000
wd18$wd<-NULL
wd18$ba<-wd1.8$ba/1000000
wd18$bai<-wd1.8$bai/1000000
di18<-wd18[wd18$year>2001,]
for (i in 1:12){
	for (j in 2006:2011){
		di18$rbai[di18$Snr==i&di18$year==j]<-di18$bai[di18$Snr==i&di18$year==j]/sum(di18$bai[di18$Snr==i&di18$year%in%2006:2011])}
	for (j in 2012:2018){
		di18$rbai[di18$Snr==i&di18$year==j]<-di18$bai[di18$Snr==i&di18$year==j]/sum(di18$bai[di18$Snr==i&di18$year%in%2012:2018])	}
	}
quartz()
par(mfrow=c(3,4))
for (i in 1:12){
plot(bai~year,di18[di18$Snr==i,],type='o')
points(bai~year,di18[di18$Snr==i&di18$year==2013,],bg=4,pch=21)
 	}

for (i in 1:12){
	for (j in 2006:2011){
		di18$rbai[di18$Snr==i&di18$year==j]<-di18$bai[di18$Snr==i&di18$year==j]/sum(di18$bai[di18$Snr==i&di18$year%in%2006:2011])}
	for (j in 2012:2018){
		di18$rbai[di18$Snr==i&di18$year==j]<-di18$bai[di18$Snr==i&di18$year==j]/sum(di18$bai[di18$Snr==i&di18$year%in%2012:2018])	}
	}

di18$plot[di18$Snr<7]<-2
di18$plot[di18$Snr>6]<-3

dhi18<-merge(di18,dh18,by=c('Snr','plot','year'))
# plot(rbai~year,dhi18,col=plot)

ih111<-read.table("../data/Ros_H_increments_2011.txt",head=TRUE,sep='\t')
ih11<-melt(ih111,id=c('Snr','d','h','d.underbark'))
ih11$year<-substr(ih11$variable,2,5)
ih11$ih<-ih11$value
ih11<-ih11[,c('Snr','year','d','h','d.underbark','ih')]
ih11<-ih11[order(ih11$year,ih11$Snr),]

id111<-read.table("../data/Revised_ring_width_2011.txt",head=TRUE,sep='\t')
id11<-summaryBy(wd~Snr+year,id111[id111$disk==1.3&id111$year>2001,],FUN=mean,na.rm=TRUE,keep.names=TRUE)
id11$id<-id11$wd*2/1000
id11$wd<-NULL
idh<-merge(ih11,id11,by=c('Snr','year'))
idh$d.m[idh$year==2011]<-idh$d.underbark[idh$year==2011]
idh$h.m[idh$year==2011]<-idh$h[idh$year==2011]

for (j in 1:12){
	for (i in 2011:2002){
		idh$d.m[idh$Snr==j&idh$year==(i-1)]<-(idh$d.m[idh$Snr==j&idh$year==i])-(idh$id[idh$Snr==j&idh$year==i])
		idh$h.m[idh$Snr==j&idh$year==(i-1)]<-(idh$h.m[idh$Snr==j&idh$year==i])-(idh$ih[idh$Snr==j&idh$year==i])}}
dhi11<-idh[,c('Snr','year','d.m','h.m','id','ih')]
colnames(dhi11)<-c('Snr','year','d','h','dd','dh')
dhi11<-dhi11[order(dhi11$year,dhi11$Snr),]
dhi11$ba<-BA(dhi11$d)

for (j in 1:12){
	for (i in 2011:2002){
		dhi11$bai[dhi11$Snr==j&dhi11$year==i]<-dhi11$ba[dhi11$Snr==j&dhi11$year==i]-BA(dhi11$d[dhi11$Snr==j&dhi11$year==i]-dhi11$dd[dhi11$Snr==j&dhi11$year==i])}}

for (j in 1:12){
	for (i in 2006:2011){		
		dhi11$rbai[dhi11$Snr==j&dhi11$year==i]<-dhi11$bai[dhi11$Snr==j&dhi11$year==i]/sum(dhi11$bai[dhi11$Snr==j&dhi11$year%in%2006:2011])
		dhi11$rdh[dhi11$Snr==j&dhi11$year==i]<-dhi11$dh[dhi11$Snr==j&dhi11$year==i]/sum(dhi11$dh[dhi11$Snr==j&dhi11$year%in%2006:2011])}}

dhi11$plot[dhi11$Snr<7]<-2
dhi11$plot[dhi11$Snr>6]<-3
dhi11$Snr<-dhi11$Snr+12


id141<-read.table("../data/Ros_increment_core_2014.txt",head=TRUE,sep='\t')
id141$rep<-NULL
id141$d<-NULL
id141$d.underbark<-id141$d.underbark/1000
id14<-melt(id141,id=c('plot','Snr','d.underbark'))
id14$year<-substr(id14$variable,2,5)
id14$id<-id14$value*2/1000	# two-sided width now
id14<-id14[,c('plot','Snr','year','d.underbark','id')]
id14<-id14[order(id14$year,id14$Snr),]
id14$d.m[id14$year==2013]<-id14$d.underbark[id14$year==2013]
id14<-id14[id14$year>2001,]
# quartz()
# par(mfrow=c(3,6))
# for(i in 19:36){
# plot(id~year,id14[id14$Snr==i,],type='o')}

for (j in 1:36){
	for (i in 2013:2002){
		id14$d.m[id14$Snr==j&id14$year==(i-1)]<-(id14$d.m[id14$Snr==j&id14$year==i])-(id14$id[id14$Snr==j&id14$year==i])}}
di14<-id14[,c('plot','Snr','year','d.m','id')]
colnames(di14)<-c('plot','Snr','year','d','dd')
di14<-di14[order(di14$year,di14$Snr),]
di14$ba<-BA(di14$d)

for (j in 1:36){
	for (i in 2013:2002){
		di14$bai[di14$Snr==j&di14$year==i]<-di14$ba[di14$Snr==j&di14$year==i]-BA(di14$d[di14$Snr==j&di14$year==i]-di14$dd[di14$Snr==j&di14$year==i])}}

for (j in 1:36){
	for (i in 2006:2011){		
		di14$rbai[di14$Snr==j&di14$year==i]<-di14$bai[di14$Snr==j&di14$year==i]/sum(di14$bai[di14$Snr==j&di14$year%in%2006:2011])}}
di14$Snr<-di14$Snr+24
di14$h<-NA
di14$dh<-NA
di14$rdh<-NA

############
dhi<-rbind(rbind(dhi11,dhi18),di14)
#dhi[!is.na(dhi$rbai)&dhi$rbai<.05,]

##### Propotional growth for the last 10 years
plot(rbai~year,dhi[dhi$year%in%2006:2011,],col=plot,ylim=c(0,.36),lwd=.3)
points(rbai~year,dhi[dhi$Snr%in%25:60,],col=plot,cex=.3)
points(rbai~year,dhi[dhi$Snr%in%13:24,],bg=plot,pch=21)
for (i in 2006:2018){
	eco<-summary(lm(rbai~ba+as.factor(plot),dhi[dhi$Snr<13&dhi$year==i,]))$coe[2,4]
	print(i)
	print(eco)
	}
prop.bai<-summaryBy(rbai~plot+year,dhi[dhi$year%in%2006:2018,],FUN=me)

# write.table(dhi[,c('Snr','year','d','h','dd','dh','ba','bai','plot')],"Ros_dba_dh_sample_trees_11.13.18.txt",sep="\t",quote=FALSE,row.names=FALSE)

# swg$plot<-ifelse(swg$trt=='C',3,2)
# dhw<-merge(dhi18,swg[,c('plot','year','Snr','swg')],by=c('plot','year','Snr'))
# dhw$Vst<-exp(predict(Vst.prd,dhw))
# dhw$Vsw<-exp(predict(Vsw.prd,dhw))
# dhw$Bsb<-exp(predict(Bsb.prd,dhw))
# dhw$Bcr<-exp(predict(Bcr.prd,dhw))
# bf.of<-lm(log(Bf)~log(d)+log(h),st)
# br.of<-lm(log(Bbr)~log(d)+log(h),st)
# dhw$Bf<-exp(predict(bf.of,dhw))
# dhw$Bbr<-exp(predict(br.of,dhw))
# # write.table(dhw,'/Users/hyli0001/Desktop/sample_tree_increment_Ros_18.txt',sep='\t',quote=FALSE,row.names=FALSE)


sl<-read.table("Ros_sampletrees.txt",head=TRUE,sep='\t')
sl$year<-sl$år-ifelse(sl$mån<7,1,0)
sl$plot<-substr(sl$avd,1,1)
sl$rep<-substr(sl$avd,2,2)
sl$d<-sl$dia/1000
sl$h<-sl$hojd/10
sl$lc<-(sl$hojd-sl$krong)/10
sl<-sl[sl$plot!=1,c('plot','rep','year','nr','d','h','lc')]
head(sl)

################### Diameter modelling ###################

tl<-read.table("Ros_treelist.txt",head=TRUE,sep='\t')
tl$year<-tl$år-ifelse(tl$mån<7,1,0)
tl$plot<-substr(tl$avd,1,1)
tl$rep<-substr(tl$avd,2,2)
tl$d<-(tl$d1+tl$d2)/2000
unique(tl[tl$år>2012&tl$mån>4,c('år','mån','dag')])
tl[tl$plot==2&tl$rep==1&tl$nr==19&tl$rev==1,]$d<-0.106	## corrected based on ring width
nt<-tl[tl$plot!=1&tl$rep%in%1:3,c('plot','rep','year','nr','d')]

nt.bs<-unique(nt[nt$year==2005,c('plot','rep','nr')])
nt.bs<-nt.bs[order(nt.bs$plot,nt.bs$rep,nt.bs$nr),]
nbs<-as.data.frame(lapply(nt.bs,rep,14))
nbs$year<-rep(2005:2018,each=length(nt.bs[,1]))
ron<-merge(nbs,nt,by=c('year','plot','rep','nr'),all.x=TRUE)
ron<-ron[order(ron$year,ron$plot,ron$rep,ron$nr),]

for (i in 2006:2018){
	ron$d.1[ron$year==i]<-ron$d[ron$year==(i-1)]}
ron$ba<-BA(ron$d)
ron$ba.1<-BA(ron$d.1)
ron$bai<-ron$ba-ron$ba.1
ron$cbai[ron$year%in%2006:2011]<-ron$ba[ron$year==2011]-ron$ba[ron$year==2005]
ron$cbai[ron$year%in%2012:2018]<-ron$ba[ron$year==2018]-ron$ba[ron$year==2011]


## accounting for dead trees' growth during the period of alive
deads<-ron[is.na(ron$d)&is.na(ron$d.1),c('plot','rep','nr','year')]
dds.py<-summaryBy(year~plot+rep+nr,deads,FUN=function(x){min(x,na.rm=TRUE)-2},keep.names=TRUE)
	### year until alive
dead.py<-unique(dds.py[,c('plot','year')])
ddm<-as.data.frame(lapply(dds.py[,-4],rep,13))
ddm$year<-rep(2006:2018)

for (i in 2:3){
	for (j in dead.py$year[dead.py$plot==i]){
		df<-dhi[dhi$Snr<13&dhi$plot==i&dhi$year%in%c(2006:j),]
		df$cbai.spc<-ave(df$bai,df$Snr,FUN=sum)
		df$rbai.spc<-df$bai/df$cbai.spc
		dfm<-summaryBy(rbai.spc~plot+year,df,FUN=me,keep.names=TRUE)
		k<-dds.py$nr[dds.py$plot==i&dds.py$year==j]
		ddm$rbai.dd.m[ddm$nr%in%k&ddm$plot==i&ddm$year%in%c(2006:j)]<-rep(dfm$rbai.spc.m[dfm$plot==i&dfm$year%in%c(2006:j)],n(k))
		ddm$rbai.dd.se[ddm$nr%in%k&ddm$plot==i&ddm$year%in%c(2006:j)]<-rep(dfm$rbai.spc.se[dfm$plot==i&dfm$year%in%c(2006:j)],n(k))
		}}
dead.trees<-ddm
ddm<-ddm[!is.na(ddm$rbai.dd.m),]
ddm$lvy<-ave(ddm$year,ddm$nr,FUN=max)
dead.pn<-unique(ddm[,c('plot','rep','nr','lvy')])

rov<-merge(ron,prop.bai,by=c('plot','year'),all.x=TRUE)
for(i in 1:11){
	p<-dead.pn$plot[i]
	r<-dead.pn$rep[i]
	nr<-dead.pn$nr[i]
	yr<-dead.pn$lvy[i]
		rov[rov$plot==p&rov$rep==r&rov$nr==nr,c('rbai.m','rbai.se','cbai')]<-NA
		rov$rbai.m[rov$plot==p&rov$rep==r&rov$nr==nr&rov$year%in%2006:yr]<-ddm$rbai.dd.m[ddm$plot==p&ddm$rep==r&ddm$nr==nr]
		rov$rbai.se[rov$plot==p&rov$rep==r&rov$nr==nr&rov$year%in%2006:yr]<-ddm$rbai.dd.se[ddm$plot==p&ddm$rep==r&ddm$nr==nr]	
		rov$cbai[rov$plot==p&rov$rep==r&rov$nr==nr&rov$year%in%2006:yr]<-rov$ba[rov$plot==p&rov$rep==r&rov$nr==nr&rov$year==yr]-rov$ba.1[rov$plot==p&rov$rep==r&rov$nr==nr&rov$year==2006]
	}

rov$bai.m<-rov$rbai.m*rov$cbai
rov$bai.se<-rov$rbai.se*rov$cbai
rov$ba.md[rov$year==2005]<-rov$ba[rov$year==2005]

for (i in 2006:2018){
rov$ba.md[rov$year==i]<-rov$ba.md[rov$year==(i-1)]+rov$bai.m[rov$year==i]}
rov$rmba<-rov$ba.md/rov$ba
rov[!is.na(rov$rmba)&rov$rmba>1.4,]
rov$dm<-sqrt(rov$ba.md/pi)*2
for (i in 2006:2018){
	rov$dm.1[rov$year==i]<-rov$dm[rov$year==(i-1)]}
rov$yr<-rov$year-2005

data.check<-merge(dead.trees,rov[,c('plot','rep','nr','year','ba','bai','bai.m','ba.md')],by=c('plot','rep','nr','year'))

tl[tl$plot==2&tl$rep==1&tl$nr==73,]	## avd:21, rev:4, nr:73, wrong.
par(mfrow=c(2,2))
plot(bai.m~bai,rov,ylim=c(-0.001,0.0051),xlim=c(-0.001,0.0051))
abline(a=0,b=1)
plot(ba.md~ba,rov)
abline(a=0,b=1)
plot(bai.m~bai,data.check)
abline(a=0,b=1)
plot(ba.md~ba,data.check)
abline(a=0,b=1)




################### Height Curve ###################
#### Harvested trees
#### annual growth fraction
hi1<-read.table("increment_shoot_Ros_2011_13.txt",head=TRUE,sep='\t')
him<-reshape::melt(hi1,id=c('h.yr','plot','Snr','d','h','lc'),variable_name='year')
him$year<-substr(him$year,2,5)
him$dh<-him$value
him$value<-NULL
him<-him[!is.na(him$dh),]

hi18<-dh18[,c('Snr','plot','year','dh','h')]
hi18$h.yr<-2018
hi12<-him[,c('h.yr','plot','Snr','year','dh','h')]	# harvested year
hia<-rbind(hi12,hi18)

## building a matrix of relative height growth between any two years
	e.y<-2018
	year<-2006:2018
	bs.df<-data.frame(e.y,year)
for (i in 2017:2011){
	e.y<-i
	year<-2006:i
	df<-data.frame(e.y,year)
	bs.df<-rbind(bs.df,df)}
prh<-as.data.frame(lapply(bs.df,rep,2))
prh$plot<-rep(2:3,each=nrow(bs.df))
for (i in 2018:2011){
	for (j in 2006:(i-5)){
	df<-hia[hia$year%in%(j:i),]
	df$cdh<-ave(df$dh,df$plot,df$Snr,FUN=sum)
	df$ndh<-ave(df$dh,df$plot,df$Snr,FUN=n)
	df<-df[df$ndh==(i-j+1),]
	df$rdh<-df$dh/df$cdh
	dfm<-summaryBy(rdh~plot+year,df,FUN=me)
	#print(me(summaryBy(ndh~plot+year,df,FUN=se)$ndh.se))
	for (k in 2:3){
		prh[prh$plot==k&prh$e.y==i&prh$year%in%(j:i),paste('m',nrow(dfm)/2,sep='')]<-dfm$rdh.m[dfm$plot==k]
		prh[prh$plot==k&prh$e.y==i&prh$year%in%(j:i),paste('se',nrow(dfm)/2,sep='')]<-dfm$rdh.se[dfm$plot==k]
		}}}
summaryBy(m6+m7+m8+m9+m10+m11+m12+m13~plot+e.y,prh,FUN=sum,na.rm=TRUE)

#### Sample trees measured
#### Applying the relative growth matrix to sample trees measured 
#### Computing height  difference between first and last measurements
#### ys:starting year; ye:ending year; hs:starting height; he:ending height...
#### h.diff: height difference during a number of year (yn)

sln<-merge(sl[,c('plot','rep','year','nr','h','lc')],nt,by=c('plot','rep','year','nr'),x.all=TRUE)
head(sln,10)
slb<-sln[order(sln$plot,sln$rep,sln$nr,sln$year),]
# write.table(slb,"sample.tree.measurement.txt",sep="\t",quote=FALSE,row.names=FALSE)

for (i in 2:3) for (j in 1:3){
	for (k in unique(slb$nr[slb$plot==i&slb$rep==j])){
		nrw<-nrow(slb[slb$plot==i&slb$rep==j&slb$nr==k,])
		slb$ys[slb$plot==i&slb$rep==j&slb$nr==k][1]<-slb$year[slb$plot==i&slb$rep==j&slb$nr==k][1]
		slb$ye[slb$plot==i&slb$rep==j&slb$nr==k][1]<-slb$year[slb$plot==i&slb$rep==j&slb$nr==k][nrw]
		slb$hs[slb$plot==i&slb$rep==j&slb$nr==k][1]<-slb$h[slb$plot==i&slb$rep==j&slb$nr==k][1]
		slb$he[slb$plot==i&slb$rep==j&slb$nr==k][1]<-slb$h[slb$plot==i&slb$rep==j&slb$nr==k][nrw]
		slb$ds[slb$plot==i&slb$rep==j&slb$nr==k][1]<-slb$d[slb$plot==i&slb$rep==j&slb$nr==k][1]
		slb$de[slb$plot==i&slb$rep==j&slb$nr==k][1]<-slb$d[slb$plot==i&slb$rep==j&slb$nr==k][nrw]}
		}
slb$h.diff<-slb$he-slb$hs
slb$yn<-slb$ye-slb$ys
sl<-slb[!is.na(slb$yn)&slb$yn>5,]
hs<-sl

# start 2005 year, 13 of 13 years, ending 2013

# Ending i
# Response length j
# Annual growth k
for (l in 2:3){
for (i in 2018:2011){
	for (j in 6:(i-2005)){
		for (k in i-(1:j)+1){
			hs[hs$plot==l&hs$ye==i&hs$yn==j,paste('h',k,sep='')]<-hs$h.diff[hs$plot==l&hs$ye==i&hs$yn==j]*prh[prh$plot==l&prh$e.y==i&prh$year==k,paste('m',j,sep='')]
			hs[hs$plot==l&hs$ye==i&hs$yn==j,paste('e',k,sep='')]<-hs$h.diff[hs$plot==l&hs$ye==i&hs$yn==j]*prh[prh$plot==l&prh$e.y==i&prh$year==k,paste('se',j,sep='')]}}}}
prh[prh$plot==l&prh$e.y==i&prh$year==k,]

### Calculation check
hsc<-hs
coln<-colnames(hsc)
cn<-coln[order(coln)][19:31]
hsc$h.df<-rowSums(hsc[,coln[order(coln)][19:31]],na.rm=TRUE)

for (i in 2006:2018){
	hsc[,paste('ch',i,sep='')]<-hsc[,paste('h',i,sep='')]/hsc$h.df
	hsc[,paste('ce',i,sep='')]<-hsc[,paste('e',i,sep='')]/hsc$h.df}

colcn<-colnames(hsc)
chk<-hsc[,colcn[c(1,2,8,9,15,43:68)]]
chkm<-melt(chk,id=c('plot','rep','ys','ye','yn'))
chkm$var<-substr(chkm$variable,2,2)
chkm$year<-as.numeric(substr(chkm$variable,3,6))
chm<-unique(chkm[,c('plot','ye','yn','value','var','year')])
chm$e.y<-chm$ye
cht<-merge(chm[chm$var=='h',c('plot','year','e.y','yn','value')],chm[chm$var=='e',c('plot','year','e.y','yn','value')],by=c('plot','year','e.y','yn'))
pck<-melt(prh,id=c('e.y','year','plot'))
pck$var<-substr(pck$variable,1,1)
pck$yn<-as.numeric(substr(pck$variable,ifelse(pck$var=='m',2,3),4))
pct<-merge(pck[pck$var=='m',c('plot','year','e.y','yn','value')],pck[pck$var=='s',c('plot','year','e.y','yn','value')],by=c('plot','year','e.y','yn'))

comp<-merge(cht,pct,by=c('plot','year','e.y','yn'),all=TRUE)
lm(value.x.x~value.x.y,comp)
lm(value.y.x~value.y.y,comp)

### Building then-current height
dh.m<-melt(hs[,c('plot','rep','nr','ys','ye','yn','hs','he','h.diff','d',paste('h',2018:2006,sep=''))],id=c('plot','rep','nr','ys','ye','yn','hs','he','h.diff','d'))
dh.m$year<-substr(dh.m$variable,2,5)
dh.m$dh<-dh.m$value
dh.m$variable<-NULL
dh.m$value<-NULL

dh.e<-melt(hs[,c('plot','rep','nr','ys','ye','yn','hs','he','h.diff','d',paste('e',2018:2006,sep=''))],id=c('plot','rep','nr','ys','ye','yn','hs','he','h.diff','d'))
dh.e$year<-substr(dh.e$variable,2,5)
dh.e$se<-dh.e$value
dh.e$variable<-NULL
dh.e$value<-NULL

dhm<-merge(dh.m,dh.e,by=c('plot','rep','nr','ys','ye','yn','hs','he','h.diff','d','year'))

sh1<-dhm[order(dhm$plot,dhm$rep,dhm$nr,dhm$ys,dhm$year),]
sh<-sh1[!is.na(sh1$dh),]

for (i in 2:3) for (j in 1:3){
	for (k in unique(sh$nr[sh$plot==i&sh$rep==j])){
		for (l in 1:sh$yn[sh$plot==i&sh$rep==j&sh$nr==k][1]){
			sh$hm[sh$plot==i&sh$rep==j&sh$nr==k][l]<-sh$hs[sh$plot==i&sh$rep==j&sh$nr==k][l]+sum(sh$dh[sh$plot==i&sh$rep==j&sh$nr==k][1:l])
			sh$hm.se[sh$plot==i&sh$rep==j&sh$nr==k][l]<-sqrt(sum(sh$se[sh$plot==i&sh$rep==j&sh$nr==k][1:l]^2))}}}

inh<-hs[,c('plot','rep','nr','ys','ye','yn','hs','he','h.diff','d','year','h')]
inh$hm<-inh$h
inh$h<-NULL
inh$dh<-NA
inh$se<-NA
inh$hm.se<-0
sh.o<-rbind(sh,inh)
hm1<-sh.o[,c('plot','rep','nr','year','d','hm','hm.se')]
hm<-merge(hm1,rov[,c('plot','rep','nr','year','dm','yr')],by=c('plot','rep','nr','year'))
hm<-hm[order(hm$plot,hm$rep,hm$nr,hm$year),]
# write.table(hm,"D_H_Reconstructed.txt",sep="\t",quote=FALSE,row.names=FALSE)
## Developing height curve ###
#### diameter in mm; height in m
plt<-rep(2:3,each=3)
rpl<-rep(1:3,2)
hm.tb<-data.frame(plt,rpl)
hm.tb$a<-NA
hm.tb$b<-NA
hm.tb$c<-NA
for (j in 2:3) for (k in 1:3){
	df<-hm[hm$plot==j&hm$rep==k,]
	h.func<-nls(hm~c*yr+1.3+(dm/(a+b*dm))^(2),data=df,start=list(a=0,b=0.5,c=0.2))
rov$h.pre[rov$plot==j&rov$rep==k]<-predict(h.func,rov[rov$plot==j&rov$rep==k,])
hm.tb[hm.tb$plt==j&hm.tb$rpl==k,3:5]<-summary(h.func)$coe[,1]
}
# write.table(hm.tb,"d_h_model.txt",sep="\t",quote=FALSE,row.names=FALSE)


# #  Check in each year in each plot, the effect of rep on the model.
# for (i in 2:3) for (j in 2005:2018){
	# print(paste(j,i))
# print(summary(aov(h.pre~dm*rep,hm[hm$plot==i&hm$year==j,])))
# }

# par(mfrow=c(2,7))
# for (k in 2005:2018){
	# plot(h.pre~dm,hm[hm$year==k,],col=0,main=k)
	# for (i in 2:3) for (j in 1:3){
		# df<-hm[hm$plot==i&hm$rep==j&hm$year==k,]
		# ddf<-rov[rov$year==k&rov$plot==i&rov$rep==j,]
		# points(h.pre~dm,ddf,cex=.1,col=1,pch=21)
		# points(h.pre~dm,df,cex=.8,col=2,pch=1)
# }}

##### Crown ratio
sln$cr<-sln$lc/sln$h
crm<-sln[sln$cr<0.8,]
crm$dm<-crm$d

crm$yr<-crm$year-2005
cr.model<-lm(cr~dm*as.integer(yr)+dm:as.factor(plot)+as.integer(yr):as.factor(plot)+as.factor(plot):as.factor(rep),crm)

rov$cr.pre<-predict(cr.model,rov)
rov$lc<-rov$h.pre*rov$cr.pre
ro<-rov[,c('plot','rep','nr','year','dm','h.pre','lc')] 
colnames(ro)<-c('plot','rep','nr','year','d','h','lc')


ro$bc<-ro$h-ro$lc
yr<-unique(ro$year)
plt<-unique(ro$plot)
rp<-unique(ro$rep)
nr<-unique(ro$nr)


for (i in yr) for (j in plt) for (k in rp) for (l in nr){
	ro$h.1[ro$year==(i+1)&ro$plot==j&ro$rep==k&ro$nr==l]<-ro$h[ro$year==i&ro$plot==j&ro$rep==k&ro$nr==l]
	ro$d.1[ro$year==(i+1)&ro$plot==j&ro$rep==k&ro$nr==l]<-ro$d[ro$year==i&ro$plot==j&ro$rep==k&ro$nr==l]
	ro$bc.1[ro$year==(i+1)&ro$plot==j&ro$rep==k&ro$nr==l]<-ro$bc[ro$year==i&ro$plot==j&ro$rep==k&ro$nr==l]
ro$lc.1[ro$year==(i+1)&ro$plot==j&ro$rep==k&ro$nr==l]<-ro$lc[ro$year==i&ro$plot==j&ro$rep==k&ro$nr==l]
	}

# write.table(ro,"inv.tree.dimensions.txt",sep="\t",quote=FALSE,row.names=FALSE)
btn<-hm[hm$plot==2,c('plot','rep','nr','year','hm','dm')]
bt.ro<-merge(btn,sln,by=c('plot','rep','nr','year'))
bt.ro$plot<-NULL
bt.ro$hm<-NULL
bt.ro$dm<-NULL
bt.ro<-bt.ro[order(bt.ro$rep,bt.ro$nr,bt.ro$year),]

# write.table(bt.ro,"/Users/hyli0001/Documents/Sweden/7_Branch_turnover/Rwork/Branch_turnover_samples.txt",sep="\t",quote=FALSE,row.names=FALSE)

