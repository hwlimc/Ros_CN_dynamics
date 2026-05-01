source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd("/Users/hyli0001/Documents/wd/Rosinedal_2018/Rwork")

####	 SLA analysis
sc<-read.table("Soil_CN_content_2020.txt",head=TRUE)
infr<-read.table("Soil_CN_info_2020.txt",head=TRUE,sep='\t')


sc$trt<-substr(sc$sample,1,1)
sc$plot<-ifelse(sc$trt=='C',3,2)
sc$rep<-substr(sc$sample,3,6)
sc$bd.gcm3[sc$layer=='M']<-sc$dw.g[sc$layer=='M']/(2.54^2*pi*2.54*2)
sc$dw.g.m2[sc$layer=='M']<-sc$bd.gcm3[sc$layer=='M']*0.1*1000000
sc$dw.g.m2[sc$layer=='O']<-sc$dw.g[sc$layer=='O']/(0.05^2*pi)
sc$depth<-substr(sc$label,5,ifelse(sc$layer=='M',6,5))
sc$gCm2<-sc$dw.g.m2*sc$C.cont
sc$gNm2<-sc$dw.g.m2*sc$N.cont

esm<-sc[sc$type=='mineral'&sc$sample%out%c('C-S1','C-S2.1'),]
esm$g.mnr.m2<-(esm$bd.gcm3)*10-(esm$bd.gcm3*10*esm$C.cont)/0.58
esm$gCcm2<-esm$gCm2/10000
esm$gNcm2<-esm$gNm2/10000

esm20<-summaryBy(g.mnr.m2+gCcm2+gNcm2~trt+plot+rep,esm,FUN=sum,keep.names=TRUE)
esm20$dpt<-20
esm10<-summaryBy(g.mnr.m2+gCcm2+gNcm2~trt+plot+rep,esm[esm$depth=='S1',],FUN=sum,keep.names=TRUE)
esm10$dpt<-10
esm0<-summaryBy(g.mnr.m2+gCcm2+gNcm2~trt+plot+rep,esm[esm$depth=='S1',],FUN=function(x){n(x)-1},keep.names=TRUE)
esm0$dpt<-0

es<-rbind(esm0,esm10,esm20)
plot(gCcm2~g.mnr.m2,es,col=plot)

x.out<-summaryBy(g.mnr.m2~dpt,es[es$plot==3,],FUN=mean)[,2]

for (i in 2:3){
	rep<-unique(es$rep[es$plot==i])
	for (j in rep){
		df<-es[es$plot==i&es$rep==j,]
		y1<-spline(df$g.mnr.m2,df$gCcm2,method="hyman",xout=x.out)$y
		y2<-spline(df$g.mnr.m2,df$gNcm2,method="hyman",xout=x.out)$y
es[es$plot==i&es$rep==j&order(df$dpt),'gCcm2.esm']<-y1
es[es$plot==i&es$rep==j&order(df$dpt),'gNcm2.esm']<-y2
	}}
	
plot(dpt~gCcm2.esm,es,col=plot,ylim=c(20,0))
plot(dpt~gNcm2.esm,es,col=plot,ylim=c(20,0))

esmm<-summaryBy(gCcm2.esm+gNcm2.esm+gCcm2+gNcm2~dpt+trt,es[es$dpt!=0,],FUN=me)

par(mfrow=c(1,2))
plot(NA,ylim=c(25,0),xlim=c(0.2,0.8),col=0,xlab="Cumulative SOC stock (g C / cm2)",ylab="Soil depth (cm)" )
for (i in 1:4){
	lines(esmm$gCcm2.m[i]+c(-1,1)*esmm$gCcm2.se[i], c(1,1)*esmm$dpt[i])}

points(dpt~gCcm2.m,esmm[esmm$trt=='C',],ylim=c(20,0),pch=24,bg='white')
points(dpt~gCcm2.m,esmm[esmm$trt=='F',],ylim=c(20,0),pch=24,bg=1)
text(0.5,13,paste('p =',round(t.test(gCcm2~trt,data=es[es$dpt==10,])$p.v,3)))
text(0.6,23,paste('p =',round(t.test(gCcm2~trt,data=es[es$dpt==20,])$p.v,3)))


plot(NA,ylim=c(25,0),xlim=c(0.2,0.8),col=0,xlab="Cumulative SOC stock (g C / cm2)",ylab="ESM depth (cm)" )
for (i in 1:4){
	lines(esmm$gCcm2.esm.m[i]+c(-1,1)*esmm$gCcm2.esm.se[i], c(1,1)*esmm$dpt[i])}

points(dpt~gCcm2.esm.m,esmm[esmm$trt=='C',],ylim=c(20,0),pch=21,bg='white')
points(dpt~gCcm2.esm.m,esmm[esmm$trt=='F',],ylim=c(20,0),pch=21,bg=1)
text(0.5,13,paste('p =',round(t.test(gCcm2.esm~trt,data=es[es$dpt==10,])$p.v,3)))
text(0.6,23,paste('p =',round(t.test(gCcm2.esm~trt,data=es[es$dpt==20,])$p.v,3)))


t.test(gCcm2.esm~trt,data=es[es$dpt==10,])
t.test(gCcm2.esm~trt,data=es[es$dpt==20,])

t.test(gCcm2~trt,data=es[es$dpt==10,])
t.test(gCcm2~trt,data=es[es$dpt==20,])



plot(gCcm2~g.mnr.m2,es[es$plot==3&es$rep=='B2',],xlim=c(0,45),ylim=c(0,1))
df<-es[es$plot==3&es$rep=='B2',]
lines(spline(df$g.mnr.m2,df$gCcm2,method="hyman")$x,,col=2)
points(x.out,spline(df$g.mnr.m2,df$gCcm2,method="hyman",xout= x.out)$y,col=2,)



quartz()
par(mfrow=c(2,2))
boxplot(bd.gcm3~plot,sc[sc$type=='mineral'&sc$depth=='S1',],ylim=c(1,2.7),ylab='Bulk density',main='0-10cm')
boxplot(bd.gcm3~plot,sc[sc$type=='mineral'&sc$depth=='S2',],ylim=c(1,2.7),ylab='Bulk density',main='10-20cm')
boxplot(C.cont~plot,sc[sc$type=='mineral'&sc$depth=='S1',],ylim=c(0,0.03),ylab='C conc.',main='0-10cm')
boxplot(C.cont~plot,sc[sc$type=='mineral'&sc$depth=='S2',],ylim=c(0,0.03),ylab='C conc.',main='10-20cm')

# sc1<-sc
# sc1$rep[sc1$rep=='S2.1']<-'S1'
# sc1$rep[sc1$rep=='C4.1']<-'D4'

scc<-sc
scc$rep[scc$sample=='C-S2'&scc$depth=='S2']<-'S1'
scc$rep[scc$sample=='C-S2.1']<-'S2'

scc$rep[scc$sample=='C-C4'&scc$layer=='M']<-'D4'
scc$rep[scc$sample=='C-C4.1']<-'C4'
scci<-merge(scc,infr,by=c('trt','rep'))

scld<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep+layer+depth+dist.tree+angle+ba+note,scci,FUN=sum,keep.names=TRUE)
scl<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep+layer+dist.tree+angle+ba+note,scci,FUN=sum,keep.names=TRUE)
scr<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep+dist.tree+angle+ba+note,scci,FUN=sum,keep.names=TRUE)
scb<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep+dist.tree+angle+ba+note,scci[scci$type!='vegetation',],FUN=sum,keep.names=TRUE)

scld$gCgdw<-scld$gCm2/scld$dw.g.m2


t.test(gCgdw~trt,scld[scld$depth=='S1',])
t.test(gCgdw~trt,scld[scld$depth=='S2',])
t.test(gCgdw~trt,scld[scld$depth=='B',])
t.test(gCgdw~trt,scld[scld$depth=='G',])
t.test(gCgdw~trt,scl[scl$layer=='M',])
t.test(gCgdw~trt,scl[scl$layer=='O',])


t.test(gCm2~trt,scld[scld$depth=='S1',])
t.test(gCm2~trt,scld[scld$depth=='S2',])
t.test(gCm2~trt,scld[scld$depth=='B',])
t.test(gCm2~trt,scld[scld$depth=='G',])
t.test(gCm2~trt,scl[scl$layer=='M',])
t.test(gCm2~trt,scl[scl$layer=='O',])
t.test(gCm2~trt,scr)
t.test(gCm2~trt,scb)

t.test(gNm2~trt,scld[scld$depth=='G',])
t.test(gNm2~trt,scld[scld$depth=='B',])
t.test(gNm2~trt,scl[scl$layer=='O',])
t.test(gNm2~trt,scld[scld$depth=='S1',])
t.test(gNm2~trt,scld[scld$depth=='S2',])
t.test(gNm2~trt,scl[scl$layer=='M',])
t.test(gNm2~trt,scr)
t.test(gNm2~trt,scb)

t.test(dw.g.m2~trt,scld[scld$depth=='G',])
t.test(dw.g.m2~trt,scld[scld$depth=='B',])
t.test(dw.g.m2~trt,scl[scl$layer=='O',])
t.test(dw.g.m2~trt,scld[scld$depth=='S1',])
t.test(dw.g.m2~trt,scld[scld$depth=='S2',])
t.test(dw.g.m2~trt,scl[scl$layer=='M',])
t.test(dw.g.m2~trt,scr)
t.test(dw.g.m2~trt,scb)


scm1<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep+layer+depth,scc,FUN=sum,keep.names=TRUE)
scm2<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep+layer,scm1,FUN=sum,keep.names=TRUE)
scm<-summaryBy(dw.g.m2+gCm2+gNm2~trt+rep,scm2,FUN=sum,keep.names=TRUE)


cnld<-summaryBy(dw.g.m2+gCm2+gNm2~trt+layer+depth,scm1,FUN=me)
summaryBy(dw.g.m2+gCm2+gNm2~trt+layer,scm2,FUN=me)
summaryBy(dw.g.m2+gCm2+gNm2~trt,scm,FUN=me)

js<-read.table("soil_Ros_2015_JM_SL.txt",head=TRUE,sep='\t')
js$org<-(js$hums+js$lich+js$moss_d)*10000/123.4079
js$orgC<-js$org*ifelse(js$trt=='F',.5,.46)

summaryBy(org+orgC~trt,js,FUN=me)

summaryBy(org+orgC~trt,summaryBy(org+orgC~trt+loc,js,FUN=mean,keep.names=TRUE),FUN=me)
cnld[cnld$layer=='O'&cnld$depth=='B',]

cmp15<-js[js$loc%in%c('A','B','C','D','E'),]
cmp201<-scc[scc$rep%in%c('S1','S2','S3','S4')&scc$layer=='O',c('trt','type','dw.g.m2','depth','rep')]
cmp20<-summaryBy(dw.g.m2~depth+trt+rep,cmp201[cmp201$depth=='B',],FUN=sum,keep.names=TRUE)
cmp20[order(cmp20$trt,cmp20$rep),c('trt','rep','dw.g.m2')]
cmp20$loc<-c('D','E',NA,'B',NA,'C','A')

cmp<-merge(cmp15,cmp20[!is.na(cmp20$loc),c('loc','dw.g.m2')])
cmp$d.dw<-cmp$dw.g.m2-cmp$org

cmpm<-summaryBy(d.dw~trt+loc,cmp,FUN=me)
summaryBy(d.dw.m~trt,cmpm,FUN=mean)
summaryBy(d.dw.se~trt,cmpm,FUN=sd.mean)






