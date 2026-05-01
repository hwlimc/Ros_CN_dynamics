source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd("/Users/hyli0001/Documents/Sweden/Rosinedal 2018/Rwork")
ll<-read.table("leaf_longevity.txt",head=TRUE)
head(ll)

for(i in 0:5) for (j in 1:12){
ll$Bf.re[ll$year==i&ll$Tnr==j]<-ll$Bf[ll$year==i&ll$Tnr==j]/mean(ll$Bf[ll$year%in%0:1&ll$Tnr==j])}

for(i in 3:5) for (j in 1:12){
	ll$lng[ll$year==i&ll$Tnr==j]<-(ll$Bf.re[ll$year==(i-1)&ll$Tnr==j]-ll$Bf.re[ll$year==i&ll$Tnr==j])*i}
for (j in 1:12){
	ll$lng[ll$year==2&ll$Tnr==j]<-(1-ll$Bf.re[ll$year==2&ll$Tnr==j])*2}

llm<-summaryBy(lng~Tnr+trt,ll,FUN=sum,na.rm=TRUE)
summaryBy(lng.sum~trt,llm,FUN=me)


llf<-summary(nls(Bf.re~1/(1+exp(a*year+b)),ll[ll$trt=="F",],start=list(a=-2.17,b=2.88)))
llc<-summary(nls(Bf.re~1/(1+exp(a*year+b)),ll[ll$trt=="C",],start=list(a=-2.17,b=2.88)))
llf$coe['a',1]
llf$coe['b',1]
llc$coe['a',1]
llc$coe['b',1]

llm<-summaryBy(Bf.re~trt+year,ll,FUN=me)

plot(Bf.re~year,ll,col=0,ylab="Relative foliage biomass")
points(Bf.re.m~year,llm[llm$trt=="F",],bg=2,pch=21)
points(Bf.re.m~year,llm[llm$trt=="C",],bg=1,pch=21)
for (i in 1:10){
	lines(c(-1,1)*Bf.re.se[i]+Bf.re.m[i]~rep(year[i],2),llm)}
curve(1/(1+exp(llf$coe['a',1]*x+llf$coe['b',1])),add=TRUE,xlim=c(0,5))
curve(1/(1+exp(llc$coe['a',1]*x+llc$coe['b',1])),add=TRUE,xlim=c(0,5))

mtext(paste("Half-life (year)"),adj=.95,line=-1.5)
mtext(paste("Control:",round(-llc$coe['b',1]/llc$coe['a',1],2)),adj=.95,line=-2.7)
mtext(paste("Fertilized:",round(-llf$coe['b',1]/llf$coe['a',1],2)),adj=.95,line=-3.7)
