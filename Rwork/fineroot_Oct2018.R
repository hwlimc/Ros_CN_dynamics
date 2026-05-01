source('/Users/hyli0001/Documents/Rwork/functions.R')
setwd("/Users/hyli0001/Documents/wd/Rosinedal_2018/Rwork")
fr<-read.table("fineroot_Rosinedal_Oct2018.txt",head=TRUE,sep='\t')
fr$plot<-as.factor(fr$plot)
fr$rep<-as.factor(fr$rep)
fr$dm.gm<-fr$drymass/((0.045/2)^2*pi)

head(fr)

frm1<-summaryBy(dm.gm~trt+plot+rep+diameter,fr,FUN=sum,na.rm=TRUE,keep.names=TRUE)
frm2<-summaryBy(dm.gm~trt+plot+diameter,frm1,FUN=mean,keep.names=TRUE)
frm<-summaryBy(dm.gm~trt+plot,frm2[frm2$diameter!=2,],FUN=sum,keep.names=TRUE)
t.test(dm.gm~trt,frm)


frm<-summaryBy(dm.gm~trt+plot,fr[fr$diameter==0&fr$depth%in%c('OM'),],FUN=mean,na.rm=TRUE,keep.names=TRUE)
t.test(dm.gm~trt,frm)


fr1<-summaryBy(dm.gm~trt+plot+diameter+depth,fr,FUN=mean,na.rm=TRUE,keep.names=TRUE)

fr22<-read.table("Ros_fineroot_mass_2022.txt",head=TRUE,sep='\t')
fr22$lay2<-substr(fr22$lay,1,1)
head(fr22)
fr22$Bfr_g.cm2<-fr22$Bfr_g/(((3.8/2)^2)*pi)*10000

par(mfrow=c(2,5))
boxplot(dep~plot1,fr22[fr22$lay=='O',],main='Organic layer',xlab='Treatment')
boxplot(Bfr_g.cm2~plot1,fr22[fr22$lay=='M5',],main='0 - 5cm',xlab='Treatment')
boxplot(Bfr_g.cm2~plot1,fr22[fr22$lay=='M10',],main='5 - 10cm',xlab='Treatment')
boxplot(Bfr_g.cm2~plot1,fr22[fr22$lay=='M20',],main='10 - 20cm',xlab='Treatment')
boxplot(Bfr_g.cm2~plot1,fr22[fr22$lay=='M30',],main='20 - 30cm',xlab='Treatment')

fr22$Bfr_g.cm3<-fr22$Bfr_g.cm2/ifelse(fr22$lay=='M30',10,ifelse(fr22$lay=='M20',10,ifelse(fr22$lay=='M10',5,ifelse(fr22$lay=='M5',5,fr22$dep))))

frm22mr<-summaryBy(Bfr_g.cm3~plot1+plot2+lay,fr22,FUN=mean,keep.names=TRUE)
frm22m<-summaryBy(Bfr_g.cm3~plot1+lay,frm22mr,FUN=me,keep.names=TRUE)
frm22m$depth<-c(-7.5,-15,-25,-2.5,2.5,-7.5,-15,-25,-2.5,2.5)
frm22m<-frm22m[order(frm22m$depth),]
plot(depth~Bfr_g.cm3.m,frm22m,col=0,xlim=c(0,35))
for (i in 1:10){
	lines(c(1,-1)*frm22m$Bfr_g.cm3.se[i]+frm22m$Bfr_g.cm3.m[i],rep(frm22m$depth[i],2))}
points(depth~Bfr_g.cm3.m,frm22m[frm22m$plot1==2,],bg=1,pch=21,type='o')
points(depth~Bfr_g.cm3.m,frm22m[frm22m$plot1==3,],bg='white',pch=21,type='o')

frm22<-summaryBy(Bfr_g.cm2~plot1+plot2+rep+lay2,fr22,FUN=sum,keep.names=TRUE)
frm22a<-summaryBy(Bfr_g.cm2~plot1+plot2+rep,fr22,FUN=sum,keep.names=TRUE)

boxplot(Bfr_g.cm2~plot1,frm22[frm22$lay=='M',],main='Mineral soil',xlab='Treatment')
boxplot(Bfr_g.cm2~plot1,frm22a,main='Organic + mineral',xlab='Treatment')





