library('collapse')
getwd()
####	 SLA analysis -- samples from 2018 harvests
scan.la<-read.table("../original_files/hav_2018_sample_needle_area.txt",head=TRUE)
scan.la$area.cm<-scan.la$area_in*(2.54*2.54)

sla1<-collap(scan.la,area.cm~Snr+Brnr+age,scan.la,FUN=list(s=fsum,n=fnobs))
sla1$rn<-sla1$n.area.cm
sla1$n.area.cm<-NULL


######################## 2026 April 28



br.lm<-read.table("../original_files/DW_sample_needle_scan_Ro_2018.txt",head=TRUE,sep='\t')
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
sla2011<-read.table("Ros_SLA_2011.txt",head=TRUE)
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