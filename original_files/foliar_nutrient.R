# source('/Users/hyli0001/Documents/Rwork/functions.R')
library('collapse')
getwd()
#### Foliar N analysis
fn11<-read.table("../original_files/Foliar_CN_2011_harvests.txt",head=TRUE,sep='\t')


fn<-read.table("../original_files/foliage_nutrients.txt",head=TRUE,sep='\t')

head(fn)
fn$PN<-fn$P/fn$N
fn$KN<-fn$K/fn$N
fn$CaN<-fn$Ca/fn$N
fn$trt<-substr(fn$avd,1,1)
fn$plot<-substr(fn$avd,3,3)

fnm<-collap(fn,N+P+K+Ca+Mg+PN+KN+CaN~trt+year,FUN=list(m=fmean,sd=fsd))

