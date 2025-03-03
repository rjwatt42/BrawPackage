
x<-read_excel("/Users/rogerwatt/downloads/chung-et-al-2016.xlsx","data")

useType<-"iTBS"
useMethod<-"random"

# basic data
nval<-as.numeric(unlist(x[3:225,7]))
gval<-as.numeric(unlist(x[3:225,28]))
type<-unlist(x[3:225,16])
# remove missing 
use<- !is.na(nval) & !is.na(gval) & nval>3
nval<-nval[use]
gval=gval[use]
type<-type[use]
# convert g to r
rval<-gval/sqrt(gval^2+4)

# d=2*r/sqrt(1-r^2)
# r=d/sqrt(d^2+4)

studies<-list(result=list(rIV=rval[type==useType],
                          nval=nval[type==useType],
                          df1=nval[type==useType]*0+1,
                          rpIV=nval[type==useType]*0))

mMLE<-makeMetaAnalysis(analysisType=useMethod,method="MLE",analyseBias=TRUE)
mTF<-makeMetaAnalysis(analysisType=useMethod,method="TF",analyseBias=TRUE)

resMLE<-doMetaAnalysis(studies,mMLE,TRUE)
resTF<-doMetaAnalysis(studies,mTF,TRUE)

braw.env$minN<<-5
braw.env$maxN<<-120
braw.env$RZ<<-"d"
braw.env$d_range<<-5.5
print(showMetaSingle(resTF,showType="1/se"))
print(showMetaSingle(resMLE,showType="1/se"))
