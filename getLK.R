############################


d<-makeDesign(sN=42,sNRand=TRUE,sNRandSD=33.3,sNRandDist="Gamma")
h<-makeHypothesis(effect=makeEffect(world=makeWorld(worldOn=TRUE,
                                                    populationPDF = "Exp",
                                                    populationPDFk = 0.3,
                                                    populationPDFmu = 0,
                                                    populationRZ = "z",
                                                    populationNullp = 0)))
nsamp<-200
sigOnly<-TRUE

if (sigOnly) {
  get<-nsamp*10
} else {
  get<-nsamp
}

rp<-getWorldEffect(get,h$effect)
ns<-getWorldN(get,d)
zs<-atanh(rp)+rnorm(get,0,1/sqrt(ns-3))
p<-rn2p(tanh(zs),ns)

if (sigOnly) {
use<-which(p<0.05)[1:nsamp]
rp<-rp[use]
zs<-zs[use]
ns<-ns[use]
p<-p[use]
}

###########################
# rs<-0.4
# ns<-42
lambda<-seq(0.01,0.99,length.out=1001)
hmodel<-h

res<-getLogLikelihood(abs(zs),ns,df1=1,"Exp",lambda,spread=0,bias=sigOnly)
mle<-resMLE

res0<-getLogLikelihood(abs(zs),ns,df1=1,"Exp",lambda,spread=0,bias=0)
mle0<-lambda[which.max(res0)]

ylim<-c(min(res),max(res))+c(-1,1)*(max(res)-min(res))*0.1
g<-startPlot(xlim=c(0,1),ylim=ylim,
             xticks=makeTicks(),yticks=NULL,
             gaps=c(1,1,1,1)*4,
             xlabel=makeLabel("λ"),ylabel=makeLabel("lk"))

g<-addG(g,dataPath(data.frame(x=lambda,y=res),linewidth=0.5))
g<-addG(g,dataPath(data.frame(x=c(0,0)+mle,y=c(ylim[1],max(res))),colour="white"))
g<-addG(g,dataText(data.frame(x=mle,y=max(res)+diff(ylim)*0.025),paste0("MLE(λ)=",brawFormat(mle)),size=0.6))

if (sigOnly) {
g<-addG(g,dataPath(data.frame(x=lambda,y=res0),linewidth=0.5,linetype = "dotted"))
g<-addG(g,dataPath(data.frame(x=c(0,0)+mle0,y=c(ylim[1],max(res0))),colour="white",linetype = "dotted"))
g<-addG(g,dataText(data.frame(x=mle0,y=max(res0)+diff(ylim)*0.025),paste0("MLE(λ)=",brawFormat(mle0)),size=0.6))
}

print(g)

############################
studies<-list(result=list(rIV=tanh(zs),
                          nval=ns,
                          df1=ns*0+1,
                          rpIV=rp))

braw.env$RZ<<-"z"
braw.env$z_range<<-2.5
braw.env$maxN<<-250
mMLE<-makeMetaAnalysis(On=TRUE,analysisType="world",modelPDF="Exp",method="MLE",
                       sourceBias=sigOnly,analyseBias=sigOnly)
resMLE<-doMetaAnalysis(studies,mMLE,keepStudies=TRUE)
print(showMetaSingle(resMLE))

############################

mMLE$analyseBias<-TRUE
mMLE$sourceBias<-TRUE
eX<-doExplore(20,NULL,explore=makeExplore("NoStudies"),hypothesis=h,
              metaAnalysis=mMLE,doingMetaAnalysis = TRUE)
print(showExplore(eX))


############################

mMLE1<-mMLE
mMLE1$analyseBias<-FALSE
mMLE1$sourceBias<-FALSE

eX1<-doExplore(1000,NULL,explore=makeExplore("NoStudies"),hypothesis=h,
              metaAnalysis=mMLE1,doingMetaAnalysis = TRUE)
print(showExplore(eX1))
