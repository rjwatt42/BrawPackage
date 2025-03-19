

d<-makeDesign(sN=42,sNRand=TRUE,sNRandSD=33.3,sNRandDist="Gamma")
h<-makeHypothesis(effect=makeEffect(world=makeWorld(worldOn=TRUE,
                                                    populationPDF = "Exp",
                                                    populationPDFk = 0.3,
                                                    populationPDFmu = 0,
                                                    populationRZ = "z",
                                                    populationNullp = 0)))
nsamp<-500
rp<-getWorldEffect(nsamp,h$effect)
ns<-getWorldN(nsamp,d)
zs<-atanh(rp)+rnorm(nsamp,0,1/sqrt(ns-3))

# rs<-0.4
# ns<-42
lambda<-seq(0.01,0.99,length.out=1001)
hmodel<-h

res<-getLogLikelihood(abs(zs),ns,df1=1,"Exp",lambda,spread=0)
# 
# res<-rep(0,length(lambda))
# for (ei in 1:length(lambda)) {
#   hmodel$effect$world$populationPDFk<-lambda[ei]
#   a<-doPossible(makePossible(zs,ns,hypothesis=hmodel,axisType="z"))
#   res[ei]<-sum(log(a$Theory$sRho_total))
#   print(c(ei,lambda[ei],res[ei],res1[ei]))
# }
mle<-lambda[which.max(res)]

ylim<-c(min(res),max(res))+c(-1,1)*(max(res)-min(res))*0.1
g<-startPlot(xlim=c(0,1),ylim=ylim,
             xticks=makeTicks(),yticks=NULL,
             gaps=c(1,1,1,1)*4,
             xlabel=makeLabel("λ"),ylabel=makeLabel("lk"))

g<-addG(g,dataPath(data.frame(x=lambda,y=res),linewidth=0.5))
g<-addG(g,dataPath(data.frame(x=c(0,0)+mle,y=c(ylim[1],max(res))),colour="white"))
g<-addG(g,dataText(data.frame(x=mle,y=max(res)+diff(ylim)*0.025),paste0("MLE(λ)=",brawFormat(mle)),size=0.6))
print(g)


studies<-list(result=list(rIV=tanh(zs),
                          nval=ns,
                          df1=ns*0+1,
                          rpIV=rp))

braw.env$RZ<<-"z"
braw.env$maxN<<-250
mMLE<-makeMetaAnalysis(On=TRUE,analysisType="world",modelPDF="Exp",method="MLE")
resMLE<-doMetaAnalysis(studies,mMLE,TRUE)
print(showMetaSingle(resMLE))


eX<-doExplore(20,NULL,explore=makeExplore("NoStudies"),hypothesis=h,
             metaAnalysis=mMLE,doingMetaAnalysis = TRUE)
print(showExplore(eX))
