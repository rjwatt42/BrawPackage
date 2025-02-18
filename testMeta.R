
devtools::load_all()
d<-makeDesign(sNRand=TRUE,sNRandDist = "Exp")
h<-makeHypothesis(effect=makeEffect(rIV=0.3,rSD=0))
m<-makeMetaAnalysis(nstudies=200,analysisType="fixed",analysisPrior="uniform",sigOnlySource = TRUE,includeBias = TRUE)
md<-doMetaAnalysis(NULL,metaAnalysis=m,hypothesis=h,design=d)
showMetaSingle(md)
