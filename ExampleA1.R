analysis<-makeAnalysis(makeSample())
showInference(analysis)


hypothesis<-getHypothesis("2C")
design<-getDesign("Within")
design1<-makeDesign()
analysis<-makeAnalysis(makeSample(hypothesis=hypothesis,design=design))
showInference(analysis)


explore<-makeExplore(nsims=10,NULL,"Usage",hypothesis=hypothesis)

