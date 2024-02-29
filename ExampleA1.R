
hypothesis<-getHypothesis("2C")
design<-getDesign("Within")
analysis<-makeAnalysis(makeSample(hypothesis=hypothesis,design=design))
showInference(analysis)
explore<-makeExplore(nsims=10,NULL,"Usage",hypothesis=hypothesis)

