###########################################
#

source("StartUp.R")

###########################################
#

hypothesis2V<-getHypothesis("3")
sample<-makeSample(hypothesis2V,autoShow=TRUE)
analysis<-doAnalysis(sample,autoShow=TRUE)

