###########################################
#

source("StartUp.R")

###########################################
#

hypothesis2V<-getHypothesis("3")
sample<-doSample(hypothesis2V,autoShow=TRUE)
analysis<-doAnalysis(sample,autoShow=TRUE)

