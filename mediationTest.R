

setHypothesis(IV2=makeVariable("IV2"),effect=makeEffect(0.1,0.2,-0.25),layout="path")
showHypothesis()
setDesign(sN=50)
setEvidence(AnalysisTerms = c(TRUE,TRUE,FALSE,TRUE),doSEM=TRUE)

data<-doAnalysis()

print(showInference(data,effectType = "direct"))
print(plotSEMModel(data$SEM))


