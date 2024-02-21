#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical",ncats=3),
                           DV=makeVariable("DV"),
                           effect=makeEffect(0.5))
design<-makeDesign(sN=100)

sample<-makeSample(hypothesis,design,autoShow = TRUE)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical",ncats=3),DV=makeVariable("DV"),
                           effect=makeEffect(0.5))
showPopulation(hypothesis)

#######################
#
hypothesis<-makeHypothesis(IV=makeVariable("IV","Interval"),DV=makeVariable("DV"),
                           effect=makeEffect(0.2))

showHypothesis(hypothesis)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV"),DV=makeVariable("DV","Categorical",ncats=2),
                           effect=makeEffect(-0.4))
showHypothesis(hypothesis)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical",ncats=2),DV=makeVariable("DV","Categorical",ncats=3),
                           effect=makeEffect(0.3))
showHypothesis(hypothesis)


#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical",ncats=2),IV2=makeVariable("IV2","Categorical",ncats=2),DV=makeVariable("DV","Interval"),
                           effect=makeEffect(-0.03,0.3,0,0))

showPrediction(hypothesis)

#######################
#

hypothesis<-makeHypothesis(IV=getVariable("Smoker?"),
                           IV2=getVariable("Diligence"),
                           DV=getVariable("IQ"),
                           effect=makeEffect(-0.03,0.3,0,0))

sample<-makeSample(hypothesis,design)
analysis<-makeAnalysis(sample=sample)
showDescription(analysis)

