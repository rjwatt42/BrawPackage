#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical",ncats=3),
                           DV=makeVariable("DV"),
                           effect=makeEffect(0.5))
design<-makeDesign(sN=100)

sample<-doSample(hypothesis,design,autoShow = TRUE)

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
                           effect=makeEffect(-0.2,0.,0,0.3))

showPrediction(hypothesis)

#######################
#

BrawOpts("set","allScatter",FALSE
hypothesis<-makeHypothesis(IV=getVariable("Smoker?"),
                           IV2=getVariable("Diligence"),
                           DV=getVariable("IQ"),
                           effect=makeEffect(-0.3,0.3,0,0.3))
showPrediction(hypothesis)

# sample<-doSample(hypothesis,design)
# analysis<-doAnalysis(sample=sample)
# showDescription(analysis)

#######################
#

BrawOpts("set","allScatter",FALSE
hypothesis<-makeHypothesis(IV=getVariable("Smoker?"),
                           IV2=getVariable("Diligence"),
                           DV=getVariable("IQ"),
                           effect=makeEffect(0,0.3,0.4,0))
showHypothesis(hypothesis)


#######################
#
hypothesis<-makeHypothesis(effect=makeEffect(-0.4))
showPopulation(hypothesis)

#######################
#
hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical"),
                           DV=makeVariable("DV","Interval"),
                           effect=makeEffect(0.4))
showPrediction(hypothesis)

hypothesis<-makeHypothesis(IV=makeVariable("IV","Interval"),
                           DV=makeVariable("DV","Interval"),
                           effect=makeEffect(0.4))
showPrediction(hypothesis)

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical",ncats=3),
                           DV=makeVariable("DV","Categorical"),
                           effect=makeEffect(0.4))
showPrediction(hypothesis)

hypothesis<-makeHypothesis(IV=makeVariable("IV","Interval"),
                           DV=makeVariable("DV","Categorical"),
                           effect=makeEffect(0.4))
showPrediction(hypothesis)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Interval"),
                           DV=makeVariable("DV","Interval"),
                           effect=makeEffect(0.4))

sample<-doSample(hypothesis,makeDesign(sN=42))
analysis<-doAnalysis(sample=sample)

showDescription(analysis)

print(analysis$pIV)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical"),
                           DV=makeVariable("DV","Interval"),
                           effect=makeEffect(0))

sample<-doSample(hypothesis,makeDesign(sN=42))

showSample(sample)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("IV","Interval"),
                           DV=makeVariable("DV","Interval",skew=0),
                           effect=makeEffect(0.2,Heteroscedasticity = 0))
design<-makeDesign(sN=120,sOutliers = 0)
sample<-doSample(hypothesis,design)
analysis<-doAnalysis(sample=sample)

showDescription(analysis)

print(analysis$pIV)

#######################
#

hypothesis<-makeHypothesis(IV=makeVariable("Week","Categorical",ncats=5,cases=1:5),
                           IV2=makeVariable("UptoDate?","Categorical",cases=c("No","Yes")),
                           DV=makeVariable("examStress","Interval",skew=0),
                           effect=makeEffect(-0.2,-0.2,0,-0.2))
design<-makeDesign(sN=4000,sOutliers = 0)
sample<-doSample(hypothesis,design)
analysis<-doAnalysis(sample=sample)

showDescription(analysis)

print(analysis$pIV)

