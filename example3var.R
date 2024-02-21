##########################

hypothesis<-makeHypothesis(IV=makeVariable("IV","Categorical"),
                           IV2=makeVariable("IV2","Interval"),
                           DV=makeVariable("DV","Interval"),
                           effect=makeEffect(0.3,-0.3,0,0.))

showPrediction(hypothesis)
