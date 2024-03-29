##################################################################################    
# LIKELIHOOD

makePossible<-function(typePossible="Samples",
                       UseSource="world",targetSample=0.3,
                       UsePrior="none",prior=getWorld("Psych"),targetPopulation=0.3,
                       hypothesis=makeHypothesis(),design=makeDesign(),
                       sigOnly=FALSE,
                       possibleSimSlice=0.1,possibleCorrection=TRUE,
                       possibleHQ=FALSE,
                       appendSim=FALSE,possibleLength="10"
) {
  
  possible<-
  list(type=typePossible,
       UseSource=UseSource,
       world=hypothesis$effect$world,
       design=design,
       targetSample=targetSample,
       UsePrior=UsePrior,
       prior=prior,
       targetPopulation=targetPopulation,
       sigOnly=sigOnly,
       showTheory=TRUE,
       possibleSimSlice=possibleSimSlice,possibleCorrection=possibleCorrection,
       possibleHQ=possibleHQ
  )
  if (possible$world$worldOn==FALSE) {
    possible$world$populationPDF<-"Single"
    possible$world$populationRZ<-"r"
    possible$world$populationPDFk<-hypothesis$effect$rIV
    possible$world$populationNullp<-0
  }
  
  return(possible)
}

