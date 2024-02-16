getHypothesis<-function(name,hypothesis=makeHypothesis()) {
  
  switch(name,
         "Psych"={
           hypothesis$effect$world<-list(worldOn=TRUE,
                                         populationPDF="Exp",
                                         populationRZ="z",
                                         populationPDFk=0.3,
                                         populationNullp=0.74)
         },
         {hypothesis$effect$world<-lmakeWorld()}
         )
  return(hypothesis)
}

getDesign<-function(name,design=makeDesign()) {
 
  switch(name,
         "Psych"={
           design$sN<-52
           design$sNRand<-TRUE
           design$sNRandK<-1.56
         },
         {}
  )
  return(design)
}
