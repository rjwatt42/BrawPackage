getHypothesis<-function(name,hypothesis=NULL) {
  
  if (is.null(hypothesis))
    hypothesis<-makeHypothesis()
  switch(name,
         "Psych"={
           hypothesis$effect$world<-list(worldOn=TRUE,
                                         populationPDF="Exp",
                                         populationRZ="z",
                                         populationPDFk=0.3,
                                         populationNullp=0.74)
         },
         {makeWorld()}
         )
}

getDesign<-function(name,design=NULL) {
 
  if (is.null(design))
    design<-makeDesign()
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
