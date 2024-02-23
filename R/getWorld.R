#' @export
getHypothesis<-function(name,hypothesis=makeHypothesis()) {
  
  switch(name,
         "Psych"={
           hypothesis$effect$world<-list(worldOn=TRUE,
                                         populationPDF="Exp",
                                         populationRZ="z",
                                         populationPDFk=0.3,
                                         populationNullp=0.74)
         },
         "3"={
           hypothesis$IV2<-makeVariable("IV2")
         },
         {}
         )
  return(hypothesis)
}

#' @export
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
