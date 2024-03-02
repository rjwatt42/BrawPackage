#' make a specific hypothesis
#' 
#' @param name  "Psych","3"
#' @returns hypothesis object
#' @examples
#' hypothesis<-getHypothesis(name,hypothesis=makeHypothesis())
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
         "2C"={
           hypothesis$IV<-makeVariable("IV","Categorical")
         },
         "II"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "IO"={
           hypothesis$IV<-makeVariable("IV","Ordinal")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "IC"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "OI"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$DV<-makeVariable("DV","Ordinal")
         },
         "OO"={
           hypothesis$IV<-makeVariable("IV","Ordinal")
           hypothesis$DV<-makeVariable("DV","Ordinal")
         },
         "OC"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$DV<-makeVariable("DV","Ordinal")
         },
         "CI"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         "CO"={
           hypothesis$IV<-makeVariable("IV","Ordinal")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         "CC"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         "III"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Interval")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "IIC"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "ICI"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Interval")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "ICC"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
           hypothesis$DV<-makeVariable("DV","Interval")
         },
         "CII"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Interval")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         "CCI"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Interval")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         "CIC"={
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         "CCC"={
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
           hypothesis$DV<-makeVariable("DV","Categorical")
         },
         {}
         )
  return(hypothesis)
}

#' make a specific design
#' 
#' @param name  "Psych"
#' @returns hypothesis object
#' @examples
#' design<-getDesign(name,design=makeDesign())
#' @export
getDesign<-function(name,design=makeDesign()) {
 
  switch(name,
         "Psych"={
           design$sN<-52
           design$sNRand<-TRUE
           design$sNRandK<-1.56
         },
         "Within"={
           design$sIV1Use<-"Within"
         },
         {}
  )
  return(design)
}
