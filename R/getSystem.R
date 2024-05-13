#' make a specific hypothesis
#' 
#' @param name  "Psych","PsychF"
#' @returns world object
#' @examples
#' world<-getWorld(name
#' @export
getWorld<-function(name) {
  switch(name,         
         "Null"={
           world<-list(worldOn=TRUE,
                       populationPDF="Single",
                       populationRZ="z",
                       populationPDFk=0,
                       populationNullp=0)
         },
         "Single"={
           world<-list(worldOn=TRUE,
                       populationPDF="Single",
                       populationRZ="r",
                       populationPDFk=0.5,
                       populationNullp=0)
         },
         "Double"={
           world<-list(worldOn=TRUE,
                       populationPDF="Double",
                       populationRZ="r",
                       populationPDFk=0.3,
                       populationNullp=0)
         },
         "Psych"={
           world<-list(worldOn=TRUE,
                       populationPDF="Exp",
                       populationRZ="z",
                       populationPDFk=0.3,
                       populationNullp=0.74)
         },
         "PsychF"={
           world<-list(worldOn=TRUE,
                       populationPDF="Exp",
                       populationRZ="z",
                       populationPDFk=0.3,
                       populationNullp=0.0)
         }
  )
  return(world)
}

#' make a specific hypothesis
#' 
#' @param name  "Psych","3"
#' @returns hypothesis object
#' @examples
#' hypothesis<-getHypothesis(name,hypothesis=makeHypothesis())
#' @export
getHypothesis<-function(name,hypothesis=braw.def$hypothesis) {

  switch(name,
         "Null"={
           hypothesis$effect$world<-getWorld("Null")
         },
         "Single"={
           hypothesis$effect$world<-getWorld("Single")
         },
         "Double"={
           hypothesis$effect$world<-getWorld("Double")
         },
         "Psych"={
           hypothesis$effect$world<-getWorld("Psych")
         },
         "PsychF"={
           hypothesis$effect$world<-getWorld("PsychF")
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
getDesign<-function(name,design=braw.def$design) {
 
  switch(name,
         "simple"={
           design$sN<-42
           design$sNRand<-FALSE
         },
         "Psych"={
           design$sN<-52
           design$sNRand<-TRUE
           design$sNRandK<-1.56
         },
         "Within"={
           design$sIV1Use<-"Within"
         },
         "Replication"={
           design$Replication<-makeReplication(TRUE)
         },
         {}
  )
  return(design)
}
