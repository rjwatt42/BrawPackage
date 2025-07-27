#' make a specific hypothesis
#' 
#' @param name  "Psych","PsychF"
#' @returns world object
#' @examples
#' world<-getWorld(name)
#' @export
getWorld<-function(name,result=braw.res$result) {
  switch(name,         
         "Sample"={
           world<-list(worldOn=TRUE,
                       populationPDFsample=TRUE,
                       populationRZ="r",
                       populationPDFsd=result$nval,
                       populationPDFmu=result$rIV,
                       populationNullp=0.0)
         },
         "SampleB"={
           world<-list(worldOn=TRUE,
                       populationPDFsample=TRUE,
                       populationRZ="r",
                       populationPDFsd=result$nval,
                       populationPDFmu=result$rIV,
                       populationNullp=1.0)
         },
         "Null"={
           world<-list(worldOn=TRUE,
                       populationPDF="Single",
                       populationRZ="z",
                       populationPDFk=0,
                       populationPDFmu=0,
                       populationNullp=0,
                       populationPDFsample=FALSE
           )
         },
         "Uniform"={
           world<-list(worldOn=TRUE,
                       populationPDF="Uniform",
                       populationRZ="r",
                       populationPDFk=0.3,
                       populationPDFmu=0,
                       populationNullp=0.0,
                       populationPDFsample=FALSE)
         },
         "Single"={
           world<-list(worldOn=TRUE,
                       populationPDF="Single",
                       populationRZ="r",
                       populationPDFk=0.3,
                       populationPDFmu=0,
                       populationNullp=0.5,
                       populationPDFsample=FALSE)
         },
         "Double"={
           world<-list(worldOn=TRUE,
                       populationPDF="Double",
                       populationRZ="r",
                       populationPDFk=0.3,
                       populationPDFmu=0,
                       populationNullp=0,
                       populationPDFsample=FALSE)
         },
         "Psych"={
           world<-list(worldOn=TRUE,
                       populationPDF="Exp",
                       populationRZ="z",
                       populationPDFk=0.3,
                       populationPDFmu=0,
                       populationNullp=0.74,
                       populationPDFsample=FALSE)
         },
         "Psych50"={
           world<-list(worldOn=TRUE,
                       populationPDF="Exp",
                       populationRZ="z",
                       populationPDFk=0.3,
                       populationPDFmu=0,
                       populationNullp=0.5,
                       populationPDFsample=FALSE)
         },
         "PsychF"={
           world<-list(worldOn=TRUE,
                       populationPDF="Exp",
                       populationRZ="z",
                       populationPDFk=0.3,
                       populationPDFmu=0,
                       populationNullp=0.0,
                       populationPDFsample=FALSE)
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
         "Psych50"={
           hypothesis$effect$world<-getWorld("Psych50")
         },
         "PsychF"={
           hypothesis$effect$world<-getWorld("PsychF")
         },
         "Sample"={
           hypothesis$effect$world<-getWorld("Sample")
         },
         "SampleB"={
           hypothesis$effect$world<-getWorld("SampleB")
         },
         "2C"={
           hypothesis$IV<-makeVariable("IV","Categorical")
         },
         "II"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Interval")
         },
         "IO"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Ordinal")
         },
         "IC"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Categorical")
         },
         "OI"={
           hypothesis$DV<-makeVariable("DV","Ordinal")
           hypothesis$IV<-makeVariable("IV","Interval")
         },
         "OO"={
           hypothesis$DV<-makeVariable("DV","Ordinal")
           hypothesis$IV<-makeVariable("IV","Ordinal")
         },
         "OC"={
           hypothesis$DV<-makeVariable("DV","Ordinal")
           hypothesis$IV<-makeVariable("IV","Categorical")
         },
         "CI"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Interval")
         },
         "CO"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Ordinal")
         },
         "CC"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Categorical")
         },
         "III"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Interval")
         },
         "IIC"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
         },
         "ICI"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Interval")
         },
         "ICC"={
           hypothesis$DV<-makeVariable("DV","Interval")
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
         },
         "CII"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Interval")
         },
         "CCI"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Interval")
         },
         "CIC"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Interval")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
         },
         "CCC"={
           hypothesis$DV<-makeVariable("DV","Categorical")
           hypothesis$IV<-makeVariable("IV","Categorical")
           hypothesis$IV2<-makeVariable("IV2","Categorical")
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
           design$sNRandSD<-33
         },
         "Within"={
           design$sIV1Use<-"Within"
         },
         "Replication"={
           design$Replication<-makeReplication(TRUE)
         },
         "Replication2"={
           design$Replication<-makeReplication(TRUE,Repeats = 2,Keep="FirstSuccess")
         },
         {}
  )
  return(design)
}
