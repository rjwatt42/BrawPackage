#' make a specific hypothesis
#' 
#' @param name  "Psych","PsychF"
#' @returns world object
#' @examples
#' world<-getWorld(name)
#' @export
getWorld<-function(name,rp=0.3,result=braw.res$result) {
  switch(name,         
         "Sample"={
           world<-makeWorld(On=TRUE,
                       PDFsample=TRUE,
                       RZ="r",
                       PDFsampleN=result$nval,
                       PDFsampleRs=result$rIV
                       )
         },
         "SampleB"={
           world<-makeWorld(On=TRUE,
                       PDFsample=TRUE,
                       RZ="r",
                       PDFsampleN=result$nval,
                       PDFsampleRs=result$rIV
           )
         },
         "Null"={
           world<-makeWorld(On=TRUE,
                       PDF="Single",
                       RZ="z",
                       PDFk=0
           )
         },
         "NullH"={
           world<-makeWorld(On=TRUE,
                       PDF="Single",
                       RZ="z",
                       PDFk=0
                       )
         },
         "Uniform"={
           world<-makeWorld(On=TRUE,
                       PDF="Uniform",
                       RZ="r"
                       )
         },
         "Single"={
           world<-makeWorld(On=TRUE,
                       PDF="Single",
                       RZ="r",
                       PDFk=rp,
                       pRplus=0.5
                       )
         },
         "Plain"={
           world<-makeWorld(On=TRUE,
                       PDF="Single",
                       RZ="r",
                       PDFk=rp
                       )
         },
         "Binary"={
           world<-makeWorld(On=TRUE,
                       PDF="Single",
                       RZ="r",
                       PDFk=rp,
                       pRplus=0.5
                       )
         },
         "Double"={
           world<-makeWorld(On=TRUE,
                       PDF="Double",
                       RZ="r",
                       PDFk=rp
                       )
         },
         "Gaussian"={
           world<-makeWorld(On=TRUE,
                       PDF="Gauss",
                       RZ="z",
                       PDFk=atanh(rp),
                       pRplus=0.5
                       )
         },
         "Psych"={
           world<-makeWorld(On=TRUE,
                       PDF="Exp",
                       RZ="z",
                       PDFk=atanh(rp),
                       pRplus=0.26
                       )
         },
         "Psych50"={
           world<-makeWorld(On=TRUE,
                       PDF="Exp",
                       RZ="z",
                       PDFk=atanh(rp),
                       pRplus=0.5
                       )
         },
         "PsychF"={
           world<-makeWorld(On=TRUE,
                       PDF="Exp",
                       RZ="z",
                       PDFk=atanh(rp)
                       )
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
