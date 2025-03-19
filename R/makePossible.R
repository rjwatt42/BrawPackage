##################################################################################    
# LIKELIHOOD

#' likelihood theory
#' 
#' @param typePossible "Samples","Populations"
#' @returns possible object
#' @seealso showPossible() 
#' @examples
#' makePossible<-function(targetSample=NULL,UseSource="world",
#' targetPopulation=NULL,UsePrior="none",prior=getWorld("Psych"),
#' sims=braw.res$multiple$result,sigOnly=FALSE,sigOnlyCompensate=FALSE,
#' typePossible="Samples",
#' hypothesis=makeHypothesis(),design=makeDesign(),
#' simSlice=0.1,correction=TRUE)
#' @export
makePossible<-function(targetSample=NULL,targetSampleN=NULL,UseSource="world",
                       targetPopulation=NULL,UsePrior="none",prior=getWorld("Psych"),
                       sigOnly=FALSE,sigOnlyCompensate=FALSE,
                       axisType=braw.env$RZ,
                       sims=braw.res$multiple$result,
                       hypothesis=braw.def$hypothesis,design=braw.def$design,
                       simSlice=0.1,correction=TRUE,HQ=FALSE
) {
  if (is.numeric(targetSample) && is.null(targetSampleN)) {
    targetSampleN<-design$sN
  }
  if (is.null(targetSample)) {
    if (is.null(braw.res$result)) {
      targetSample<-0.3
      targetSampleN<-design$sN
    } else {
      targetSample<-braw.res$result
    }
  }
  
  if (is.list(targetSample)) {
    result<-targetSample
    if (is.data.frame(targetSample)) {
      targetSample<-result$rIV
      targetSampleN<-result$nval
    } else {
      targetSample<-result$rIV
      targetSampleN<-result$nval
      targetPopulation<-result$rpIV
      hypothesis=result$hypothesis
      design=result$design
      design$sN<-result$nval
    }
  }
  if (length(targetSample)==1 && is.na(targetSample)) {
    targetSample<-NULL
    targetSampleN<-design$sN
  }
  
  if (sigOnly>0) {
    rcrit<-p2r(braw.env$alphaSig,targetSampleN)
    if (!is.null(targetSample)) targetSample<-targetSample[abs(targetSample)>=rcrit]
  }
  if (is.null(targetSampleN)) {
    if (design$sNRand) {
      n<-nDistrRand(length(targetSample),design)
      while (any(n>100000)) {n<-nDistrRand(length(targetSample),design)}
      targetSampleN<-n
    } else targetSampleN<-rep(design$sN,length(targetSample))
  }
    
  if (is.null(sims)) {
      sims<-braw.res$multiple$result
  }
  if (hypothesis$effect$world$worldOn==FALSE) {
    hypothesis$effect$world$populationPDF<-"Single"
    hypothesis$effect$world$populationRZ<-"r"
    hypothesis$effect$world$populationPDFk<-hypothesis$effect$rIV
    hypothesis$effect$world$populationNullp<-0
  }
  
  possible<-
  list(targetSample=targetSample,
       targetSampleN=targetSampleN,
       sigOnly=sigOnly,
       sigOnlyCompensate=sigOnlyCompensate,
       UseSource=UseSource,
       targetPopulation=targetPopulation,
       UsePrior=UsePrior,
       prior=prior,
       axisType=axisType,
       hypothesis=hypothesis,
       design=design,
       showTheory=TRUE,
       sims=sims,
       simSlice=simSlice,correction=correction,HQ=HQ
  )
  
  return(possible)
}
