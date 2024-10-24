##################################################################################    
# LIKELIHOOD

#' likelihood theory
#' 
#' @param typePossible "Samples","Populations"
#' @returns possibleResult object
#' @seealso showPossible() 
#' @examples
#' makePossible<-function(typePossible="Samples",
#' possibleResult=NULL,
#' UseSource="world",targetSample=0.3,
#' UsePrior="none",prior=getWorld("Psych"),targetPopulation=0.3,
#' hypothesis=makeHypothesis(),design=makeDesign(),
#' simSlice=0.1,correction=TRUE)
#' @export
makePossible<-function(targetSample=braw.res$result,UseSource="world",
                       targetPopulation=NULL,UsePrior="none",prior=getWorld("Psych"),
                       sims=braw.res$expected$result,
                       hypothesis=braw.def$hypothesis,design=braw.def$design,
                       simSlice=0.1,correction=TRUE
) {
  if (is.null(targetSample)) {
    if (is.null(braw.res$result)) {
      targetSample<-0.3
    } else {
      targetSample<-braw.res$result
    }
  }
  if (!is.numeric(targetSample)) {
    result<-targetSample
    targetSample<-result$rIV
    targetPopulation<-result$rpIV
    hypothesis=result$hypothesis
    design=result$design
    design$sN<-result$nval
  }
  if (is.null(sims)) {
      sims<-braw.res$expected$result
  }
  if (hypothesis$effect$world$worldOn==FALSE) {
    hypothesis$effect$world$populationPDF<-"Single"
    hypothesis$effect$world$populationRZ<-"r"
    hypothesis$effect$world$populationPDFk<-hypothesis$effect$rIV
    hypothesis$effect$world$populationNullp<-0
  }
  
  possible<-
  list(targetSample=targetSample,
       UseSource=UseSource,
       targetPopulation=targetPopulation,
       UsePrior=UsePrior,
       prior=prior,
       hypothesis=hypothesis,
       design=design,
       showTheory=TRUE,
       sims=sims,
       simSlice=simSlice,correction=correction
  )
  
  return(possible)
}


doPossible <- function(possible=NULL,possibleResult=NULL){
  
  if (is.null(possible)) possible<-makePossible()
  npoints=201

  design<-possible$design
  hypothesis<-possible$hypothesis
  world<-hypothesis$effect$world
  n<-design$sN
  
  # note that we do everything in r and then, if required transform to z at the end
  rs<-seq(-1,1,length=npoints)*braw.env$r_range
  rp<-seq(-1,1,length=npoints)*braw.env$r_range
  if (braw.env$RZ=="z") {
            rs<-tanh(seq(-1,1,length=npoints)*braw.env$z_range)
            rp<-tanh(seq(-1,1,length=npoints)*braw.env$z_range)
          }

  # get the sample effect size of interest and its corresponding sample size
  sRho<-possible$targetSample
  if (braw.env$RZ=="z") sRho<-tanh(sRho)
  
  # get the source population distribution
  switch(possible$UseSource,
         "null"={source<-list(worldOn=FALSE,
                              populationPDF="Single",
                              populationPDFk=0,
                              populationRZ="r",
                              populationNullp=0
         )},
         "hypothesis"={source<-list(worldOn=FALSE,
                                    populationPDF="Single",
                                    populationPDFk=hypothesis$effect$rIV,
                                    populationRZ="r",
                                    populationNullp=0.5
         )},
         "world"={source<-world},
         "prior"={source<-possible$prior}
  )
  sourcePopDens_r<-rPopulationDist(rp,source)
  sourcePopDens_r<-sourcePopDens_r/max(sourcePopDens_r)
  # we add in the nulls for display, but only when displaying them makes sense
  if (source$populationPDF=="Single" || source$populationPDF=="Double") {
    sourcePopDens_r<-sourcePopDens_r*(1-source$populationNullp)
    sourcePopDens_r[rp==0]<-sourcePopDens_r[rp==0]+source$populationNullp
  }
  
  pRho<-possible$targetPopulation
  if (braw.env$RZ=="z") pRho<-tanh(pRho)
  # get the prior population distribution
  switch(possible$UsePrior,
         "none"={ prior<-list(worldOn=TRUE,
                              populationPDF="Uniform",
                              populationPDFk=1,
                              populationRZ="r",
                              populationNullp=0.0) },
         "hypothesis"={prior<-list(worldOn=FALSE,
                                    populationPDF="Single",
                                    populationPDFk=hypothesis$effect$rIV,
                                    populationRZ="r",
                                    populationNullp=0.5) },
         "world"={ prior<-world },
         "prior"={ prior<-possible$prior }
  )
  # if (possible$type=="Populations") source<-prior
  
  priorPopDens_r<-rPopulationDist(rp,prior)
  priorPopDens_r<-priorPopDens_r/mean(priorPopDens_r)/2
  if (max(priorPopDens_r)>0.9) priorPopDens_r<-priorPopDens_r/max(priorPopDens_r)*0.9
  priorPopDens_r_full<-priorPopDens_r*(1-prior$populationNullp)
  priorPopDens_r_full[rp==0]<-priorPopDens_r_full[rp==0]+prior$populationNullp
  if (prior$populationPDF=="Single" || prior$populationPDF=="Double") {
    priorPopDens_r_show<-priorPopDens_r_full/max(priorPopDens_r_full)
  } else {
    priorPopDens_r_show<-priorPopDens_r/max(priorPopDens_r)
  }
  
  # enumerate the source populations
  sD<-fullRSamplingDist(rs,source,design,separate=TRUE)
  sourceRVals<-sD$vals
  sourceSampDens_r<-sD$dens
  sourceSampDens_r_plus<-rbind(sD$densPlus)
  sourceSampDens_r_null<-sD$densNull
  if (is.element(source$populationPDF,c("Single","Double")) && source$populationNullp>0) {
    sourceRVals<-c(sourceRVals,0)
    sourceSampDens_r_plus<-rbind(sourceSampDens_r_plus,sourceSampDens_r_null)
  }
  dr_gain<-max(sourceSampDens_r_plus,na.rm=TRUE)
  sourceSampDens_r_null<-sourceSampDens_r_null/dr_gain
  sourceSampDens_r_plus<-sourceSampDens_r_plus/dr_gain
  
  # enumerate the prior populations
  pD<-fullRSamplingDist(rp,prior,design,separate=TRUE)
  priorRVals<-pD$vals
  priorSampDens_r<-pD$dens
  priorSampDens_r_plus<-pD$densPlus
  priorSampDens_r_null<-pD$densNull
  
  if (possible$correction) {
    nout<-ceil(possible$simSlice*sqrt(design$sN-3))*20+1
    correction<-seq(-1,1,length.out=nout)*possible$simSlice
  }  else {
    correction<-0
  }
  
  # likelihood function for each sample (there's usually only 1)
  sampleSampDens_r<-1
  sampleLikelihood_r<-c()
  if (!is.null(sRho) && !is.na(sRho)) {
    for (ei in 1:length(sRho)){
      rDens<-0
      for (ci in 1:length(correction)) {
        local_r<-tanh(atanh(sRho[ei])+correction[ci])
        if (design$sNRand) {
          d<-0
          for (ni in seq(braw.env$minN,braw.env$maxRandN*design$sN,length.out=braw.env$nNpoints)) {
            g<-dgamma(ni-braw.env$minN,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
            d<-d+rSamplingDistr(rp,local_r,ni)*g
          }
          d<-d/sum(d)
          rDens<-rDens+d
        } else {
          rDens<-rDens+rSamplingDistr(rp,local_r,n[ei])
        }
      }
      sampleLikelihood_r<-rbind(sampleLikelihood_r,rDens/length(correction))
      sampleSampDens_r <- sampleSampDens_r * rDens/length(correction)
    }
    sampleLikelihood_r_show<-sampleLikelihood_r
    
    # times the a-priori distribution
    sampleSampDens_r<-sampleSampDens_r*priorPopDens_r_full
    for (ei in 1:length(sRho)){
      sampleLikelihood_r[ei,]<-sampleLikelihood_r[ei,]*priorPopDens_r_full
    }
    
    for (ei in 1:length(sRho)){
      sampleLikelihood_r_show[ei,]<-sampleLikelihood_r_show[ei,]*priorPopDens_r
    }
    
    # dr_gain<-max(sourceSampDens_r,na.rm=TRUE)
    # sourceSampDens_r<-sourceSampDens_r/dr_gain
    
    if (any(!is.na(priorSampDens_r))) {
      dr_gain<-max(priorSampDens_r,na.rm=TRUE)
      priorSampDens_r<-priorSampDens_r/dr_gain
    }
    
    if (prior$worldOn && prior$populationNullp>0) {
      sampleLikelihood_r<-sampleLikelihood_r*(1-prior$populationNullp)
      priorPopDens_r<-priorPopDens_r*(1-prior$populationNullp)
      sourcePopDens_r<-sourcePopDens_r*(1-source$populationNullp)
      for (i in 1:length(sRho)) {
        sampleLikelihood_r<-sampleLikelihood_r*dnorm(atanh(sRho[i]),0,1/sqrt(n[i]-3))
      }
      priorSampDens_r_plus<-priorSampDens_r_plus/sum(priorSampDens_r_plus)*(1-prior$populationNullp)
      priorSampDens_r_null<-priorSampDens_r_null/sum(priorSampDens_r_null)*(prior$populationNullp)
    }
    sampleLikelihood_r<-sampleLikelihood_r/max(sampleLikelihood_r,na.rm=TRUE)
  } else {
    sampleLikelihood_r<-c()
  }
  
  possibleResult<-list(possible=possible,
                       sourceRVals=sourceRVals,
                       sRho=sRho,
                       pRho=pRho,
                       source=source,prior=prior,
                       Theory=list(
                         rs=rs,sourceSampDens_r=sourceSampDens_r,sourceSampDens_r_plus=sourceSampDens_r_plus,sourceSampDens_r_null=sourceSampDens_r_null,
                         rp=rp,priorSampDens_r=sourceSampDens_r,sampleLikelihood_r=sampleLikelihood_r,sampleLikelihood_r_show=sampleLikelihood_r_show,priorPopDens_r=priorPopDens_r,sourcePopDens_r=sourcePopDens_r,priorSampDens_r_null=priorSampDens_r_null,priorSampDens_r_plus=priorSampDens_r_plus
                       ),
                       Sims=list(
                         r=possible$sims$rIV,
                         rp=possible$sims$rpIV,
                         n<-possible$sims$nval
                       )
  )
  
  setBrawRes("possibleResult",possibleResult)
  return(possibleResult)
}
