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
makePossible<-function(targetSample=NULL,UseSource="world",
                       targetPopulation=NULL,UsePrior="none",prior=getWorld("Psych"),
                       sigOnly=FALSE,sigOnlyCompensate=FALSE,
                       axisType=braw.env$RZ,
                       sims=braw.res$multiple$result,
                       hypothesis=braw.def$hypothesis,design=braw.def$design,
                       simSlice=0.1,correction=TRUE,HQ=FALSE
) {
  if (is.numeric(targetSample)) {
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
  if (is.na(targetSample)) {
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

#' @returns possibleResult object
#' @seealso showPossible() 
#' @examples
#' doPossible<-function(possible=makePossible(),possibleResult=NULL)
#' @export
doPossible <- function(possible=NULL,possibleResult=NULL){
  
  if (is.null(possible)) possible<-makePossible()
  oldRZ<-braw.env$RZ
  braw.env$RZ<-possible$axisType
  on.exit(setBrawEnv("RZ",oldRZ))

  npoints=201

  design<-possible$design
  hypothesis<-possible$hypothesis
  world<-hypothesis$effect$world
  n<-design$sN

  # get the sample effect size of interest and its corresponding sample size
  sRho<-possible$targetSample
  sRhoN<-possible$targetSampleN
  pRho<-possible$targetPopulation
  
  # note that we do everything in r and then, if required transform to z at the end
  switch(braw.env$RZ,
         "r"={
           rs<-seq(-1,1,length=npoints)*braw.env$r_range
           rp<-seq(-1,1,length=npoints)*braw.env$r_range
         },
         "z"={
           rs<-tanh(seq(-1,1,length=npoints)*braw.env$z_range)
           rp<-tanh(seq(-1,1,length=npoints)*braw.env$z_range)
           if (!is.null(sRho)) sRho<-tanh(sRho)
           if (!is.null(pRho)) pRho<-tanh(pRho)
         })

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
  sD<-fullRSamplingDist(rs,source,design,separate=TRUE,
                        sigOnly=possible$sigOnly,sigOnlyCompensate=possible$sigOnlyCompensate,
                        HQ=possible$HQ)
  sourceRVals<-sD$vals
  sourceSampDens_r_total<-sD$dens
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
  pD<-fullRSamplingDist(rp,prior,design,separate=TRUE,HQ=TRUE)
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
  if (length(sRhoN)<length(sRho)) sRhoN<-rep(sRhoN,length(sRho))
  sampleLikelihoodTotal_r<-1
  sampleLikelihood_r<-c()
  if (length(sRho)>0 && !any(is.null(sRho)) && !any(is.na(sRho))) {
    for (ei in 1:length(sRho)) {
      s1<-c()
      for (i in 1:length(rp)) {
        rDens<-0
        for (ci in 1:length(correction)) {
        dg<-0
        if (design$sNRand) {
          d<-0
          for (ni in seq(braw.env$minN,braw.env$maxRandN*design$sN,length.out=braw.env$nNpoints)) {
            g<-nDistrDens(ni,design)
            dr<-rSamplingDistr(rs,rp[i],ni)*g
            dg1<-sum(dr)
            if (possible$sigOnly>0) {
              rcrit<-p2r(braw.env$alphaSig,ni,1)
              dr[abs(rs)<rcrit]<-dr[abs(rs)<rcrit]*(1-possible$sigOnly)
              if (possible$sigOnlyCompensate)
              dr<-dr/sum(dr)*dg1
            }
            dg<-dg+dg1
            d<-d+dr
          }
        } else {
          dr<-rSamplingDistr(rs,rp[i],sRhoN[ei])
          dg1<-sum(dr)
          if (possible$sigOnly>0) {
            rcrit<-p2r(braw.env$alphaSig,sRhoN[ei],1)
            dr[abs(rs)<rcrit]<-dr[abs(rs)<rcrit]*(1-possible$sigOnly)
            if (possible$sigOnlyCompensate)
              dr<-dr/sum(dr)*dg1
          }
          dg<-dg+dg1
          d<-dr
        }
        d<-d/dg
        rDens<-rDens+d
      }
        useDens<-approx(rs,rDens/length(correction),tanh(atanh(sRho[ei])))$y
        s1<-cbind(s1,useDens)
      }
      sampleLikelihood_r<-rbind(sampleLikelihood_r,s1)
      sampleLikelihoodTotal_r<-sampleLikelihoodTotal_r*s1
    }
    sampleLikelihood_r_show<-sampleLikelihood_r
    
    # times the a-priori distribution
    sampleLikelihoodTotal_r<-sampleLikelihoodTotal_r*priorPopDens_r_full

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
    sampleLikelihood_r_show<-c()
  }
  
  possibleResult<-list(possible=possible,
                       sourceRVals=sourceRVals,
                       sRho=sRho,
                       pRho=pRho,
                       source=source,prior=prior,
                       Theory=list(
                         rs=rs,sourceSampDens_r_total=sourceSampDens_r_total,
                               sourceSampDens_r_plus=sourceSampDens_r_plus,sourceSampDens_r_null=sourceSampDens_r_null,
                         rp=rp,priorSampDens_r=sourceSampDens_r_total,
                               sampleLikelihood_r=sampleLikelihood_r,sampleLikelihood_r_show=sampleLikelihood_r_show,
                               sampleLikelihoodTotal_r=sampleLikelihoodTotal_r,
                               priorPopDens_r=priorPopDens_r,sourcePopDens_r=sourcePopDens_r,
                               priorSampDens_r_null=priorSampDens_r_null,priorSampDens_r_plus=priorSampDens_r_plus
                       ),
                       Sims=list(
                         r=possible$sims$rIV,
                         rp=possible$sims$rpIV,
                         n<-possible$sims$nval
                       ),
                       mle=densityFunctionStats(sampleLikelihoodTotal_r,rp)$peak
  )
  
  setBrawRes("possibleResult",possibleResult)
  return(possibleResult)
}
