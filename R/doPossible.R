##################################################################################    
# LIKELIHOOD

#' likelihood theory
#' 
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
         "null"={source<-list(On=FALSE,
                              PDF="Single",
                              PDFk=0,
                              RZ="r",
                              pRPlus=1
         )},
         "hypothesis"={source<-list(On=FALSE,
                                    PDF="Single",
                                    PDFk=hypothesis$effect$rIV,
                                    RZ="r",
                                    pRPlus=0.5
         )},
         "world"={source<-world},
         "prior"={source<-possible$prior}
  )
  sourcePopDens_r<-rPopulationDist(rp,source)
  sourcePopDens_r<-sourcePopDens_r/max(sourcePopDens_r)
  # we add in the nulls for display, but only when displaying them makes sense
  if (source$PDF=="Single" || source$PDF=="Double") {
    sourcePopDens_r<-sourcePopDens_r*(source$pRPlus)
    sourcePopDens_r[rp==0]<-sourcePopDens_r[rp==0]+(1-source$pRPlus)
  }
  
  # get the prior population distribution
  switch(possible$UsePrior,
         "none"={ prior<-list(On=TRUE,
                              PDF="Uniform",
                              PDFk=1,
                              RZ="r",
                              pRPlus=1,
                              PDFsample=FALSE) },
         "hypothesis"={prior<-list(On=FALSE,
                                    PDF="Single",
                                    PDFk=hypothesis$effect$rIV,
                                    RZ="r",
                                    pRPlus=0.5,
                                   PDFsample=FALSE) },
         "world"={ prior<-world },
         "prior"={ prior<-possible$prior }
  )
  # if (possible$type=="Populations") source<-prior
  
  priorPopDens_r<-rPopulationDist(rp,prior)
  priorPopDens_r<-priorPopDens_r/mean(priorPopDens_r)/2
  if (max(priorPopDens_r)>0.9) priorPopDens_r<-priorPopDens_r/max(priorPopDens_r)*0.9
  priorPopDens_r_full<-priorPopDens_r*(prior$pRPlus)
  priorPopDens_r_full[rp==0]<-priorPopDens_r_full[rp==0]+(1-prior$pRPlus)$pRPlus
  if (prior$PDF=="Single" || prior$PDF=="Double") {
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
  if (is.element(source$PDF,c("Single","Double")) && source$pRPlus<1) {
    sourceRVals<-c(sourceRVals,0)
    sourceSampDens_r_plus<-rbind(sourceSampDens_r_plus,sourceSampDens_r_null)
  }
  dr_gain<-sum(sourceSampDens_r_total[1:(length(sourceSampDens_r_total)-1)]*diff(rs),na.rm=TRUE)
  sourceSampDens_r_total<-sourceSampDens_r_total/dr_gain
  sourceSampDens_r_null<-sourceSampDens_r_null/dr_gain
  sourceSampDens_r_plus<-sourceSampDens_r_plus/dr_gain
  
  if (!is.null(sRho)) {
    if (!is.null(nrow(sourceSampDens_r_plus)))
         sourceSampDens_r_plus1<-colSums(sourceSampDens_r_plus)
    else sourceSampDens_r_plus1<-sourceSampDens_r_plus
    
    switch(braw.env$RZ,
           "r"={
             sRho_total<-approx(rs,sourceSampDens_r_total,sRho)$y
             sRho_plus<-approx(rs,sourceSampDens_r_plus1,sRho)$y
             sRho_null<-approx(rs,sourceSampDens_r_null,sRho)$y
           },
           "z"={
             sRho_total<-approx(atanh(rs),rdens2zdens(sourceSampDens_r_total,rs),sRho)$y
             sRho_plus<-approx(atanh(rs),rdens2zdens(sourceSampDens_r_plus1,rs),sRho)$y
             sRho_null<-approx(atanh(rs),rdens2zdens(sourceSampDens_r_null,rs),sRho)$y
           }
    )
  } else {
    sRho_total<-NA
    sRho_plus<-NA
    sRho_null<-NA
  }
  
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
    
    if (prior$On && prior$pRPlus<1) {
      sampleLikelihood_r<-sampleLikelihood_r*(prior$pRPlus)
      priorPopDens_r<-priorPopDens_r*(prior$pRPlus)
      sourcePopDens_r<-sourcePopDens_r*(source$pRPlus)
      for (i in 1:length(sRho)) {
        sampleLikelihood_r<-sampleLikelihood_r*dnorm(atanh(sRho[i]),0,1/sqrt(n[i]-3))
      }
      priorSampDens_r_plus<-priorSampDens_r_plus/sum(priorSampDens_r_plus)*(prior$pRPlus)
      priorSampDens_r_null<-priorSampDens_r_null/sum(priorSampDens_r_null)*(1-prior$pRPlus)
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
                               sRho_total=sRho_total,sRho_plus=sRho_plus,sRho_null=sRho_null,
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
