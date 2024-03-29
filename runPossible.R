
runPossible <- function(possible=makePossible()){
  
  npoints=201
  wDensMethod=2
  
  design<-possible$design
  n<-design$sN

  # note that we do everything in z and then, if required transform to r at the end
  switch (braw.env$RZ,
          "r" ={
            rs<-seq(-1,1,length=npoints)*braw.env$r_range
            rp<-seq(-1,1,length=npoints)*braw.env$r_range
          },
          "z" ={
            rs<-tanh(seq(-1,1,length=npoints)*braw.env$z_range)
            rp<-tanh(seq(-1,1,length=npoints)*braw.env$z_range)
          }
  )

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
                                    populationPDFk=effect$rIV,
                                    populationRZ="r",
                                    populationNullp=0
         )},
         "world"={source<-possible$world},
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
         "world"={ prior<-possible$world },
         "prior"={ prior<-possible$prior }
  )
  if (possible$type=="Populations") source<-prior
  
  priorPopDens_r<-rPopulationDist(rp,prior)
  priorPopDens_r<-priorPopDens_r/max(priorPopDens_r)
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
  sourceSampDens_r_plus<-sD$densPlus
  sourceSampDens_r_null<-sD$densNull
  if (is.element(source$populationPDF,c("Single","Double")) && source$populationNullp>0) {
    sourceRVals<-c(sourceRVals,0)
    sourceSampDens_r_plus<-rbind(sourceSampDens_r_plus,sourceSampDens_r_null)
  }
  
  pD<-fullRSamplingDist(rp,prior,design,separate=TRUE)
  priorRVals<-pD$vals
  priorSampDens_r<-pD$dens
  priorSampDens_r_plus<-pD$densPlus
  priorSampDens_r_null<-pD$densNull

  if (possible$possibleCorrection) {
    nout<-ceil(possible$possibleSimSlice*sqrt(design$sN-3))*20+1
    correction<-seq(-1,1,length.out=nout)*possible$possibleSimSlice
  }  else {
    correction<-0
  }
  
  # likelihood function for each sample (there's usually only 1)
  sampleSampDens_r<-1
  sampleLikelihood_r<-c()
  for (ei in 1:length(sRho)){
    rDens<-0
    for (ci in 1:length(correction)) {
      local_r<-tanh(atanh(sRho[ei])+correction[ci])
      if (design$sNRand) {
        d<-0
        for (ni in seq(minN,maxRandN*design$sN,length.out=nNpoints)) {
          # for (ni in 5+seq(0,maxRandN,1/n[ei])*n[ei]) {
          g<-dgamma(ni-minN,shape=design$sNRandK,scale=(design$sN-minN)/design$sNRandK)
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
  # times the a-priori distribution
  sampleSampDens_r<-sampleSampDens_r*priorPopDens_r_full
    for (ei in 1:length(sRho)){
      sampleLikelihood_r[ei,]<-sampleLikelihood_r[ei,]*priorPopDens_r_full
    }
  
  # convert from z to r
  # rs<-zs
  # rp<-zp
  # sourcePopDens_r<-sourcePopDens_z
  # sourceSampDens_r<-sourceSampDens_z
  # sourceSampDens_r_plus<-sourceSampDens_z_plus
  # sourceSampDens_r_null<-sourceSampDens_z_null
  # 
  # priorPopDens_r<-priorPopDens_z_show
  # priorSampDens_r<-priorSampDens_z
  # priorLikelihood_r<-priorLikelihood_z
  # priorSampDens_r_plus<-priorSampDens_z_plus
  # priorSampDens_r_null<-priorSampDens_z_null
  # if (braw.env$RZ=="r") {
  #   sourceRVals<-tanh(sourceRVals)
  #   sRho<-tanh(sRho)
  #   rp<-tanh(zp)
  #   rs<-tanh(zs)
  #   for (ei in 1:length(sRho)){
  #     priorLikelihood_r[ei,]<-zdens2rdens(priorLikelihood_z[ei,],rp)
  #   }
  #   for (ei in 1:length(sourceRVals)){
  #     sourceSampDens_r[ei,]<-zdens2rdens(sourceSampDens_z[ei,],rs)
  #   }
  #   sourceSampDens_r_plus<-zdens2rdens(sourceSampDens_z_plus,rs)
  #   sourceSampDens_r_null<-zdens2rdens(sourceSampDens_z_null,rs)
  #   priorSampDens_r<-zdens2rdens(priorSampDens_z,rp)
  #   priorPopDens_r<-zdens2rdens(priorPopDens_z_show,rp)
  #   priorSampDens_r_plus<-zdens2rdens(priorSampDens_z_plus,rs)
  #   priorSampDens_r_null<-zdens2rdens(priorSampDens_z_null,rs)
  #   sourcePopDens_r<-zdens2rdens(sourcePopDens_z,rs)
  # }
  # if (any(!is.na(priorLikelihood_r))) {
  #   dr_gain<-max(priorLikelihood_r,na.rm=TRUE)
  #   priorLikelihood_r<-priorLikelihood_r/dr_gain
  # }

  # power calculations
  # if (design$Replication$ReplicationOn && design$Replication$ReplPowerOn) {
  #   if (braw.env$RZ=="r") {
  #     nUse<-rw2n(sRho[1],design$Replication$ReplPowerOn)
  #   } else {
  #     nUse<-rw2n(tanh(sRho[1]),design$Replication$ReplPowerOn)
  #   }
  # }
  # else {
  #   nUse<-design$sN
  # }

  dr_gain<-max(sourceSampDens_r,na.rm=TRUE)
  sourceSampDens_r<-sourceSampDens_r/dr_gain
  
  dr_gain<-max(sourceSampDens_r_plus,na.rm=TRUE)
  sourceSampDens_r_null<-sourceSampDens_r_null/dr_gain
  sourceSampDens_r_plus<-sourceSampDens_r_plus/dr_gain
  
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
  
    switch (possible$type,
          "Samples"={
            possibleResult<-list(possible=possible,
                                   sourceRVals=sourceRVals,
                                   sRho=sRho,
                                   source=source,prior=prior,
                                   Theory=list(
                                     rs=rs,sourceSampDens_r=sourceSampDens_r,sourceSampDens_r_plus=sourceSampDens_r_plus,sourceSampDens_r_null=sourceSampDens_r_null,
                                     rp=rp,priorSampDens_r=sourceSampDens_r,sampleLikelihood_r=sampleLikelihood_r,priorPopDens_r=priorPopDens_r,sourcePopDens_r=sourcePopDens_r
                                   )
            )
          },
          "Populations"={
            possibleResult<-list(possible=possible,
                                   sourceRVals=sourceRVals,
                                   sRho=sRho,
                                   source=source,prior=prior,
                                   Theory=list(
                                     rs=rs,sourceSampDens_r=sourceSampDens_r,sourceSampDens_r_plus=sourceSampDens_r_plus,sourceSampDens_r_null=sourceSampDens_r_null,
                                     rp=rp,priorSampDens_r=priorSampDens_r,sampleLikelihood_r=sampleLikelihood_r,priorPopDens_r=priorPopDens_r,sourcePopDens_r=sourcePopDens_r,priorSampDens_r_null=priorSampDens_r_null,priorSampDens_r_plus=priorSampDens_r_plus
                                   )
            )
          }
  )
}
