
doMultipleTheory<-function(showType,logScale,hypothesis=braw.def$hypothesis,design=braw.def$design,i=1) {
  
  effect<-hypothesis$effect
  
  effectTheory<-effect
  if (!effectTheory$world$worldOn) {
    effectTheory$world$worldOn<-TRUE
    effectTheory$world$populationPDF<-"Single"
    effectTheory$world$populationRZ<-"r"
    switch(i,
           effectTheory$world$populationPDFk<-effect$rIV,
           effectTheory$world$populationPDFk<-effect$rIV2,
           effectTheory$world$populationPDFk<-effect$rIVIV2DV
    )
    effectTheory$world$populationNullp<-0
  }
  
  xdsig<-NULL
  xd<-NULL
  
  if (is.element(showType,c("p","e1p","e2p","po"))) {
    npt<-201
    if (logScale) {
      yv<-seq(0,ylim[1],length.out=npt)
      yvUse<-10^yv
    }else{
      yv<-seq(1,0,length.out=npt)
      yvUse<-yv
    }
    oldEffect<-effectTheory
    if (showType=="e1p") effectTheory$world$populationNullp<-1
    if (showType=="e2p") effectTheory$world$populationNullp<-0
    xd<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
    xdsig<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
    effectTheory<-oldEffect
  }
  
  if (is.element(showType,c("rs","ro","ci1","ci2","e1r","e2r"))) {
    npt<-101
    if (showType=="e1r") effectTheory$world$populationNullp<-1
    if (showType=="e2r") effectTheory$world$populationNullp<-0
    if (braw.env$RZ=="z") {
      zvals<-seq(-1,1,length.out=npt*2)*braw.env$z_range*2
      rvals<-tanh(zvals)
      # rvals<-seq(-1,1,length.out=npt)*0.99
      xd<-fullRSamplingDist(rvals,effectTheory$world,design,"rs",logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
      xdsig<-fullRSamplingDist(rvals,effectTheory$world,design,"rs",logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
      xd<-rdens2zdens(xd,rvals)
      xdsig<-rdens2zdens(xdsig,rvals)
      yv<-atanh(rvals)
      use<-abs(zvals)<=braw.env$z_range
      yv<-yv[use]
      xd<-xd[use]
      xdsig<-xdsig[use]
    } else {
      rvals<-seq(-1,1,length.out=npt)*0.99
      xd<-fullRSamplingDist(rvals,effectTheory$world,design,"rs",logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
      xdsig<-fullRSamplingDist(rvals,effectTheory$world,design,"rs",logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
      yv<-rvals
    }
  }
  
  npt<-101
  switch(showType,
         "rp"={
           if (braw.env$RZ=="z") {
             yv<-seq(-1,1,length.out=npt)*braw.env$z_range
             xd<-rPopulationDist(tanh(yv),effectTheory$world)
             xd<-rdens2zdens(xd,tanh(yv))
           } else {
             yv<-seq(-1,1,length.out=npt)*0.99
             xd<-rPopulationDist(yv,effectTheory$world)
           }
         },
         "n"={
           ndist<-getNDist(analysis$design,effectTheory$world,logScale=logScale,sigOnly=TRUE)
           yv<-ndist$nvals
           xd<-ndist$ndens
           xdsig<-ndist$ndensSig
         },
         "ws"={
           yv<-seq(braw.env$alphaSig*1.01,1/1.01,length.out=npt)
           xd<-fullRSamplingDist(yv,effectTheory$world,design,"ws",logScale=logScale,sigOnly=sigOnly)
         },
         "log(lrs)"={
           yv<-seq(0,braw.env$lrRange,length.out=npt)
           xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrs)",logScale=logScale,sigOnly=sigOnly)
         },
         "log(lrd)"={
           yv<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
           xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
         },
         "e1d"={
           yv<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
           xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
         },
         "e2d"={
           yv<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
           xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
         },
         "nw"={
           if (logScale) {
             yv<-seq(log10(5),log10(braw.env$max_nw),length.out=npt)
             yvUse<-10^yv
           }else{
             yv<-5+seq(0,braw.env$max_nw,length.out=npt)
             yvUse<-yv
           }
           xd<-fullRSamplingDist(yvUse,effectTheory$world,design,"nw",logScale=logScale,sigOnly=sigOnly)
           xd<-abs(xd)
         },
         "wp"={
           yv<-seq(braw.env$alphaSig,1/1.01,length.out=npt)
           xd<-fullRSamplingDist(yv,effectTheory$world,design,"wp",logScale=logScale,sigOnly=sigOnly)
         },
         { } # do nothing
  )
  
  return(list(yv=yv,
              xd=xd,
              xdsig=xdsig)
  )
}


