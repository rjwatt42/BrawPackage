theoryPlot<-function(g,theory,orientation,baseColour,theoryAlpha,xoff,lineOnly=FALSE) {
  theoryVals<-theory$theoryVals
  theoryDens_all<-theory$theoryDens_all
  theoryDens_sig<-theory$theoryDens_sig
  showAll<-all(theoryDens_all==theoryDens_sig)
  if (showAll) theoryDens_all<-theoryDens_all*0
  
  switch(orientation,
         "horz"={
           ln<-length(theoryVals)
           theory_all<-data.frame(x=theoryVals[c(1,1:ln,ln)],y=c(0,theoryDens_all,0)+xoff)
         },
         "vert"={
           theory_all<-data.frame(y=c(theoryVals,rev(theoryVals)),x=c(theoryDens_all,-rev(theoryDens_all))+xoff)
         })
  if (is.null(theoryDens_sig)) baseColour<-"white"
  if (!lineOnly)
    g<-addG(g,dataPolygon(data=theory_all,colour=NA,fill=baseColour,alpha=theoryAlpha))
  g<-addG(g,dataPath(data=theory_all,colour="#000000",linewidth=0.2))
  
  if (!is.null(theoryDens_sig)) {
    i2<-0
    while (i2<length(theoryDens_sig)) {
      i1<-i2+min(which(c(theoryDens_sig[(i2+1):length(theoryDens_sig)],1)>0))
      i2<-(i1-1)+min(which(c(theoryDens_sig[i1:length(theoryDens_sig)],0)==0))
      use<-i1:(i2-1)
      switch(orientation,
             "horz"={
               ln<-length(theoryVals[use])
               theory_sig<-data.frame(x=theoryVals[use[c(1,1:ln,ln)]],y=c(0,theoryDens_sig[use],0)+xoff)
             },
             "vert"={
               theory_sig<-data.frame(y=c(theoryVals[use],rev(theoryVals[use])),x=c(theoryDens_sig[use],-rev(theoryDens_sig[use]))+xoff)
             })
      if (!lineOnly)
        g<-addG(g,dataPolygon(data=theory_sig,colour=NA,fill=braw.env$plotColours$infer_sigC,alpha=theoryAlpha))
      # if (showAll) 
      g<-addG(g,dataPath(data=theory_sig,colour="black",linewidth=0.1))
      # else
        # g<-addG(g,dataPath(data=theory_sig,colour="white",linewidth=0.1))
    }
  }
  
  # if (!showAll) 
  #   g<-addG(g,dataPath(data=theory_sig,colour="#000000",linewidth=0.2))
  
  return(g)
}
makeTheoryMultiple<-function(hypothesis,design,evidence,showType,whichEffect,logScale,ylim,labelNSig,labelSig,distGain) {
  effect<-hypothesis$effect
  
  effectTheory<-effect
  if (!effectTheory$world$On) {
    effectTheory$world$On<-TRUE
    effectTheory$world$PDF<-"Single"
    effectTheory$world$RZ<-"r"
    switch(whichEffect,
           "Model"=effectTheory$world$PDFk<-sqrt(effect$rIV^2+effect$rIV2^2+effect$rIVIV2DV^2+effect$rIV*effect$rIV2*effect$rIVIV2),
           "Main 1"=effectTheory$world$PDFk<-effect$rIV,
           "Main 2"=effectTheory$world$PDFk<-effect$rIV2,
           "Interaction"=effectTheory$world$PDFk<-effect$rIVIV2DV,
           "Covariation"=effectTheory$world$PDFk<-effect$rIVIV2
    )
    effectTheory$world$pRplus<-1
  }
  
  theoryDens_sig<-NULL
  theoryDens_all<-NULL
  histGain<-NA
  
  if (is.element(showType,c("p","e1p","e2p","po"))) {
    npt<-201
    if (logScale) {
      theoryVals<-seq(0,ylim[1],length.out=npt)
      yvUse<-10^theoryVals
    }else{
      theoryVals<-seq(1,0,length.out=npt)
      yvUse<-theoryVals
    }
    oldEffect<-effectTheory
    if (showType=="e1p") effectTheory$world$pRplus<-0
    if (showType=="e2p") effectTheory$world$pRplus<-1
    theoryDens_all<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=0,HQ=braw.env$showTheoryHQ)
    theoryDens_sig<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
    effectTheory<-oldEffect
  }
  
  if (is.element(showType,c("rs","rse","sig","ns","nonnulls","nulls","rss","re","ro","ci1","ci2","e1r","e2r","e1+","e2+","e1-","e2-"))) {
    npt<-braw.env$npoints
    if (is.element(showType,c("e1r","e1+","e1-"))) effectTheory$world$pRplus<-0
    if (is.element(showType,c("e2r","e2+","e2-"))) effectTheory$world$pRplus<-1
    if (showType=="re") rOff<-"re"
    else rOff<-"rs"
    switch(braw.env$RZ,
           "r"={
             if (!design$sNRand) {
               cr<-tanh(pn2z(braw.env$alphaSig,design$sN))
               cr<-tanh(qnorm(1-braw.env$alphaSig/2,0,1/sqrt(design$sN-3)))
               inc<-cr/ceiling(cr/(2/npt))
               rvals<-seq(inc,0.99,inc)
               rvals<-c(-rev(rvals),0,rvals)
             } else 
               rvals<-seq(-1,1,length.out=npt)*0.99
             switch(showType,
                    "sig"={
                      theoryDens_all<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
                      theoryDens_sig<-theoryDens_all
                    },
                    "ns"={
                      theoryDens_all<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=0,HQ=braw.env$showTheoryHQ)
                      theoryDens_sig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
                      theoryDens_all<-theoryDens_all-theoryDens_sig
                      theoryDens_sig<-theoryDens_sig*0
                    },
                    "nonnulls"={
                      ew<-effectTheory$world
                      ew$pRplus<-1
                      theoryDens_all<-fullRSamplingDist(rvals,ew,design,rOff,logScale=logScale,sigOnly=0,HQ=braw.env$showTheoryHQ)
                      theoryDens_sig<-fullRSamplingDist(rvals,ew,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
                    },
                    "nulls"={
                      ew<-effectTheory$world
                      ew$pRplus<-0
                      theoryDens_all<-fullRSamplingDist(rvals,ew,design,rOff,logScale=logScale,sigOnly=0,HQ=braw.env$showTheoryHQ)
                      theoryDens_sig<-fullRSamplingDist(rvals,ew,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
                    },
                    {
                      if (design$Replication$On) {
                        design$Replication$On<-FALSE
                        # original populations
                          pR<-getRList(effectTheory$world,addNulls=TRUE,HQ=braw.env$showTheoryHQ)
                          rSourceVals<-pR$pRho
                          rSourcePopdens<-pR$pRhogain
                          rSourcePopdens<-rSourcePopdens/sum(rSourcePopdens)
                          # we are only replicating significant results
                          rSourcePopdens<-rSourcePopdens*rn2w(rSourceVals,design$sN)
                        # original samples -
                        theoryDens_all<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=0,HQ=braw.env$showTheoryHQ)
                        theoryDens_sig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
                        # carry forward the ns results that don't get replicated
                        theoryDens_ns<-theoryDens_all-theoryDens_sig
                        # if we are forcing significant original, then there are none to carry forward
                        if (design$Replication$forceSigOriginal) theoryDens_ns<-theoryDens_ns*0
                        # # these are the ones we will replicate: the sig ones
                        theoryFullAll<-theoryFullSig<-0
                        # for each possible population effect size
                        for (i in 1:length(rSourceVals)) {
                          # make original sample & get replication sample size
                          samp1Dens<-fullRSamplingDist(rvals,makeWorld(TRUE,"Single","r",rSourceVals[i],pRplus = 1),
                                                       design=design,rOff,logScale=logScale,
                                                       sigOnly=1,HQ=braw.env$showTheoryHQ)
                          n2<-replicationNewN(rvals,design$sN,hypothesis,design,evidence)
                          # now get sampling distribution for this population and all sample sizes
                          theoryPartAll<-theoryPartSig<-0
                          nUse<-unique(n2)
                          for (i2 in 1:length(nUse)) {
                            rdens<-samp1Dens[n2==nUse[i2]]
                            rS<-rSamplingDistr(rvals,rSourceVals[i],nUse[i2])
                            rCrit<-wn2r(0.5,nUse[i2])
                            theoryPartAll<-theoryPartAll+rS*sum(rdens)
                            theoryPartSig<-theoryPartSig+rS*(abs(rvals)>rCrit)*sum(rdens)
                          }
                          theoryFullAll<-theoryFullAll+theoryPartAll/sum(theoryPartAll)*rSourcePopdens[i]
                          theoryFullSig<-theoryFullSig+theoryPartSig/sum(theoryPartAll)*rSourcePopdens[i]
                        }
                        theoryDens_all<-theoryFullAll+theoryDens_ns
                        theoryDens_sig<-theoryFullSig
                      } else {
                        theoryDens_all<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=evidence$sigOnly,HQ=braw.env$showTheoryHQ)
                        theoryDens_sig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
                      }
                    }
             )
             theoryVals<-rvals
             if (whichEffect=="Model") {
               theoryDens_all[theoryVals<0]<-0
               theoryDens_sig[theoryVals<0]<-0
             }
           },
           "z"={
             zvals<-seq(-1,1,length.out=npt*2)*braw.env$z_range*2
             rvals<-tanh(zvals)
             # rvals<-seq(-1,1,length.out=npt)*0.99
             theoryDens_all<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=0,HQ=braw.env$showTheoryHQ)
             theoryDens_sig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=1,HQ=braw.env$showTheoryHQ)
             theoryDens_all<-rdens2zdens(theoryDens_all,rvals)
             theoryDens_sig<-rdens2zdens(theoryDens_sig,rvals)
             theoryVals<-atanh(rvals)
             use<-abs(zvals)<=braw.env$z_range
             theoryVals<-theoryVals[use]
             theoryDens_all<-theoryDens_all[use]
             theoryDens_sig<-theoryDens_sig[use]
           }
    )
  }
  
  npt<-braw.env$npoints
  switch(showType,
         "rp"={
                    theoryVals<-seq(-1,1,length.out=npt)*0.99
                    theoryDens_all<-rPopulationDist(theoryVals,effectTheory$world)
                    theoryDens_sig<-theoryDens_all
                      ndist<-getNList(design,hypothesis$effect$world)
                      ns<-ndist$nvals
                      # nd<-nDistrDens(ns,design)
                      nd<-ndist$ndens
                      nd<-nd/sum(nd)
                    # we do each population separately
                    # if (!design$Replication$On) {
                      for (ri in 1:npt) {
                        w<-rn2w(theoryVals[ri],ns)
                        psig<-sum(nd*w)
                        theoryDens_sig[ri]<-theoryDens_all[ri]*psig
                      }
                    # } 
                  if (design$Replication$On) {
                      # for each of the possible sample rs
                      for (ri1 in 1:npt) {
                        w1<-rn2w(theoryVals[ri1],ns)*nd
                        w2<-0
                        for (ni2 in 1:length(ns)) {
                          rd<-rSamplingDistr(theoryVals,theoryVals[ri1],ns[ni2])
                          rd<-rd/sum(rd)
                          use<-which(rd>max(rd)/50)
                          nrep<-replicationNewN(theoryVals[use],ns[ni2],hypothesis,design)
                          if (design$Replication$Keep=="MetaAnalysis")
                            nrep<-nrep+ns[ni2]
                          w2<-w2+sum(rn2w(theoryVals[ri1],nrep)*rd[use])*nd[ni2]
                        }
                        if (design$Replication$Keep=="MetaAnalysis") psig<-sum(w2)
                        else psig<-sum(w1*w2)
                        if (design$Replication$forceSigOriginal)
                          theoryDens_sig[ri1]<-theoryDens_sig[ri1]*psig
                        else 
                          theoryDens_sig[ri1]<-theoryDens_all[ri1]*psig
                      }
                  }
                  if (braw.env$RZ=="z") {
                    theoryDens_all<-rdens2zdens(theoryDens_all,theoryVals)
                    theoryDens_sig<-rdens2zdens(theoryDens_sig,theoryVals)
                    theoryVals<-atanh(theoryVals)
                  }
         },
         "n"={
           ndist<-getNDist(design,effectTheory$world,logScale=logScale,sigOnly=1)
           if (logScale) {
             theoryVals<-log10(ndist$nvals)
           } else {
             theoryVals<-ndist$nvals
           }
           theoryDens_all<-ndist$ndens
           theoryDens_sig<-ndist$ndensSig
         },
         "ws"={
           dw<-0.01
           theoryVals<-seq(braw.env$alphaSig*(1+dw),1/(1+dw),length.out=npt)
           theoryDens_all<-fullRSamplingDist(theoryVals,effectTheory$world,design,"ws",logScale=logScale,sigOnly=evidence$sigOnly)
         },
         "log(lrs)"={
           theoryVals<-seq(0,braw.env$lrRange,length.out=npt)
           theoryDens_all<-fullRSamplingDist(theoryVals,effectTheory$world,design,"log(lrs)",logScale=logScale,sigOnly=evidence$sigOnly)
         },
         "log(lrd)"={
           theoryVals<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
           theoryDens_all<-fullRSamplingDist(theoryVals,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=evidence$sigOnly)
         },
         "e1d"={
           theoryVals<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
           theoryDens_all<-fullRSamplingDist(theoryVals,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=evidence$sigOnly)
         },
         "e2d"={
           theoryVals<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
           theoryDens_all<-fullRSamplingDist(theoryVals,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=evidence$sigOnly)
         },
         "nw"={
           if (logScale) {
             theoryVals<-seq(log10(5),log10(braw.env$max_nw),length.out=npt)
             yvUse<-10^theoryVals
           }else{
             theoryVals<-5+seq(0,braw.env$max_nw,length.out=npt)
             yvUse<-theoryVals
           }
           theoryDens_all<-fullRSamplingDist(yvUse,effectTheory$world,design,"nw",logScale=logScale,sigOnly=evidence$sigOnly)
           theoryDens_all<-abs(theoryDens_all)
         },
         "wp"={
           dw<-0.01
           theoryVals<-seq(braw.env$alphaSig*(1+dw),1/(1+dw),length.out=npt)
           theoryDens_all<-fullRSamplingDist(theoryVals,effectTheory$world,design,"wp",logScale=logScale,sigOnly=evidence$sigOnly)
         },
         "iv.mn"={
           var<-hypothesis$IV
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           theoryDens_all<-dnorm(theoryVals,var$mu,var$sd/sqrt(n))
         },
         "iv.sd"={
           var<-hypothesis$IV
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           yvuse<-theoryVals/var$sd
           # theoryDens_all<-exp(-n/2*yvuse^2+(n-2)*log(theoryVals))
           # theoryDens_all<-exp(-n/2*yvuse^2+n*log(theoryVals)-n*2/n*log(theoryVals))
           # theoryDens_all<-exp(n*(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals)))
           # theoryDens_all<-exp(n*(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals)))
           xd1<-exp(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals))
           xd1<-xd1/max(xd1)
           theoryDens_all<-xd1^n
           if (max(theoryDens_all)==0) theoryDens_all<-NULL # for n>1000 because of underflow
         },
         "iv.sk"={
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           sksd<-sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
           theoryDens_all<-dnorm(theoryVals,0,sksd)
         },
         "iv.kt"={
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           ktsd<-sqrt(24*n*(n-2)*(n-3)/(n+1)^2/(n+3)/(n+5))
           theoryDens_all<-dnorm(theoryVals,0,ktsd)
         },
         "dv.mn"={
           var<-hypothesis$DV
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           theoryDens_all<-dnorm(theoryVals,var$mu,var$sd/sqrt(n))
         },
         "dv.sd"={
           var<-hypothesis$DV
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           yvuse<-theoryVals/var$sd
           # theoryDens_all<-exp(-n/2*yvuse^2+(n-2)*log(theoryVals))
           # theoryDens_all<-exp(-n/2*yvuse^2+n*log(theoryVals)-n*2/n*log(theoryVals))
           # theoryDens_all<-exp(n*(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals)))
           # theoryDens_all<-exp(n*(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals)))
           xd1<-exp(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals))
           xd1<-xd1/max(xd1)
           theoryDens_all<-xd1^n
           if (max(theoryDens_all)==0) theoryDens_all<-NULL # for n>1000 because of underflow
         },
         "dv.sk"={
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           sksd<-sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
           theoryDens_all<-dnorm(theoryVals,0,sksd)
         },
         "dv.kt"={
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           ktsd<-sqrt(24*n*(n-2)*(n-3)/(n+1)^2/(n+3)/(n+5))
           theoryDens_all<-dnorm(theoryVals,0,ktsd)
         },
         "er.mn"={
           var<-hypothesis$DV
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           mnsd<-1/sqrt(n)*var$sd*sqrt(1-hypothesis$effect$rIV^2)
           theoryDens_all<-dnorm(theoryVals,0,mnsd)
         },
         "er.sd"={
           var<-hypothesis$DV
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           sdsd<-var$sd*sqrt(1-hypothesis$effect$rIV^2)
           yvuse<-theoryVals/sdsd
           # theoryDens_all<-exp(-n/2*yvuse^2+(n-2)*log(theoryVals))
           # theoryDens_all<-exp(-n/2*yvuse^2+n*log(theoryVals)-n*2/n*log(theoryVals))
           # theoryDens_all<-exp(n*(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals)))
           # theoryDens_all<-exp(n*(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals)))
           xd1<-exp(-1/2*yvuse^2+log(theoryVals)-2/n*log(theoryVals))
           xd1<-xd1/max(xd1)
           theoryDens_all<-xd1^n
           if (max(theoryDens_all)==0) theoryDens_all<-NULL # for n>1000 because of underflow
         },
         "er.sk"={
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           sksd<-sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
           theoryDens_all<-dnorm(theoryVals,0,sksd)
         },
         "er.kt"={
           n<-design$sN
           theoryVals<-seq(ylim[1],ylim[2],length.out=npt)
           ktsd<-sqrt(24*n*(n-2)*(n-3)/(n+1)^2/(n+3)/(n+5))
           theoryDens_all<-dnorm(theoryVals,0,ktsd)
         },
         { } # do nothing
  )
  if (is.element(showType,c("p","e1p","e2p","po"))) {
    if (!labelNSig) {
      theoryDens_all<-theoryDens_all-theoryDens_sig
      theoryDens_sig<-NA
    }
    if (!labelSig) theoryDens_all<-theoryDens_sig
  }
  
  if (!all(is.na(theoryDens_all))) {
    theoryDens_all[is.na(theoryDens_all)]<-0
    theoryGain<-1/max(theoryDens_all)*distGain
    if (is.infinite(theoryGain)) theoryGain<-0
    theoryDens_all<-theoryDens_all*theoryGain
    
    theoryDens_sig<-theoryDens_sig*theoryGain
    theoryDens_sig[is.na(theoryDens_sig)]<-0
    
  }
  
  return(list(theoryVals=theoryVals,theoryDens_all=theoryDens_all,theoryDens_sig=theoryDens_sig))
}


collectData<-function(analysis,whichEffect) {
  min_p<-1e-20
  use<-(!is.na(analysis$rIV))
  ns<-cbind(analysis$nval[use])
  df1<-cbind(analysis$df1[use])
  rp<-cbind(analysis$rpIV[use])
  ro<-cbind(analysis$roIV[use])
  po<-cbind(analysis$poIV[use])
  
  iv.mn<-cbind(analysis$iv.mn[use])
  iv.sd<-cbind(analysis$iv.sd[use])
  iv.sk<-cbind(analysis$iv.sk[use])
  iv.kt<-cbind(analysis$iv.kt[use])
  
  dv.mn<-cbind(analysis$dv.mn[use])
  dv.sd<-cbind(analysis$dv.sd[use])
  dv.sk<-cbind(analysis$dv.sk[use])
  dv.kt<-cbind(analysis$dv.kt[use])
  
  er.mn<-cbind(analysis$er.mn[use])
  er.sd<-cbind(analysis$er.sd[use])
  er.sk<-cbind(analysis$er.sk[use])
  er.kt<-cbind(analysis$er.kt[use])
  
  if (all(is.na(analysis$rIV2))){
    rs<-cbind(analysis$rIV[use])
    ps<-cbind(analysis$pIV[use])
  } else {
    switch (whichEffect,
            "Model"={
              rs<-rbind(cbind(analysis$rFull[use]))
              ps<-rbind(cbind(analysis$pFull[use]))
            },
            "Main 1"={
              column<-1
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "Main 2"={
              column<-2
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "Interaction"={
              column<-3
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "Covariation"={
              column<-4
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "All"={
              rs<-c()
              ps<-c()
              ysc=1/3
              xoff=c(0,0,0,2,2,2,4,4,4)
              for (jk in 1:ncol(analysis$r$direct)) {
                rs<-cbind(rs,analysis$r$direct[use,jk],analysis$r$unique[use,jk],analysis$r$total[use,jk])
                ps<-cbind(ps,analysis$p$direct[use,jk],analysis$p$unique[use,jk],analysis$p$total[use,jk])
              }
            },
            "coefficients"={
              rs<-rbind(analysis$r$coefficients[use])
              ps<-rbind(analysis$p$direct[use,])
            }
    )
  }
  ps[ps<min_p]<-min_p
  po[po<min_p]<-min_p
  # if (braw.env$truncate_p) {
  #   ps[ps<braw.env$min_p]<-braw.env$min_p
  #   po[po<braw.env$min_p]<-braw.env$min_p
  # }
  out<-list(rs=rs,ps=ps,ns=ns,df1=df1,rp=rp,ro=ro,po=po,
            iv.mn=iv.mn,iv.sd=iv.sd,iv.sk=iv.sk,iv.kt=iv.kt,
            dv.mn=dv.mn,dv.sd=dv.sd,dv.sk=dv.sk,dv.kt=dv.kt,
            er.mn=er.mn,er.sd=er.sd,er.sk=er.sk,er.kt=er.kt)
}

makeFiddle<-function(y,yd,orientation="horz"){
  
  if (min(y)==max(y)) y_vals<- c(-0.1,0,0.1)+y[1]
  else y_vals<-seq(min(y),max(y),length.out=501)
  
  yG<-(braw.env$plotArea[4]-braw.env$plotLimits$gap[4]-braw.env$plotLimits$gap[2])/diff(braw.env$plotLimits$ysc)
  rY<-function(y) y*yG
  
  xG<-(braw.env$plotArea[3]-braw.env$plotLimits$gap[3]-braw.env$plotLimits$gap[1])/diff(braw.env$plotLimits$xsc)
  rX<-function(x) x*xG
  
  dotSize<-min(4,braw.env$dotSize*(200/length(y))^2)
  dotSize<-0.5
  # dotSize<-braw.env$dS
  rr<-8*ceiling(dotSize/4*yG/25/diff(y_vals[c(1,2)]))
  rr<-min(rr,floor(length(y_vals)/2))
  rj<-0.075
  # rj<-braw.env$rJ

  dy<-diff(y_vals[c(1,1+rr*2)])
  y_filledp<-y_vals*0
  y_filledn<-y_vals*0
  x_pos<-y*0
  if (orientation=="horz") 
    for (i in 1:length(y)) {
      use<-which.min(abs(y[i]-y_vals))
      dx<-sqrt(rX(dy)^2-rX(y[i]-y_vals[use])^2)
      if (dx==0) dxr<-0
      else       dxr<-runif(1,-1,1)*rj*dy
      fill<-use+(-rr:rr)
      fill<-fill[fill>=1 & fill<=length(y_vals)]
      x_pos[i]<-y_filledp[use]+dxr
      y_filledp[fill]<-x_pos[i]+dx
    }
  else
    for (i in 1:length(y)) {
      use<-which.min(abs(y[i]-y_vals))
      dx<-sqrt(rX(dy)^2-rX(y[i]-y_vals[use])^2)
      dxr<-runif(1,-1,1)*rj*dy
      fill<-use+(-rr:rr)
      fill<-fill[fill>=1 & fill<=length(y_vals)]
      if (y_filledp[use]<y_filledn[use] || (y_filledp[use]==0 && y_filledn[use]==0 && runif(1)>0.5)) {
        x_pos[i]<-y_filledp[use]+dxr
        y_filledp[fill]<-x_pos[i]+dx
        if (x_pos[i]==0) y_filledn[fill]<-x_pos[i]+dx
      } else {
        x_pos[i]<- -(y_filledn[use]+dxr)
        y_filledn[fill]<- -x_pos[i]+dx
        if (x_pos[i]==0) y_filledp[fill]<- -x_pos[i]+dx
      }
    }
  # if (length(y)>=10) x_pos<-x_pos/max(abs(x_pos))
  if (length(y)>=10) x_pos<-x_pos/(sum(y_filledp)*diff(y_vals[1:2]))
  return(x_pos)
  
  d<-0.05
  d2<-d^2
  
  possible_xs<-seq(0,500,by=0.01)
  
  yz<-y[1]
  yzR<-rY(yz)
  xz<-0
  xzR<-rX(xz)
  if (length(y)>1)
  for (i in 2:length(y)){
    this_y<-rY(y[i])
    dy2<-(yzR-this_y)^2
    for (possible_x in possible_xs) {
      this_x<-rX(possible_x)
      distances1=dy2+(xzR-this_x)^2
      use1<-min(distances1)
      if (orientation=="vert") {
        this_xneg<- -this_x
        distances2=dy2+(xzR-this_xneg)^2
        use2<-min(distances2)
        if (all(c(use1,use2)>d2)) {
          if (use2>use1) possible_x<- -possible_x
          break
        }
      } else {
        if (all(use1>d2)) break
      }
    }
    xz<-c(xz,possible_x)
    xzR<-c(xzR,rX(possible_x))
    yz<-c(yz,y[i])
    yzR<-c(yzR,this_y)
  }
  
  if (orientation=="horz") xz<-xz/2
  return(xz)
}

get_upperEdge<-function(allvals,svals){
  if (isempty(svals)) target1<-min(allvals)
  else   target1<-min(svals,na.rm=TRUE)
  if (any(allvals<target1,na.rm=TRUE)){
    target2<-max(allvals[allvals<target1],na.rm=TRUE)
    target<-(target1+target2)/2
  } else target<-target1+0.001
}
get_lowerEdge<-function(allvals,svals) {
  if (isempty(svals)) target1<-min(allvals)
  else   target1<-min(svals,na.rm=TRUE)
  if (any(allvals<target1)){
    target2<-max(allvals[allvals<target1],na.rm=TRUE)
    if (target2==-Inf) target2=target1-0.5
    target<-(target1+target2)/2
  } else {target<-target1-0.5}
}
getBins<-function(vals,nsvals,target,minVal,maxVal,fixed=FALSE) {
  if (min(vals,na.rm=TRUE)==max(vals,na.rm=TRUE)) {
    bins<-min(vals)+min(vals)/10*c(-1.5,-0.5,0.5,1.5)
    return(bins)
  }
  maxBins<-braw.env$maxBins
  
  nv=max(length(nsvals),length(vals))
  nb<-min(round(sqrt(nv)*0.75),maxBins)
  
  high_p<-max(vals,na.rm=TRUE)+0.0000002
  low_p<-min(vals,na.rm=TRUE)-0.0000002
  if (!is.null(minVal)) {
    low_p<-max(minVal,low_p,na.rm=TRUE)
  }
  if (!is.null(maxVal)) {
    high_p<-min(maxVal,high_p,na.rm=TRUE)
  }
  
  if ((length(nsvals)==0) || (length(nsvals)==length(vals))){
    bins<-seq(low_p,high_p,length.out=nb)
    return(bins)
  }
  
  if (fixed) {
    target_low<-max(-target,low_p)
    target_high<-min(target,high_p)
    targetRange<-target_high-target_low
    nbs<-ceiling(nb*targetRange/(high_p-low_p))
    binStep<-targetRange/nbs
    if (target_low<target_high)   {
      bins<-seq(target_low,target_high,binStep)
      if (target<high_p)  bins<-c(bins,seq(target+binStep,high_p+binStep,binStep))
      if (-target>low_p)  bins<-c(rev(seq(-target-binStep,low_p-binStep,-binStep)),bins)
    } 
    else  {
      bins<-seq(target_high,target_low,binStep)
      if (-target<high_p)  bins<-c(bins,seq(-target+binStep,high_p+binStep,binStep))
      if (target>low_p)  bins<-c(rev(seq(target-binStep,low_p-binStep,-binStep)),bins)
    }                         
    return(bins)
  } 
  
  # make sure it goes through target
  if (length(target)>1) {
    if (high_p>target[2] && low_p< target[1]) {
      nbs<-ceiling(nb*(target[2]-0)/(high_p-low_p))
      binStep<-target[2]/nbs
      bins<-c(rev(seq(0,low_p-binStep,-binStep)),seq(binStep,high_p,binStep))
      return(bins)
    }
    if (high_p>target[2]) {
      nbs<-ceiling(nb*(high_p-target[2])/(high_p-low_p))
      binStep<-(high_p-target[2])/nbs
      bins<-rev(seq(high_p,low_p-binStep,-binStep))
      return(bins)
    } 
    if (low_p<target[1]) {
      nbs<-ceiling(nb*(target[1]-low_p)/(high_p-low_p))
      binStep<-(target[1]-low_p)/nbs
      bins<-seq(low_p-binStep,high_p,binStep)
      return(bins)
    } 
  } else {
    if (high_p>target) {
      nbs<-ceiling(nb*(high_p-target)/(high_p-low_p))
      binStep<-(high_p-target)/nbs
      bins<-rev(seq(high_p,low_p-binStep,-binStep))
      return(bins)
    } 
    if (low_p<target) {
      nbs<-ceiling(nb*(target-low_p)/(high_p-low_p))
      binStep<-(target-low_p)/nbs
      bins<-seq(low_p-binStep,high_p,binStep)
      return(bins)
    } 
  }
  # if all else fails
  binStep<-(high_p-low_p)/nb
  bins<-seq(low_p-binStep,high_p,binStep)
  return(bins)
}

simulations_hist<-function(pts,valType,ylim,histGain,histGainrange,orientation){
  
  vals<-pts$y1
  svals<-pts$y1[pts$sig>0]
  
  if (is.null(valType)) valType<-"rs"
  if (is.element(valType,c("rse","sig","ns","nonnulls","nulls","rss","ro","ci1","ci2","llknull","metaRiv"))) valType<-"rs"
  if (is.element(valType,c("e1p","e2p","po"))) valType<-"p"
  if (is.element(valType,c("wp","ws"))) valType<-"ws"
  if (is.element(valType,c("iv.mn","iv.sd","iv.sk","iv.kt",
                           "dv.mn","dv.sd","dv.sk","dv.kt",
                           "er.mn","er.sd","er.sk","er.kt"))) valType<-"mn"
  
  switch (valType,
          "rs"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "re"=  { # ns is small
            target<-get_upperEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "p"=  { # ns is large
            if (braw.env$pPlotScale=="log10") {
              target<-log10(braw.env$alphaSig)
              bins<-getBins(vals,svals,target,ylim[1],log10(1))
            } else {
              target<-braw.env$alphaSig
              bins<-getBins(vals,svals,target,0,1)
              bins<-c(0,bins[bins>0])
            }
          },
          
          "rp"=  { # ns is small
            target<-0.3
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "log(lrs)"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),0,braw.env$lrRange)
          },
          
          "e1d"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),-braw.env$lrRange,braw.env$lrRange)
          },
          
          "log(lrd)"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),-braw.env$lrRange,braw.env$lrRange)
          },
          
          "e2d"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),-braw.env$lrRange,braw.env$lrRange)
          },
          
          "ws"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            target[1]<-0.05
            bins<-getBins(vals,svals,target,log10(braw.env$min_p),NULL)
          },
          
          "n"= { # ns is small
            target<-get_lowerEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,10000)
            if (is.integer(vals)) {
              bins<-unique(floor(bins))
              binStep<-max(floor(median(diff(bins))),1)
              bins<-seq(bins[1],bins[length(bins)],binStep)
            }
          },
          
          "nw"= { # ns is large
            target<-get_lowerEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,braw.env$max_nw)
          },
          
          {bins<-getBins(vals,NULL,NULL,NULL,NULL,fixed=TRUE)}
          
  )
  
  use<-vals>=bins[1] & vals<bins[length(bins)]
  
  vals<-pts$y1[use]
  sigs<-pts$sig[use]
  nonnulls<-pts$notNull[use]
  if (all(is.na(nonnulls))) nonnulls<-rep(TRUE,length(vals))
  
  vals1<-vals[nonnulls & sigs==1]
  dens1<-hist(vals1,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens1<-dens1$counts
  
  vals2<-vals[nonnulls & sigs==0]
  dens2<-hist(vals2,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens2<-dens2$counts
  
  vals3<-vals[!nonnulls & sigs==0]
  dens3<-hist(vals3,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens3<-dens3$counts
  
  vals4<-vals[!nonnulls & sigs==1]
  dens4<-hist(vals4,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens4<-dens4$counts
  dens0<-dens1+dens2+dens3+dens4
  
  if (is.numeric(sigs)) {
    vals5<-vals[nonnulls & sigs==-1]
    dens5<-hist(vals5,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
    dens5<-dens5$counts
    
    vals6<-vals[!nonnulls & sigs==-1]
    dens6<-hist(vals6,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
    dens6<-dens6$counts
    
    dens0<-dens0+dens5+dens6
  } else {
    dens5<-NULL
    dens6<-NULL
  }
  
  if (is.na(histGain)) {
    switch(orientation,
           "horz"={scale<-0.8/max(dens0)},
           "vert"={scale<-0.35/max(dens0)}
           )
  } else {
    use<- (bins>=histGainrange[1]) & (bins<=histGainrange[2])
    gain<-sum(dens0[use]*c(0,diff(bins[use])),na.rm=TRUE)
    scale<-histGain/gain
  }
  # x<-(bins[1:(length(bins)-1)]+bins[2:(length(bins))])/2
  return(list(x=bins,h1=dens1*scale,h2=dens2*scale,h3=dens3*scale,h4=dens4*scale,h5=dens5*scale,h6=dens6*scale))
  # # browser()
  # x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
  # y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
  # y2<-c(0,as.vector(matrix(c(sdens,sdens),2,byrow=TRUE)),0)
  # data.frame(y1=c(-y1,rev(y1)), y2=c(-y2,rev(y2)), x=c(x,rev(x)))
}

simulations_plot<-function(g,pts,showType=NULL,simWorld,design,
                        i=1,scale=1,width=1,col="white",alpha=0.6,useSignificanceCols=braw.env$useSignificanceCols,
                        histStyle="width",orientation="vert",ylim,histGain=NA,histGainrange=NA,
                        npointsMax=braw.env$npointsMax,sequence=FALSE){

  se_size<-0.25
  c1=col
  c2=col
  c3=col
  c4=col
  
  doingDLLR<-FALSE
  if (!is.null(showType)) {
    if (useSignificanceCols){
      if (is.numeric(pts$sig)) {
        c1<-braw.env$plotColours$infer_sigNonNull
        c2<-braw.env$plotColours$infer_nsigNonNull
        c3<-braw.env$plotColours$infer_nsigNull
        c4<-braw.env$plotColours$infer_sigNull
        c5<-braw.env$plotColours$infer_isigNonNull
        c6<-braw.env$plotColours$infer_isigNull
        doingDLLR<-TRUE
      }
      if (is.element(showType,c("rse","sig","ns","nonnulls","nulls"))) {
        c1<-braw.env$plotColours$infer_sigNonNull
        c2<-braw.env$plotColours$infer_nsigNonNull
        c3<-braw.env$plotColours$infer_nsigNull
        c4<-braw.env$plotColours$infer_sigNull
      } else {
        c1<-braw.env$plotColours$infer_sigNonNull
        c2<-braw.env$plotColours$infer_nsigNull
        c3<-braw.env$plotColours$infer_nsigNull
        c4<-braw.env$plotColours$infer_sigNonNull
      }
      if (all(is.na(pts$notNull))) pts$notNull<-rep(TRUE,length(pts$sig))

      if (showType=="e1r") {
        c1=braw.env$plotColours$infer_sigNull
        c2=braw.env$plotColours$infer_nsigNull
      }
      if (showType=="e2r") {
        c1=braw.env$plotColours$infer_sigNonNull
        c2=braw.env$plotColours$infer_nsigNonNull
      }
      if (showType=="e1p") {
        c1=braw.env$plotColours$infer_sigNull
        c2=braw.env$plotColours$infer_nsigNull
      }
      if (showType=="e2p") {
        c1=braw.env$plotColours$infer_sigNonNull
        c2=braw.env$plotColours$infer_nsigNonNull
      }
      if (showType=="e1d") {
        c1=braw.env$plotColours$infer_sigNull
        c2=braw.env$plotColours$infer_nsdNull
        c3<-braw.env$plotColours$infer_isigNull
      }
      if (showType=="e2d") {
        c1=braw.env$plotColours$infer_sigNonNull
        c2=braw.env$plotColours$infer_nsdNonNull
        c3<-braw.env$plotColours$infer_isigNonNull
      }
      if (showType=="wp") c1<-c2<-c3<-c4<-braw.env$plotColours$powerPopulation
    }
  }
  if (length(pts$y1)<=npointsMax) {

    if (is.element(showType,c("rs","rse","sig","ns","nonnulls","nulls","rss")) && length(pts$y1)==1) {
          n<-pts$n
          r<-pts$y1
          rCI<-r2ci(r,n)
          if (pts$sig) c<-c1 else c=c2
          x<-pts$x
          if (length(x)<length(rCI)) x<-rep(x,length(rCI))
          pts1se<-data.frame(y=rCI[1,],x=x)
          # g<-addG(g,dataLine(data=pts1se,colour="white",linewidth=se_size))
    }
    
    # if (is.null(pts$notNull)) 
    #   use<-c(which(pts$sig),which(!pts$sig),which(!pts$sig),which(pts$sig))
    # else
    if (!sequence) {
      use<-c(which(pts$sig & pts$notNull),which(!pts$sig & pts$notNull),which(!pts$sig & !pts$notNull),which(pts$sig & !pts$notNull))
    pts<-pts[use,]
    }
    xr<-makeFiddle(pts$y1,2/40/braw.env$plotArea[4],orientation)
    
    # pa<-chull(pts$y1,xr)
    # p1<-abs(polyarea(pts$y1[pa],xr[pa]))
    # 
    if (sequence && !is.na(histGain)) histGain<-0.8
    switch(orientation,
           "horz"=hoff<-0.025,
           "vert"=hoff<-0
           )
    dotSize<-min(4,braw.env$dotSize*sqrt(min(1,100/length(pts$y1))))
    # if (max(abs(xr))>0) xr<-xr*hgain/max(abs(xr))
    if (!is.na(histGain)) {
      xr<-xr*histGain
      if (!sequence && max(xr)<0.5 && length(xr)>10) xr<-xr/max(abs(xr))*0.5
    }
    else xr<-xr/max(abs(xr))*0.25
    if (max(xr)>0.5) xr<-xr/max(abs(xr))*0.5
    xr<-xr*min(1,length(xr)/100)
    xr<-xr+hoff
    
    pts$x<-pts$x+xr*sum(width)*0.3/0.35
    if (sequence)  {
      pts$x<-0+((1:nrow(pts))-1)/nrow(pts)/5
      if (design$Replication$On && design$Replication$Keep=="MetaAnalysis") {
        np<-length(pts$x)
        metaPts<-pts[np,]
        pts<-pts[1:(np-1),]
      }
    }   
    
    gain<-50/max(50,length(xr))
    colgain<-1-min(1,sqrt(max(0,(length(xr)-50))/200))
    
    if (scale<1) {
      co1<-c1
      co2<-c2
    } else {
      co1<-"#000000"
      co2<-"#000000"
    }
    co1<-darken(c1,off=-colgain)
    co2<-darken(c2,off=-colgain)
    shape<-braw.env$plotShapes$study
    if (is.element(showType,c("metaRiv","metaRsd","metaK","metaSpread","metaShape","metaBias","metaSmax"))) { # metaAnalysis
      shape<-braw.env$plotShapes$meta
      c1<-c2<-c3<-c4<-braw.env$plotColours$metaMultiple
    }
    
    if (useSignificanceCols) {
      makeData<-function(x,y,or) switch(or,"horz"={data.frame(x=x,y=y)},"vert"={data.frame(x=y,y=x)})
      pts_sigNonNull=pts[pts$sig==1 & pts$notNull,]
      pts_nsNonNull=pts[pts$sig==0 & pts$notNull,]
      pts_sigNull=pts[pts$sig==1 & !pts$notNull,]
      pts_nsNull<-pts[pts$sig==0 & !pts$notNull,]
      sigNonNullData<-makeData(x=pts_sigNonNull$y1,y=pts_sigNonNull$x,orientation)
      nsNonNullData<-makeData(x=pts_nsNonNull$y1,y=pts_nsNonNull$x,orientation)
      sigNullData<-makeData(x=pts_sigNull$y1,y=pts_sigNull$x,orientation)
      nsNullData<-makeData(x=pts_nsNull$y1,y=pts_nsNull$x,orientation)
      
      g<-addG(g,dataPoint(data=sigNonNullData,shape=shape, 
                          colour = "black", alpha=alpha, fill = c1, size = dotSize))
      g<-addG(g,dataPoint(data=nsNonNullData,shape=shape, 
                          colour = "black", alpha=alpha, fill = c2, size = dotSize))
      g<-addG(g,dataPoint(data=nsNullData,shape=shape, 
                          colour = "black", alpha=alpha, fill = c3, size = dotSize))
      g<-addG(g,dataPoint(data=sigNullData,shape=shape, 
                          colour = "black", alpha=alpha, fill = c4, size = dotSize))
      if (doingDLLR) {
        pts_isigNonNull=pts[pts$sig==-1 & pts$notNull,]
        pts_isigNull=pts[pts$sig==-1 & !pts$notNull,]
        isigNonNullData<-makeData(x=pts_isigNonNull$y1,y=pts_isigNonNull$x,orientation)
        isigNullData<-makeData(x=pts_isigNull$y1,y=pts_isigNull$x,orientation)
        if (length(isigNonNullData)>0)
        g<-addG(g,dataPoint(data=isigNonNullData,shape=shape, 
                            colour = darken(c5,off=-colgain), alpha=alpha, fill = c5, size = dotSize))
        if (length(isigNullData)>0)
          g<-addG(g,dataPoint(data=isigNullData,shape=shape, 
                            colour = darken(c6,off=-colgain), alpha=alpha, fill = c6, size = dotSize))
      }
    } else {
      switch(orientation,
             "horz"={
               mainData<-data.frame(x=pts$y1,y=pts$x)
             },
             "vert"={
               mainData<-data.frame(x=pts$x,y=pts$y1)
             })
      g<-addG(g,dataPoint(data=mainData,shape=shape, 
                          colour = darken(c1,off=-colgain), alpha=alpha, fill = c1, size = dotSize))
    }
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        pts_wsig=pts[pts$y3,]
        g<-addG(g,dataPoint(data=data.frame(x=pts_wsig$x,y=pts_wsig$y1),shape=braw.env$plotShapes$study, colour = co1, alpha=alpha, fill = c3, size = dotSize))
      }
    if (sequence==1) {
      if (design$Replication$On && design$Replication$Keep=="MetaAnalysis") {
        if (metaPts$sig && metaPts$notNull) c3<-braw.env$plotColours$infer_sigNonNull
        if (!metaPts$sig && metaPts$notNull) c3<-braw.env$plotColours$infer_nsigNonNull
        if (metaPts$sig && !metaPts$notNull) c3<-braw.env$plotColours$infer_sigNull
        if (!metaPts$sig && !metaPts$notNull) c3<-braw.env$plotColours$infer_nsigNull
        g<-addG(g,dataPoint(makeData(x=metaPts$y1,y=metaPts$x,orientation),
                            shape=braw.env$plotShapes$meta,
                            fill = c3, 
                            size=dotSize))
      }
      g<-addG(g,dataPath(makeData(pts$y1,pts$x,orientation),arrow=TRUE,linewidth=0.75,colour="white"))
    }
  } else { # more than 250 points
    hists<-simulations_hist(pts,showType,ylim,histGain,histGainrange,orientation)
    gh<-max(hists$h1+hists$h2+hists$h3+hists$h4)
    if (gh>0.8) {
      hists$h1<-hists$h1/gh*0.8
      hists$h2<-hists$h2/gh*0.8
      hists$h3<-hists$h3/gh*0.8
      hists$h4<-hists$h4/gh*0.8
    }
    xoff<-pts$x[1]
    if (orientation=="vert") {
      simAlpha<-0.85
    } else {
      simAlpha<-0.85
    }
    dx<-diff(hists$x[1:2])

    if (length(width)==1) width=c(width,width)
    dens<-cbind(hists$h1,hists$h2,hists$h3,hists$h4)
    cols<-c(c1,c2,c3,c4)
    if (doingDLLR) {
      dens<-cbind(hists$h2,hists$h5,hists$h1,hists$h3,hists$h4,hists$h6)
      cols<-c(c1,c5,c4,c3,c2,c6)
    }
    for (i in 1:nrow(dens)) {
      ystart<-0
      for (j in 1:ncol(dens)) {
          if (histStyle=="width" && j>1) ystart<-ystart+dens[i,j-1] 
          else ystart<-0  
          if (dens[i,j]>0) {
            if (histStyle=="width") {
            alpha<-simAlpha
            alpha<-1
            colour<-cols[j]
            w<-dens[i,j]
          } else {
            alpha<-(dens[j]/0.35)^0.6
            colour<-NA
            # alpha<-1
            w<-0.5/0.35
          }
          # alpha<-1
          if (i==1) w0<-0
          else w0<-dens[i-1,j]
          switch(orientation,
                 "vert"={
                   data<-data.frame(y=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                                    x=-(c(w,0,0,w)+ystart)*width[1]+xoff)
                   g<-addG(g,dataPolygon(data=data,
                                         colour=colour, fill = cols[j],alpha=alpha))
                   data<-data.frame(y=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                                    x=(c(w,0,0,w)+ystart)*width[2]+xoff)
                   g<-addG(g,dataPolygon(data=data,
                                         colour=colour, fill = cols[j],alpha=alpha))
                 },
                 "horz"={
                   data<-data.frame(x=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                                    y=(c(w,0,0,w)+ystart)*width[1]+xoff)
                   g<-addG(g,dataPolygon(data=data,
                                         colour=colour, fill = cols[j],alpha=alpha))
                 })
        }
      }
    }
    if (orientation=="vert") {
      dens1<-rowSums(dens)
      x<-c(hists$x[1],hists$x[1])
      y<-c(0,dens1[1])
      for (i in 1:(nrow(dens)-1)) {
        x<-c(x,hists$x[i],hists$x[i+1],hists$x[i+1])
        y<-c(y,dens1[i],dens1[i],dens1[i+1])
      }
       x<-c(x,hists$x[nrow(dens)],hists$x[nrow(dens)+1],hists$x[nrow(dens)+1]) 
       y<-c(y,dens1[nrow(dens)],dens1[nrow(dens)],0)
       g<-addG(g,dataLine(data.frame(y=x*width[2]+xoff,x=y),colour="black"))
       g<-addG(g,dataLine(data.frame(y=x*width[1]+xoff,x=-y),colour="black"))
    } else {
      dens1<-rowSums(dens)
      x<-c(hists$x[1],hists$x[1])
      y<-c(0,dens1[1])
      for (i in 1:(nrow(dens)-1)) {
        x<-c(x,hists$x[i],hists$x[i+1],hists$x[i+1])
        y<-c(y,dens1[i],dens1[i],dens1[i+1])
      }
      x<-c(x,hists$x[nrow(dens)],hists$x[nrow(dens)+1],hists$x[nrow(dens)+1]) 
      y<-c(y,dens1[nrow(dens)],dens1[nrow(dens)],0)
      g<-addG(g,dataLine(data.frame(x=x*width[2]+xoff,y=y),colour="black"))
    }
    # g<-addG(g,
    #   dataPolygon(data=data.frame(y=hist1$x,x=hist1$y1+xoff),colour=NA, fill = c2,alpha=simAlpha),
    #   dataPolygon(data=data.frame(y=hist1$x,x=hist1$sig+xoff),colour=NA, fill = c1,alpha=simAlpha)
    # )
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        if (is.logical(pts$y3)) {
          hist1<-simulations_hist(pts,showType,ylim,orientation=orientation)
        }
        g<-addG(g,
          dataPolygon(data=data.frame(y=hist1$x,x=hist1$sig+xoff),colour=NA, fill = c3,alpha=simAlpha))
      }
  }
  g
}

r_plot<-function(analysis,showType="rs",logScale=FALSE,otheranalysis=NULL,
                 orientation="vert",whichEffect="Main 1",effectType="all",
                 showTheory=TRUE,showData=TRUE,showLegend=FALSE,showNsims=FALSE,
                 showYaxis=TRUE,
                 g=NULL){

  baseColour<-braw.env$plotColours$infer_nsigC
  theoryFirst<-FALSE
  npct<-0
  showSig<-TRUE
  labelSig<-TRUE
  labelNSig<-TRUE
  top<-1
  if (is.element(showType,c("e1d","e2d"))) top<-1.5
  if (is.element(showType,c("rse","sig","ns","nonnulls","nulls","rss"))) top<-1.5

  if (showType=="wp") {
    showSig<-FALSE
    baseColour<-plotAxis("wp",analysis$hypothesis)$cols[1]
  }
  
  if (is.element(showType,c("iv.mn","iv.sd","iv.sk","iv.kt",
                            "dv.mn","dv.sd","dv.sk","dv.kt",
                            "er.mn","er.sd","er.sk","er.kt"))) {
    # showSig<-FALSE
    labelSig<-FALSE
    labelNSig<-FALSE
  }

  if (showType=="e1p") {
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e2p") {
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (is.element(showType,c("e1r","e1+","e1-"))) {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (is.element(showType,c("e2r","e2+","e2-"))) {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e1a") {
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e2a") {
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e1b") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-FALSE
    # top<-TRUE
  }
  
  if (showType=="e2b") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-FALSE
    # top<-TRUE
  }
  
  if (is.element(showType,c("metaRiv","metaK","metaSpread","metaShape","metaBias","metaSmax"))){
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-FALSE
    showSig<-FALSE
    doingMetaAnalysis<-TRUE
    showTheory<-FALSE
    ydata<-analysis$best$Smax
    # top<-TRUE
  } else {
    doingMetaAnalysis<-FALSE
    ydata<-NULL
  }
  
  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  evidence<-analysis$evidence
  if (design$Replication$On && !is.null(analysis$ResultHistory$original$evidence$sigOnly)) 
    evidence$sigOnly<-analysis$ResultHistory$original$evidence$sigOnly
  
  sequence<-analysis$sequence
  if (is.null(sequence)) sequence<-FALSE
  
  r<-effect$rIV
  if (!is.null(hypothesis$IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }

  switch(braw.env$RZ,
         "r"={
           rlims<-c(-1,1)
         },
         "z"={
           r<-atanh(r)
           rlims<-c(-1,1)*braw.env$z_range
         })

  xlabel<-NULL
  if (doingMetaAnalysis) result<-analysis$best$Smax
  else result<-NULL
  
  yaxis<-plotAxis(showType,hypothesis,design,result=result)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ylines<-yaxis$lines
  logScale<-yaxis$logScale
  if (is.element(showType,c("rs","rse","sig","ns","nonnulls","nulls","rss")) && (!is.null(hypothesis$IV2))) 
    switch(whichEffect,"Model"={mTitle<-"Model";ylines<-c(0)},
                       "Main 1"={mTitle<-hypothesis$IV$name;ylines<-c(0,hypothesis$effect$rIV)},
                       "Main 2"={mTitle<-hypothesis$IV2$name;ylines<-c(0,hypothesis$effect$rIV2)},
                       "Interaction"={mTitle<-paste0(hypothesis$IV$name,braw.env$interaction_string,hypothesis$IV2$name);ylines<-c(0,hypothesis$effect$rIVIV2DV)},
                       "Covariation"={mTitle<-paste0(hypothesis$IV$name,braw.env$covariation_string,hypothesis$IV2$name)}
           )
  else mTitle<-""
  if (!showYaxis) ylabel<-NULL
  
  if (showType=="p" && braw.env$pPlotScale=="log10" && any(is.numeric(analysis$pIV)) && any(analysis$pIV>0)) 
    while (mean(log10(analysis$pIV)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
  
  xoff=0
  iUse<-1
  if (!is.null(hypothesis$IV2) && effectType=="all") {
    xoff=c(0,1,2)*1.2
    iUse<-1:sum(evidence$AnalysisTerms)
  }
  if (!is.null(hypothesis$IV2) && effectType!="all"){
    if (effectType=="direct") iUse<-1
    if (effectType=="unique") iUse<-2
    if (effectType=="total") iUse<-3
  }
  ydlim<-ylim
  switch(orientation,
         "horz"={
           xlim<-ylim
           xlim<=xlim+c(-1,1)*diff(xlim)/5
           ylim<-c(0,1)
           orient<-"horz"
           box<-"X" 
           if (is.null(ylabel)) {
             xlabel<-NULL
             xticks<-NULL
           } else {
             xlabel<-makeLabel(ylabel)
             xticks<-makeTicks(logScale=yaxis$logScale)
           }
           ylabel<-NULL
           yticks<-NULL
           xmax<-FALSE
           ymax<-TRUE
         },
         "vert"={
           xlim<-c(0,max(xoff))+c(-1,1)/2
           ylim<-ylim+c(-1,1)*diff(ylim)/25
           orient<-"horz"
           box<-"Y" 
           if (is.null(ylabel)) {
             ylabel<-NULL
             yticks<-NULL
           } else{
             ylabel<-makeLabel(ylabel)
             yticks<-makeTicks(logScale=yaxis$logScale)
           }
           xlabel<-whichEffect
           xticks<-NULL
           xmax<-TRUE
           ymax<-FALSE
         }
  )
  
  if (!is.null(hypothesis$IV2) && whichEffect=="All" && effectType=="all") 
    xticks<-makeTicks(breaks=xoff,c("direct","unique","total"))

  g<-startPlot(xlim,ylim,
               xticks=xticks,xlabel=xlabel,xmax=xmax,
               yticks=yticks,ylabel=ylabel,ymax=ymax,
               box="both",top=top,orientation=orient,g=g)
  
  nr<-sum(!is.na(analysis$rIV))
  if (showNsims) {
  if (is.element(showType[1],c("NHST","Hits","Misses","Inference","SEM")) && sum(!is.na(analysis$nullresult$rIV))>0) {
    n1<-brawFormat(sum(analysis$rpIV!=0))
    n2<-brawFormat(sum(!is.na(analysis$rpIV==0)))
    title<-paste("nsims = ",n1,"+",n2,sep="")
  } else {
    n1<-brawFormat(nr)
    title<-paste("nsims = ",n1,sep="")
  }
    if (nr>1 && !sequence)   g<-addG(g,plotTitle(title,size=0.65,fontface="normal"))
  }
  
  
  lineCol<-"#000000"
  if (is.element(showType,c("p","e1p","e2p","e1d","e2d"))) lineCol<-"green"
  switch(orientation,
         "horz"={
           # for (yl in ylines)
           #   g<-addG(g,vertLine(intercept=yl,linewidth=0.25,linetype="dashed",colour=lineCol))
         },
         "vert"={
           for (yl in ylines)
             g<-addG(g,horzLine(intercept=yl,linewidth=0.25,linetype="dashed",colour=lineCol))
         })
  
  sampleVals<-c()
  if (!all(is.na(analysis$rIV)) || doingMetaAnalysis) {
    data<-collectData(analysis,whichEffect)
    # if (length(data$rs)>0) dw<-1/max(250,length(data$rs))
    switch(braw.env$RZ,
           "r"={},
           "z"={
             data$rs<-atanh(data$rs)
             data$rp<-atanh(data$rp)
             data$ro<-atanh(data$ro)
           })
    switch (showType,
            "rs"={sampleVals<-data$rs},
            "rse"={sampleVals<-data$rs},
            "sig"={sampleVals<-data$rs},
            "ns"={sampleVals<-data$rs},
            "nonnulls"={sampleVals<-data$rs},
            "nulls"={sampleVals<-data$rs},
            "rss"={sampleVals<-data$rs},
            "rp"={sampleVals<-data$rp},
            "ro"={sampleVals<-data$ro},
            "re"={sampleVals<-data$rs-data$rp},
            "p"={sampleVals<-data$ps},
            "po"={sampleVals<-data$po},
            "metaRiv"={sampleVals<-cbind(analysis$best$PDFk)},
            "metaRsd"={sampleVals<-cbind(analysis$best$PDFspread)},
            "metaK"={sampleVals<-cbind(analysis$best$PDFk)},
            "metaSpread"={sampleVals<-cbind(analysis$best$PDFspread)},
            "metaShape"={sampleVals<-cbind(analysis$best$PDFshape)},
            "metaPDFk"={sampleVals<-cbind(analysis$best$PDFk)},
            "metaPRplus"={sampleVals<-cbind(analysis$best$pRplus)},
            "metaBias"={sampleVals<-cbind(analysis$best$sigOnly)},
            "metaSmax"={sampleVals<-cbind(analysis$best$Smax)},
            "llknull"={sampleVals<-exp(cbind(-0.5*(analysis$AIC-analysis$AICnull)))},
            "sLLR"={sampleVals<-cbind(res2llr(analysis,"sLLR"))},
            "log(lrs)"={sampleVals<-cbind(res2llr(analysis,"sLLR"))},
            "log(lrd)"={sampleVals<-cbind(res2llr(analysis,"dLLR"))},
            "e1d"={sampleVals<-cbind(res2llr(analysis,"dLLR"))},
            "e2d"={sampleVals<-cbind(res2llr(analysis,"dLLR"))},
            "n"={sampleVals<-data$ns},
            "ws"={sampleVals<-rn2w(data$rs,data$ns)},
            "wp"={sampleVals<-rn2w(data$rp,data$ns)},
            "nw"={sampleVals<-rw2n(data$rs,0.8,design$Replication$Tails)},
            "ci1"={sampleVals<-r2ci(data$rs,data$ns,-1)},
            "ci2"={sampleVals<-r2ci(data$rs,data$ns,+1)},
            "e1r"={sampleVals<-data$rs},
            "e2r"={sampleVals<-data$rs},
            "e1+"={sampleVals<-data$rs},
            "e2+"={sampleVals<-data$rs},
            "e1-"={sampleVals<-data$rs},
            "e2-"={sampleVals<-data$rs},
            "e1p"={sampleVals<-data$ps},
            "e2p"={sampleVals<-data$ps},
            "iv.mn"=sampleVals<-data$iv.mn,
            "iv.sd"=sampleVals<-data$iv.sd,
            "iv.sk"=sampleVals<-data$iv.sk,
            "iv.kt"=sampleVals<-data$iv.kt,
            "dv.mn"=sampleVals<-data$dv.mn,
            "dv.sd"=sampleVals<-data$dv.sd,
            "dv.sk"=sampleVals<-data$dv.sk,
            "dv.kt"=sampleVals<-data$dv.kt,
            "er.mn"=sampleVals<-data$er.mn,
            "er.sd"=sampleVals<-data$er.sd,
            "er.sk"=sampleVals<-data$er.sk,
            "er.kt"=sampleVals<-data$er.kt,
    )
    if (logScale) {
      sampleVals<-log10(sampleVals)
      sampleVals[sampleVals<(-10)]<--10
    }  
    if (nrow(sampleVals)<=1000) theoryFirst<-TRUE
  } 

  if (doingMetaAnalysis) { theoryAlpha<-0.5} else {theoryAlpha<-0.8}
  
  for (i in 1:length(xoff)){
    histGain<-NA
    histGainrange<-c(NA,NA)
    switch(orientation,
           "horz"=distGain<-0.8,
           "vert"=distGain<-0.45
    )
    
    if (showTheory) {
      # because the theory can be slow...
      possibleTheory<-braw.res[[paste0("theoryMultiple",showType)]]
      if (!is.null(possibleTheory) 
          && identical(hypothesis,possibleTheory$hypothesis)
          && identical(design,possibleTheory$design)
          && identical(evidence,possibleTheory$evidence)
          && showType==possibleTheory$showType
          && whichEffect==possibleTheory$whichEffect
          && logScale==possibleTheory$logScale
          && identical(ydlim,possibleTheory$ydlim)
          && labelNSig==possibleTheory$labelNSig
          && labelSig==possibleTheory$labelSig
          && distGain==possibleTheory$distGain
      ) theory<-possibleTheory$theory
      else {
        theory<-makeTheoryMultiple(hypothesis,design,evidence,showType,whichEffect,logScale,ydlim,labelNSig,labelSig,distGain)
        setBrawRes(paste0("theoryMultiple",showType),
                   list(theory=theory,hypothesis=hypothesis,design=design,evidence=evidence,
                        showType=showType,whichEffect=whichEffect,logScale=logScale,ydlim=ydlim,
                        labelNSig=labelNSig,labelSig=labelSig,distGain=distGain)
                   )
      }
      theoryVals<-theory$theoryVals
      theoryDens_all<-theory$theoryDens_all
      theoryDens_sig<-theory$theoryDens_sig

      if (theoryFirst)
      g<-theoryPlot(g,theory,orientation,baseColour,theoryAlpha,xoff[i])

      histGainrange<-sort(c(theoryVals[1],theoryVals[length(theoryVals)]))
      if (is.element(showType,c("wp","ws")))   histGainrange<-c(0.06,0.99)
      use<-theoryVals>=histGainrange[1] & theoryVals<=histGainrange[2]
      histGain<-abs(sum(theoryDens_all[use]*c(0,diff(theoryVals[use]))))
      # histGain<-histGain*distGain
    } else theory<-NULL
    
    # then the samples
    if (showData) {
    rvals<-c()
    if (!all(is.na(analysis$rIV)) || doingMetaAnalysis) {
      shvals<-sampleVals[,iUse[i]]
      if (showSig) {
        if (showType=="rss") 
          resSig<-(analysis$sem[,8]==2)
        else {
          rvals<-data$rs[,iUse[i]]
          pvals<-data$ps[,iUse[i]]
          nvals<-data$ns
          resSig<-isSignificant(braw.env$STMethod,pvals,rvals,nvals,data$df1,evidence)
        }
        resNotNull<-abs(data$rp)>evidence$minRp
      } else {
        resSig<-rep(FALSE,length(shvals))
        resNotNull<-rep(FALSE,length(shvals))
        nvals<-rep(NA,length(shvals))
      }
      
      if (effectType=="all") {
        ysc<-1/3
        rvals<-(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1
      }
      if (is.element(showType,c("e1d","e2d"))) {
        d<-res2llr(analysis,braw.env$STMethod)
        err<-(d<0 & abs(data$rp)>evidence$minRp) | (d>0 & abs(data$rp)<=evidence$minRp)
        resWSig<-resSig & err
        pts<-data.frame(x=shvals*0+xoff[i],y1=shvals,sig=resSig,y3=resWSig,notNull=resNotNull,n=nvals)
      } else {
          pts<-data.frame(x=shvals*0+xoff[i],y1=shvals,sig=resSig,notNull=resNotNull,n=nvals)
      }
      
      g<-simulations_plot(g,pts,showType,analysis$hypothesis$effect$world$On,analysis$design,
                          i,orientation=orientation,
                       ylim=ylim,histGain=histGain,histGainrange=histGainrange,
                       sequence=sequence)

      ns<-c()
      s<-c()
      if (length(rvals)>1 && is.element(showType,c("rs","rse","sig","ns","nonnulls","nulls","rss","p",
                                                   "e1r","e2r","e1+","e2+","e1-","e2-",
                                                   "e1p","e2p","e1d","e2d"))) {
        n<-length(pvals)
        if (!is.null(otheranalysis) && effect$world$On) n<-n+length(otheranalysis$pIV)
        ns<-sum(resSig==0,na.rm=TRUE)
        s<-sum(resSig>0,na.rm=TRUE)
        if (is.element(showType,c("rse","sig","ns","rss","nonnulls","nulls"))) {
          sc<-sum((resSig>0)&(resNotNull))
          nse<-sum((resSig==0)&(resNotNull))
          se<-sum((resSig>0)&(!resNotNull))
          nsc<-sum((resSig==0)&(!resNotNull))
          if (braw.env$STMethod=="dLLR") {
            isc<-sum((resSig<0)&(!resNotNull))
            inse<-sum((resSig<0)&(resNotNull))
          }
        }
        if (is.element(showType,c("e1d","e2d"))) {
          s2<-sum(resSig>0 & shvals<0,na.rm=TRUE)
          s1<-sum(resSig>0 & shvals>0,na.rm=TRUE)
        }
        if (is.element(showType,c("e1+","e2+"))) {
          s2<-sum(resSig>0,na.rm=TRUE)
          s1<-sum(resSig>0,na.rm=TRUE)
        }
        if (is.element(showType,c("e1-","e2-"))) {
          s2<-sum(resSig==0,na.rm=TRUE)
          s1<-sum(resSig==0,na.rm=TRUE)
        }
      }
    }  else {
      # no simulations
      n<-1
      ns<-c()
      switch (showType,
              "p"={
                s<-fullPSig(hypothesis$effect$world,design)
                ns<-1-s
              },
              "e1p"={
                pRplus<-hypothesis$effect$world$pRplus
                hypothesis$effect$world$pRplus<-0
                s<-fullPSig(hypothesis$effect$world,design)
                hypothesis$effect$world$pRplus<-pRplus
                ns<-1-s
                if (hypothesis$effect$world$On) {
                  s<-s*hypothesis$effect$world$pRplus
                  ns<-ns*hypothesis$effect$world$pRplus
                }
                if (labelSig != labelNSig) {
                  s<-s/fullPSig(hypothesis$effect$world,design)
                  ns<-ns/(1-fullPSig(hypothesis$effect$world,design))
                }
              },
              "e2p"={
                pRplus<-hypothesis$effect$world$pRplus
                hypothesis$effect$world$pRplus<-1
                s<-fullPSig(hypothesis$effect$world,design)
                hypothesis$effect$world$pRplus<-pRplus
                ns<-1-s
                if (hypothesis$effect$world$On) {
                  s<-s*(hypothesis$effect$world$pRplus)
                  ns<-ns*(hypothesis$effect$world$pRplus)
                }
                if (labelSig != labelNSig) {
                  s<-s/fullPSig(hypothesis$effect$world,design)
                  ns<-ns/(1-fullPSig(hypothesis$effect$world,design))
                }
              },
              "e1d"={
                ns<-sum(!resSig,na.rm=TRUE)
                s2<-sum(resSig & shvals<0,na.rm=TRUE)
                s1<-sum(resSig & shvals>0,na.rm=TRUE)
              },
              "e2d"={
                ns<-sum(!resSig,na.rm=TRUE)
                s2<-sum(resSig & shvals<0,na.rm=TRUE)
                s1<-sum(resSig & shvals>0,na.rm=TRUE)
              }
      )
    }
    if (showTheory) {
    if (!theoryFirst)
      g<-theoryPlot(g,theory,orientation,baseColour,theoryAlpha/2,xoff[i])
    else
      g<-theoryPlot(g,theory,orientation,baseColour,theoryAlpha/2,xoff[i],lineOnly=TRUE)
    }
    
    if (is.element(showType,c("rse","sig","ns","nonnulls","nulls","rss","p","e1r","e2r","e1+","e2+","e1-","e2-",
                              "e1p","e2p","e1d","e2d"))) showLegend<-TRUE
    if (length(rvals)>1 && showLegend) {
      lb1<-"p("
      lb2<-") = "
      lb1<-"("
      lb2<-")="
      lb1<-""
      lb2<-"="
      labels<-c()
      colours<-c()
      title<-""
      if (braw.env$STMethod=="dLLR") nsLabel<-" ns" else nsLabel<-" error"
      if (braw.env$STMethod=="dLLR") sLabel<-" ns" else sLabel<-" correct"
      
      npad<-function(a) {if (nchar(a)<2) return(paste0("  ",a)) else return(a)}
      if (evidence$minRp!=0) nlab<-braw.env$Inactive else nlab<-braw.env$Null
      switch (showType,
              "rs"={
                if (!is.null(s)) {
                  labels<-c(labels,paste0(lb1,"sig",lb2,reportNumber(s,n,braw.env$reportCounts)))
                  colours<-c(colours,braw.env$plotColours$infer_sigC)
                }
                if (!is.null(ns)) {
                  labels<-c(labels,paste0(lb1,"ns",lb2,reportNumber(ns,n,braw.env$reportCounts)))
                  colours<-c(colours,braw.env$plotColours$infer_nsigC)
                }
              },
              "rse"={
                title<-""
                if (braw.env$STMethod=="dLLR")
                  if (!is.null(inse) && any(resNotNull)) {
                    labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(inse,n,braw.env$reportCounts),"'"," error"))
                    colours<-c(colours,braw.env$plotColours$infer_isigNonNull)
                  }
                if (evidence$sigOnly<1 || design$Replication$On) {
                    labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(nse,n,braw.env$reportCounts),"'",nsLabel))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNonNull)
                }
                  labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(sc,n,braw.env$reportCounts),"'"," correct"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNonNull)
                if (braw.env$STMethod=="dLLR") 
                  if (!is.null(isc) && any(!resNotNull)) {
                    labels<-c(labels,paste0(braw.env$Null," '",reportNumber(isc,n,braw.env$reportCounts),"'"," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_isigNull)
                  }
                  if (hypothesis$effect$world$On && hypothesis$effect$world$pRplus<1) {
                  if (evidence$sigOnly<1 || design$Replication$On) {
                      labels<-c(labels,paste0(nlab," '",reportNumber(nsc,n,braw.env$reportCounts),"'",sLabel))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNull)
                }
                  labels<-c(labels,paste0(nlab," '",reportNumber(se,n,braw.env$reportCounts),"'"," error"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNull)
                  }
              },
              "sig"={
                title<-"Hits"
                
                if (braw.env$STMethod=="dLLR")
                  if (!is.null(inse)) {
                    labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(inse,n,braw.env$reportCounts),"'"," error"))
                    colours<-c(colours,braw.env$plotColours$infer_isigNonNull)
                  }
                if (!is.null(sc)) {
                  labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(sc,n,braw.env$reportCounts),"'"," correct"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNonNull)
                }
                if (braw.env$STMethod=="dLLR") 
                  if (!is.null(isc)) {
                    labels<-c(labels,paste0(braw.env$Null," '",reportNumber(isc,n,braw.env$reportCounts),"'"," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_isigNull)
                  }
                if (!is.null(se)) {
                  labels<-c(labels,paste0(nlab," '",reportNumber(se,n,braw.env$reportCounts),"'"," error"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNull)
                }
                
              },
              "ns"={
                title<-"Misses"
                if (!is.null(nse)) {
                  labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(nse,n,braw.env$reportCounts),"'",nsLabel))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNonNull)
                }
                if (!is.null(nsc)) {
                  labels<-c(labels,paste0(nlab," '",reportNumber(nsc,n,braw.env$reportCounts),"'",sLabel))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNull)
                }
              },
              "nonnulls"={
                if (analysis$evidence$minRp!=0)
                  title<-braw.env$activeTitle
                else
                  title<-braw.env$nonnullTitle
                
                if (braw.env$STMethod=="dLLR")
                  if (!is.null(inse) && any(resNotNull)) {
                    labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(inse,n,braw.env$reportCounts),"'"," error"))
                    colours<-c(colours,braw.env$plotColours$infer_isigNonNull)
                  }
                if (!is.null(nse) && any(resNotNull)) {
                  labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(nse,n,braw.env$reportCounts),"'",nsLabel))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNonNull)
                }
                if (!is.null(sc) && any(resNotNull)) {
                  labels<-c(labels,paste0(braw.env$nonNull," '",reportNumber(sc,n,braw.env$reportCounts),"'"," correct"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNonNull)
                }
              },
              "nulls"={
                if (analysis$evidence$minRp!=0)
                  title<-braw.env$inactiveTitle
                else
                  title<-braw.env$nullTitle

                if (braw.env$STMethod=="dLLR") 
                  if (!is.null(isc) && any(!resNotNull)) {
                    labels<-c(labels,paste0(braw.env$Null," '",reportNumber(isc,n,braw.env$reportCounts),"'"," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_isigNull)
                  }
                if (!is.null(nsc) && any(!resNotNull)) {
                  labels<-c(labels,paste0(nlab," '",reportNumber(nsc,n,braw.env$reportCounts),"'",sLabel))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNull)
                }
                if (!is.null(se) && any(!resNotNull)) {
                  labels<-c(labels,paste0(nlab," '",reportNumber(se,n,braw.env$reportCounts),"'"," error"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNull)
                }
              },
              "rss"={
                  if (!is.null(nse)) {
                    labels<-c(labels,paste0(npad(reportNumber(nse,n,braw.env$reportCounts))," error"))
                    colours<-c(colours,braw.env$plotColours$infer_nsigNonNull)
                  }
                  if (!is.null(sc)) {
                    labels<-c(labels,paste0(npad(reportNumber(sc,n,braw.env$reportCounts))," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_sigNonNull)
                  }
                  if (!is.null(se)) {
                    labels<-c(labels,paste0(npad(reportNumber(se,n,braw.env$reportCounts))," error"))
                    colours<-c(colours,braw.env$plotColours$infer_sigNull)
                  }
                  if (!is.null(nsc)) {
                    labels<-c(labels,paste0(npad(reportNumber(nsc,n,braw.env$reportCounts))," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_nsigNull)
                  }
                },
              "p"={
                labels<-c(paste0(lb1,"sig",lb2,reportNumber(s,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns",lb2,reportNumber(ns,n,braw.env$reportCounts),""))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e1r"={
                labels<-c(paste0(lb1,"sig error",lb2,reportNumber(s,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns correct",lb2,reportNumber(ns,n,braw.env$reportCounts),""))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e2r"={
                labels<-c(paste0(lb1,"sig correct",lb2,reportNumber(s,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns error",lb2,reportNumber(ns,n,braw.env$reportCounts),""))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e1+"={
                labels<-paste0(lb1,"sig error",lb2,reportNumber(s1,n,braw.env$reportCounts),"")
                colours<-braw.env$plotColours$infer_sigC
              },
              "e2+"={
                labels<-paste0(lb1,"sig correct",lb2,reportNumber(s2,n,braw.env$reportCounts),"")
                colours<-braw.env$plotColours$infer_sigC
              },
              "e1-"={
                labels<-paste0(lb1,"ns correct",lb2,reportNumber(s1,n,braw.env$reportCounts),"")
                colours<-braw.env$plotColours$infer_nsigC
              },
              "e2-"={
                labels<-paste0(lb1,"ns error",lb2,reportNumber(s2,n,braw.env$reportCounts),"")
                colours<-braw.env$plotColours$infer_nsigC
              },
              "e1p"={
                labels<-c(paste0(lb1,"sig error",lb2,reportNumber(s,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns correct",lb2,reportNumber(ns,n,braw.env$reportCounts),""))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e2p"={
                labels<-c(paste0(lb1,"sig correct",lb2,reportNumber(s,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns error",lb2,reportNumber(ns,n,braw.env$reportCounts),""))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e1d"={
                labels<-c(paste0(lb1,"sig error",lb2,reportNumber(s1,n,braw.env$reportCounts),""),
                          paste0(lb1,"sig correct",lb2,reportNumber(s2,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns",lb2,reportNumber(ns,n,braw.env$reportCounts),"")
                          )
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e2d"={
                labels<-c(paste0(lb1,"sig correct",lb2,reportNumber(s1,n,braw.env$reportCounts),""),
                          paste0(lb1,"sig error",lb2,reportNumber(s2,n,braw.env$reportCounts),""),
                          paste0(lb1,"ns",lb2,reportNumber(ns,n,braw.env$reportCounts),"")
                )
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              }
      )
      if (!(sequence==1) && length(labels)>0) g<-addG(g,dataLegend(data.frame(names=labels,colours=colours),title=title,shape=22))
    }
    }
    
    if (!is.null(hypothesis$IV2)) {
      if(effectType=="all"){
               g<-addG(g,dataText(data.frame(x=xoff[1],y=ylim[2]-diff(ylim)*0.05),"direct",hjust=0.5,size=0.75))
               g<-addG(g,dataText(data.frame(x=xoff[2],y=ylim[2]-diff(ylim)*0.05),"unique",hjust=0.5,size=0.75))
               g<-addG(g,dataText(data.frame(x=xoff[3],y=ylim[2]-diff(ylim)*0.05),"total",hjust=0.5,size=0.75))
      } else {
        # g<-addG(g,dataText(data.frame(x=xoff,y=ylim[2]-diff(ylim)*0.05),effectType,hjust=0.5,size=0.75))
      }
    }
  }
  
  g
}

l_plot<-function(analysis,ptype=NULL,otheranalysis=NULL,orientation="vert",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  g<-r_plot(analysis,ptype,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
  g
}

p_plot<-function(analysis,ptype="p",otheranalysis=NULL,PlotScale=braw.env$pPlotScale,orientation="vert",
                 whichEffect="Main 1",effectType="all",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  g<-r_plot(analysis,ptype,PlotScale=="log10",otheranalysis,orientation=orientation,
            whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
  g
}

w_plot<-function(analysis,wtype,orientation="vert",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  g<-r_plot(analysis,wtype,braw.env$wPlotScale=="log10",orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
  g
}

n_plot<-function(analysis,ntype,orientation="vert",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  r_plot(analysis,ntype,braw.env$nPlotScale=="log10",orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
}

e2_plot<-function(analysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  if (!analysis$hypothesis$effect$world$On) {
    lambda<-brawFormat(analysis$hypothesis$effect$rIV,digits=3)
    switch (braw.env$RZ,
            "r"={
              lab<-paste0("NonNull: r[p]~ ",lambda)
            },
            "z"={
              lab<-paste0("NonNull: z[p]~ ",atanh(lambda))
            }
    )
  } else {
  distr<-tolower(analysis$hypothesis$effect$world$PDF)
  rz<-analysis$hypothesis$effect$world$RZ
  lambda<-brawFormat(analysis$hypothesis$effect$world$PDFk,digits=3)
  switch (braw.env$RZ,
          "r"={
            lab<-paste0("NonNull: r[p]~ ",distr,"(",rz,"/",lambda,")")
          },
          "z"={
            lab<-paste0("NonNull: z[p]~ ",distr,"(",rz,"/",atanh(lambda),")")
          }
  )
  
  }
  lab<-"NonNull"
  
  switch (braw.env$STMethod,
          "NHST"={
            analysis$hypothesis$effect$world$pRplus<-1
            switch(disp,
                   "e2r"={
                     g<-r_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   },
                   "e2+"={
                     g<-r_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   },
                   "e2-"={
                     g<-r_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   },
                   "e2p"={
                     g<-p_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   }
            )
            g<-addG(g,plotTitle(lab))
          },
          "sLLR"={
            g<-p_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
            g<-addG(g,plotTitle(lab))
          },
          "dLLR"={
            g<-p_plot(analysis,"e2d",otheranalysis=otheranalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
            g<-addG(g,plotTitle(lab))
          }
  )
  return(g)
}

e1_plot<-function(nullanalysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  switch (braw.env$RZ,
          "r"={
            lab<-"Null: r[p]= 0"
          },
          "z"={
            lab<-"Null: z[p]= 0"
          }
  )
  lab<-"Null"
  switch (braw.env$STMethod,
          "NHST"={
            nullanalysis$hypothesis$effect$world$pRplus<-0
            # g<-r_plot(nullanalysis,"rs_e1",otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
            switch(disp,
                   "e1r"={
                     g<-r_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   },
                   "e1+"={
                     g<-r_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   },
                   "e1-"={
                     g<-r_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   },
                   "e1a"={
                     g<-p_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
                   }
            )
            g<-addG(g,plotTitle(lab))
          },
          "sLLR"={
            g<-p_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
              g<-addG(g,plotTitle(lab))
          },
          "dLLR"={
            g<-p_plot(nullanalysis,"e1d",otheranalysis=otheranalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory,showData=showData,showYaxis=showYaxis,g=g)
            g<-addG(g,plotTitle(lab))
          }
  )
  return(g)
}

ps_plot<-function(analysis,disp,showTheory=TRUE,showLegend=FALSE,showData=TRUE,showYaxis=TRUE,g=NULL){
  
  if (is.null(analysis$hypothesis$IV2)) {
    sigs<-isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
    nulls<-abs(analysis$rpIV)<=analysis$evidence$minRp
    if ((all(nulls) || all(!nulls)) && disp=="ps") {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("p(sig)"),
                   top=1.5,orientation="horz",g=g)
      g<-addG(g,dataBar(data=data.frame(x=0,y=mean(sigs)),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    } else {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("Outcomes"),
                   top=1.5,orientation="horz",g=g)
      col0<-braw.env$plotColours$infer_nsigNonNull
      col2<-braw.env$plotColours$infer_sigNonNull
      col3<-braw.env$plotColours$infer_sigNull
      col4<-braw.env$plotColours$infer_nsigNull
      col5<-braw.env$plotColours$infer_isigNonNull
      col6<-braw.env$plotColours$infer_isigNull
      # lb0<-paste0(braw.env$nonNullNS," ~ ",brawFormat(mean(!nulls & !sigs)*100,digits=0),'~"%"')
      # lb2<-paste0(braw.env$nonNullSig," ~ ",brawFormat(mean(!nulls & sigs)*100,digits=0),'~"%"')
      # lb3<-paste0(braw.env$nullSig," ~ ",brawFormat(mean(nulls & sigs)*100,digits=0),'~"%"')
      # lb5<-paste0(braw.env$nullNS," ~ ",brawFormat(mean(nulls & !sigs)*100,digits=0),'~"%"')
      lb0<-paste0(braw.env$nonNullNS," '",reportNumber(sum(!nulls & sigs==0),length(sigs),braw.env$reportCounts),"'")
      lb2<-paste0(braw.env$nonNullSig," '",reportNumber(sum(!nulls & sigs>0),length(sigs),braw.env$reportCounts),"'")
      if (analysis$evidence$minRp!=0) {
        lb3<-paste0(braw.env$inactiveSig," '",reportNumber(sum(nulls & sigs>0),length(sigs),braw.env$reportCounts),"'")
        lb4<-paste0(braw.env$inactiveNS," '",reportNumber(sum(nulls & sigs==0),length(sigs),braw.env$reportCounts),"'")
      } else {
        lb3<-paste0(braw.env$nullSig," '",reportNumber(sum(nulls & sigs>0),length(sigs),braw.env$reportCounts),"'")
        lb4<-paste0(braw.env$nullNS," '",reportNumber(sum(nulls & sigs==0),length(sigs),braw.env$reportCounts),"'")
      }
      if (braw.env$STMethod=="dLLR") {
        lb3<-paste0("H[0]~err"," '",reportNumber(sum(nulls & sigs>0),length(sigs),braw.env$reportCounts),"'")
        lb5<-paste0("H[+]~err"," '",reportNumber(sum(!nulls & sigs<0),length(sigs),braw.env$reportCounts),"'")
      lb6<-paste0(braw.env$nullSig," '",reportNumber(sum(nulls & sigs<0),length(sigs),braw.env$reportCounts),"'")
      }
      cols<-c()
      nms<-c()
      y<-1
      if (!all(nulls)) {
        if (braw.env$STMethod=="dLLR") {
          g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col5,barwidth=0.4))
          y<-y-mean(!nulls & sigs<0)
          cols<-c(cols,col5)
          nms<-c(nms,lb5)
        }
        g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col0,barwidth=0.4))
        y<-y-mean(!nulls & sigs==0)
        g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col2,barwidth=0.4))
        y<-y-mean(!nulls & sigs>0)
        cols<-c(cols,col0,col2)
        nms<-c(nms,lb0,lb2)
      }
      if (!all(!nulls)) {
        if (braw.env$STMethod=="dLLR") {
          g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col6,barwidth=0.4))
          y<-y-mean(nulls & sigs<0)
          cols<-c(cols,col6)
          nms<-c(nms,lb6)
        }
        g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col4,barwidth=0.4))
        y<-y-mean(nulls & sigs==0)
        g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col3,barwidth=0.4))
        y<-y-mean(nulls & sigs>0)
        cols<-c(cols,col4,col3)
        nms<-c(nms,lb4,lb3)
      }       
      if (!(analysis$sequence==1))
        g<-addG(g,dataLegend(data.frame(colours=cols,names=nms),title="",shape=22))

    }
  } else {
    g<-startPlot(xlim=c(-1,3),ylim=c(0,1),
                 xticks=makeTicks(breaks=c(0,1,2),labels=c("DV~IV","DV~IV2","DV~IVxIV2")),
                 yticks=makeTicks(),ylabel=makeLabel("p(sig)"),
                 top=0,orientation="horz",g=g)
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=0,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV2,analysis$rIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=1,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIVIV2,analysis$rIVIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=2,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
  }
  return(g)
}

aic_plot<-function(analysis,disp,showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL) {
  # plotting a single result:
  
  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  n<-analysis$result$nval
  DVsd<-sd(analysis$result$dv)
  
  yaxis<-plotAxis("SEM",hypothesis,design)
  cols<-yaxis$cols
  
  if (is.null(analysis$sem)) analysis$sem<-analysis$result$sem
  
  nbar<-sum(!is.na(analysis$sem[1,1:7]))
  if (nbar==2) 
    if (analysis$hypothesis$effect$rIV!=0) correct<-2 else correct<-1
  else {
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2!=0) correct<-7
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2==0) correct<-6
    if (analysis$hypothesis$effect$rIV==0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2!=0) correct<-5
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2==0 && analysis$hypothesis$effect$rIVIV2!=0) correct<-4
    if (analysis$hypothesis$effect$rIV==0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2==0) correct<-3
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2==0 && analysis$hypothesis$effect$rIVIV2==0) correct<-2
    if (analysis$hypothesis$effect$rIV==0 && analysis$hypothesis$effect$rIV2==0 && analysis$hypothesis$effect$rIVIV2==0) correct<-1
  }
  
  sem<-analysis$sem1
  if (nrow(sem)==1) {
    range<-(max(sem[,1:nbar])-min(sem[,1:nbar]))
    lowY<-min(min(sem[,1:nbar])-range*0.5 , 1.5*n*DVsd)
    highY<-max(max(sem[,1:nbar])+range*0.25 , 3.5*n*DVsd)
    lowY<-lowY-(highY-lowY)/4
    g<-startPlot(xlim=c(0,nbar+1),ylim=c(lowY,highY),
                 yticks=makeTicks(),ylabel=makeLabel("AIC"),
                 top=FALSE,orientation="horz",g=g)
    startBar<-1
  } else {
    sem<-cbind(sem[,1:7]-sem[,1],sem[,8])
    lowY<- -1.0*analysis$design$sN*hypothesis$DV$sd
    highY<- 0.1*analysis$design$sN*hypothesis$DV$sd
    g<-startPlot(xlim=c(0,nbar),ylim=c(lowY,highY),
                 yticks=makeTicks(),ylabel=makeLabel("diff(AIC)"),
                 top=FALSE,orientation="horz",g=g)
    if (nbar==2) cols<-c(braw.env$plotColours$infer_nsigNull,braw.env$plotColours$infer_sigNonNull)
    startBar<-2
  }
  if (nrow(sem)>1) {
    markSize<-2
    shape<-22
  } else {
    markSize<-6
    shape<-21
  }
  for (ig in startBar:nbar) {
    if (ig==correct) fontface<-"bold" else fontface="plain"
    xvals<-makeFiddle(sem[,ig])
    if (any(xvals!=0))    xvals<-xvals/max(abs(xvals))*0.4
    use<-sem[,ncol(sem)]==ig
    if (startBar==2) {
      use1<-sem[,ncol(sem)]==1
      # g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use1],y=sem[use1,ig]),shape=shape,size=size,fill="grey"))
      g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use1],y=sem[use1,ig]),shape=shape,size=markSize,fill=cols[1]))
    } else {
      # g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[!use],y=sem[!use,ig]),shape=shape,size=size,fill="grey"))
      g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[!use],y=sem[!use,ig]),shape=shape,size=markSize,fill="white"))
    }
    # g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use],y=sem[use,ig]),shape=shape,size=size,fill="grey"))
    g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use],y=sem[use,ig]),shape=shape,size=markSize,fill=cols[ig]))
    if (nrow(sem)==1)
    g<-addG(g,dataText(data.frame(x=ig-startBar+1,y=min(sem[,ig])-(highY-lowY)/30),label=colnames(sem)[ig],
                       fontface=fontface,hjust=1,vjust=0.5,angle=90,size=0.6))   
  }
  return(g)
}

sem_plot<-function(analysis,disp,showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  
  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  yaxis<-plotAxis("SEM",hypothesis,design)
  cols<-yaxis$cols
  
  if (is.null(analysis$sem)) analysis$sem<-analysis$result$sem
  nulls<-abs(analysis$rp)<=analysis$evidence$minRp
  if (all(nulls) || all(!nulls)) {
      g<-startPlot(xlim=c(-2,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("SEM"),
                   top=1.5,orientation="horz",g=g)
      nbars<-sum(!is.na(analysis$sem[1,]))-1
      labels<-colnames(analysis$sem)
      proportions<-hist(analysis$sem[,8],breaks=(0:nbars)+0.5,plot=FALSE)$density
      plots<-cumsum(proportions)
      for (ig in nbars:1) {
        g<-addG(g,dataBar(data=data.frame(x=0,y=plots[ig]),fill=cols[ig],barwidth=0.4))
      if (proportions[ig]>0.02) 
        g<-addG(g,dataText(data=data.frame(x=-0.45,y=plots[ig]-proportions[ig]/2),labels[ig],
                           hjust=1,vjust=0.5,size=0.5,colour='white'))
    }
    } else {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("SEM"),
                   top=1.5,orientation="horz",g=g)
      nbars<-sum(!is.na(analysis$sem[1,]))-1
      labels<-c(
        paste0(braw.env$Null,"('",colnames(analysis$sem)[1:2],"')"),
        paste0(braw.env$nonNull,"('",colnames(analysis$sem)[2:1],"')")
        )
      proportions<-c(
        hist(analysis$sem[nulls,8],breaks=(0:nbars)+0.5,plot=FALSE)$counts,
        rev(hist(analysis$sem[!nulls,8],breaks=(0:nbars)+0.5,plot=FALSE)$counts)
        )
      proportions<-proportions/sum(proportions)
      plots<-cumsum(proportions)
      
      cols<-c(
        braw.env$plotColours$infer_nsigNull,
        braw.env$plotColours$infer_sigNull,
        braw.env$plotColours$infer_sigNonNull,
        braw.env$plotColours$infer_nsigNonNull
      )
      for (ig in length(plots):1) {
        g<-addG(g,dataBar(data=data.frame(x=0,y=plots[ig]),fill=cols[ig],barwidth=0.4))
      }
      g<-addG(g,dataLegend(data.frame(colours=rev(cols),names=rev(labels)),title="",shape=22))
    }
  return(g)
}

var_plot<-function(analysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,showData=TRUE,showLegend=FALSE,showYaxis=TRUE,g=NULL){
  g<-r_plot(analysis,showType=disp,showTheory=showTheory,showData=showData,showLegend=FALSE,showYaxis=showYaxis,g=g)
}
