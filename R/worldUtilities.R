
# below we use (npops-1)/6 as an integer

nDistrDens<-function(nvals,design) {
  switch(design$sNRandDist,
         "Gamma"={
           return(dgamma(nvals-braw.env$minN,
                         shape=design$sN/design$sNRandSD,
                         scale=design$sNRandSD)
           )
         },
         "Gauss"={
           return(dnorm(nvals,braw.env$minN,design$sNRandSD))
         },
         "Exp"={
           return(dexp(nvals-braw.env$minN,1/design$sNRandSD))
         },
         "Uniform"={
           return(nvals>=braw.env$minN & nvals<=braw.env$maxN)
         }
  )
}

nDistrRand<-function(nSamples,design=braw.def$design){
  switch(design$sNRandDist,
         "Gamma"={
           return(
             round(braw.env$minN+rgamma(nSamples,
                         shape=(design$sN-braw.env$minN)/design$sNRandSD,
                         scale=design$sNRandSD)
             )
           )
         },
         "Gauss"={
           return(round(braw.env$minN+abs(rnorm(nSamples,0,design$sNRandSD))))
         },
         "Exp"={
           return(round(braw.env$minN+abs(rexp(nSamples,1/design$sNRandSD))))
         },
         "Uniform"={
           return(round(runif(nSamples,braw.env$minN,braw.env$maxN)))
         }
  )
  braw.env$minN+rgamma(nSamples,shape=design$sN/design$sNRandSD,scale=design$sNRandSD)
}

zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

rdens2zdens<-function(rdens,rvals){
  rdens*(1-rvals^2)
}

rdens2ddens<-function(rdens,dvals) {
  rdens*(4/(dvals^2+4)/sqrt(dvals^2+4))
}

zSamplingDistr<-function(zvals,Z,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-Z)/s)^2)
}

rSamplingDistr<-function(rvals,R,n,sigOnly=0){
  # map to Fisher-z
  zvals<-atanh(rvals)
  Z<-atanh(R)
  zdens<-zSamplingDistr(zvals,Z,n)
  if (sigOnly>0) {
    zcrit<-atanh(p2r(braw.env$alphaSig,n,1))
    if (length(rvals)==1) {
      gain<-1-sigOnly*(pnorm(zcrit,Z,1/sqrt(n-3))-pnorm(-zcrit,Z,1/sqrt(n-3)))
    } else {
      g<-sum(zdens)
      zdens[abs(zvals)<zcrit]<-zdens[abs(zvals)<zcrit]*(1-sigOnly)
      gain<-sum(zdens)/g
    }
    zdens<-zdens/gain
  }
  zdens2rdens(zdens,rvals)
}

pSamplingDistr<-function(pvals,R,n) {
    Z<-atanh(R)*sqrt(n-3)
    z<-qnorm(pvals/2)
    dpdz<-exp(-0.5*(z)^2)
    dens<-(exp(-0.5*(z+Z)^2)+exp(-0.5*(z-Z)^2))/2/dpdz
    min_p<-min(pvals)
    psum<-pnorm(qnorm(min_p/2),Z)+pnorm(qnorm(min_p/2),-Z)
    gain<-(1-psum)/sum(dens*c(0,diff(log10(pvals))))
    dens*gain
}

wSamplingDistr<-function(wvals,R,n,alpha=braw.env$alphaSig) {
  # we need to do it this way, alas
    z<-seq(0,10,0.05)
    wz<-pnorm(qnorm(alpha/2),z)+pnorm(qnorm(alpha/2),-z)
    Z <- atanh(R)*sqrt(n-3)
    dwdz<-sqrt(n-3)/(2*pi)*(exp(-0.5*(z+qnorm(alpha/2))^2)-exp(-0.5*(z-qnorm(alpha/2))^2))
    densW<-(exp(-0.5*(z-Z)^2)+exp(-0.5*(z+Z)^2))/dwdz
    approx(wz,densW,wvals)$y
}

rSamp2Pop<-function(r_s,n,world=NULL) {
  if (is.null(world)) {world<-list(PDF="Uniform",RZ="r",PDFk<-0)}
  k<-world$PDFk
  z_s<-atanh(r_s)
  if (all(is.na(z_s))) {return(NA)}
  switch(world$PDF,
         "Uniform"={mlEst<-z_s},
         "Single"={mlEst<-z_s},
         "Gauss"={mlEst<-z_s*k^2*(n-3)/(k^2*(n-3) + 1)},
         "Exp"={
           overEst<-1/k/(n-3)
           mlEst<-z_s*0
           use<- z_s<(-overEst)
           if (any(use)) mlEst[use]<-z_s[use]+overEst
           use<- z_s>(overEst)
           if (any(use)) mlEst[use]<-z_s[use]-overEst
           #  if (z_s<0) {
           #   mlEst<-min(z_s+overEst,0)
           # } else {
           #   mlEst<-max(z_s-overEst,0)
           # }
           },
         "Gamma"={mlEst<-z_s},
         "GenExp"={mlEst<-z_s}
  )
  tanh(mlEst)
}
  
getRList<-function(world,addNulls=FALSE,HQ=FALSE) {
  if (HQ) npops=50*6+1
  else    npops=20*6+1 # 2*3*4*5+1

  if (is.null(world)) {
    return(list(pRho=0,pRhogain=1)  )
  }
  if (!world$On) {
    world$PDF="Single"
    world$RZ="r"
  }
  
  switch (world$PDF,
          "Single"={
            if (world$RZ=="r") {
              pRho<-world$PDFk
            } else {
              pRho<-tanh(world$PDFk)
            }
            pRhogain<-1
          },
          "Double"={
            if (world$RZ=="r") {
              pRho<-c(-1,1)*world$PDFk
            } else {
              pRho<-c(-1,1)*tanh(world$PDFk)
            }
            pRhogain<-c(0.5,0.5)
          },
          {
            switch(braw.env$RZ,
                   "r"=pRho<-seq(-1,1,length=npops)*braw.env$r_range,
                   "z"=pRho<-tanh(seq(-1,1,length=npops)*braw.env$z_range)
            )
            pRhogain<-rPopulationDist(pRho,world)
          }
  )
  
  if (addNulls)
  if (world$pRPlus<1) {
    pRho<-c(0,pRho)
    pRhogain<-c(1-world$pRPlus,pRhogain/sum(pRhogain)*world$pRPlus)
  } else {
    pRho<-c(pRho,0)
    pRhogain<-c(pRhogain/sum(pRhogain),0)
  }
  
  list(pRho=pRho,pRhogain=pRhogain)  
}

getNDist<-function(design,world=NULL,logScale=FALSE,sigOnly=0,HQ=FALSE) {
  if (HQ) npt<-1001 else npt=21
  nmax<-5
  if (logScale) {
    nvals<-10^seq(log10(braw.env$minN),log10(nmax*design$sN),length.out=npt)
  }else{
    nvals<-braw.env$minN+seq(0,nmax*design$sN,length.out=npt)
  }
  
  n<-design$sN
  if (design$sNRand) {
    ng<-nDistrDens(nvals,design)
  } else {
    ng<-nvals*0
    use<-which.min(abs(nvals-design$sN))
    ng[use]<-1
  }
  if (sigOnly>0) {
    nsig<-ng 
    pR<-getRList(world,HQ=HQ)
    pR$pRhogain<-pR$pRhogain/sum(pR$pRhogain)
    for (ni in 1:length(nvals)) {
      psig<-sum(rn2w(pR$pRho,nvals[ni])*pR$pRhogain)
      nsig[ni]<-nsig[ni]*psig+(1-psig)*(1-sigOnly)
    }
  } else {
    nsig<-NA
  }
  if (logScale) {
    ng<-ng*nvals
    nsig<-nsig*nvals
  }
  list(nvals=nvals,ndens=ng,ndensSig=nsig)
}

getNList<-function(design,world,HQ=FALSE) {
    if (!design$sNRand) {
      return(list(nvals=design$sN,ndens=1,ndensSig=1))
    }
    return(getNDist(design,world=NULL,logScale=FALSE,sigOnly=0,HQ=HQ))
}

rRandomValue<-function(world=braw.def$hypothesis$effect$world,ns) {
  k<-world$PDFk
  mu<-world$PDFmu
  sh<-world$PDFs
  rangeMax<-braw.env$r_range
  rangeMax<-0.9999999
  if (world$RZ=="z") rangeMax<-atanh(rangeMax)
  switch (world$PDF,
          "Single"={pops<-rep(k,ns)},
          "Double"={pops<-rep(k,ns)},
          "Uniform"={pops<-runif(ns,min=0,max=rangeMax)},
          "Exp"={pops<-rexp(ceil(5*ns),rate=1/k)},
          "Gauss"={pops<-rnorm(ceil(5*ns),mean=mu,sd=k)},
          "Gamma"={pops<-rgamma(ceil(5*ns),shape=sh,rate=sh/k)},
          "GenExp"={
            zi<-seq(-rangeMax,rangeMax,0.001)
            zd<-cumsum(GenExpSamplingPDF(zi,k,0,sh))
            zd<-(zd-min(zd))/(max(zd)-min(zd))
            pops<-approx(zd,zi,runif(ns,0,1))$y
            }
  )
  if (world$RZ=="z") pops<-tanh(pops)
  if (world$PDF!="Single") pops<-pops*sign(rnorm(length(pops)))
  pops<-pops[abs(pops)<1]
  if (length(pops)>ns) {
    pops<-pops[1:ns]
  }
  
  popsOld<-pops
  if (world$pRPlus<1) {
    change<-rand(length(pops),1)>=world$pRPlus
    pops[change]<-0
  }
  return(list(old=popsOld,use=pops))
}

rPopulationDist<-function(rvals,world) {
  if (world$PDFsample) {
    mn<-world$PDFsamplemn
    sd<-world$PDFsamplesd
    rdens1<-rSamplingDistr(mn,rvals,1/sd^2+3,sigOnly=world$PDFsamplebias)
  } else rdens1<-1
  k<-world$PDFk
  mu<-world$PDFmu
  sh<-world$PDFs
  if (world$RZ=="z") rvals<-atanh(rvals)
  rdens<-rvals*0
  switch (world$PDF,
          "Single"={rdens[which.min(abs(k-rvals))]<-1 },
          "Double"={ rdens[c(which.min(abs(k-rvals)),which.min(abs(k+rvals)))]<-1/2},
          "Uniform"={rdens<-rdens+0.5},
          "Exp"={rdens<-dexp(abs(rvals),rate=1/k)},
          "Gauss"={rdens<-dnorm(rvals,mean=mu,sd=k)},
          "Gamma"={rdens<-dgamma(abs(rvals),shape=sh,rate=sh/k)},
          "GenExp"={rdens<-GenExpSamplingPDF(rvals,k,sigma=0,sh)}
  )
if (world$RZ=="z") rdens<-zdens2rdens(rdens,tanh(rvals))
return(rdens*rdens1)
}

fullPSig<-function(world,design,HQ=FALSE,alpha=braw.env$alphaSig) {

  # distribution of population effect sizes
  pR<-getRList(world,HQ=HQ)
  rvals<-pR$pRho
  rdens<-pR$pRhogain
  if (length(rvals)>1) rdens<-rdens*diff(rvals[1:2])
  # next line is precautionary
  rdens<-rdens/sum(rdens)
  if (world$On && world$pRPlus<1) {
    rvals<-c(rvals,0)
    rdens<-c(rdens*(world$pRPlus),1-world$pRPlus)
  }
  
  # distribution of sample sizes
  ndist<-getNList(design,world,HQ=HQ)
  nvals<-ndist$nvals
  ndens<-ndist$ndens
  if (length(nvals)>1) ndens<-ndens*c(diff(nvals),0)
  pSig<-0
  for (ei in 1:length(rvals)){
    # for (ni in 1:length(nvals)) {
    #   Z<-atanh(rvals[ei])*sqrt(nvals[ni]-3)
    #   thisPSig<-pnorm(qnorm(alpha/2),Z)+pnorm(qnorm(alpha/2),-Z)
    #   pSig<-pSig+thisPSig*rdens[ei]*ndens[ni]
    # }
      Z<-atanh(rvals[ei])*sqrt(nvals-3)
      thisPSig<-pnorm(qnorm(alpha/2),Z)+pnorm(qnorm(alpha/2),-Z)
      pSig<-pSig+sum(thisPSig*ndens)*rdens[ei]
  }
  return(pSig)
}

fullRSamplingDist<-function(vals,world,design,doStat="rs",logScale=FALSE,sigOnly=0,sigOnlyCompensate=FALSE,HQ=FALSE,separate=FALSE,quantiles=NULL) {
  # sampling distribution from specified populations (pRho)
  if (is.null(vals)) 
    vals<-seq(-1,1,length=braw.env$worldNPoints)*braw.env$r_range

  # distribution of population effect sizes
  if (!is.null(world$On)) pR<-getRList(world,HQ=HQ)
  else pR<-world
  rvals<-pR$pRho
  rPopdens<-pR$pRhogain
  if (length(rvals)>1) rPopdens<-rPopdens*diff(rvals[1:2])
  if (!world$On) world$pRPlus<-1
  if (!is.element(world$PDF,c("sample"))) {
  rvals<-c(rvals,0)
  rPopdens<-c(rPopdens/sum(rPopdens)*(world$pRPlus),1-world$pRPlus)
  }

  # distribution of sample sizes
  ndist<-getNList(design,world,HQ=HQ)
  nvals<-ndist$nvals
  ndens<-ndist$ndens
  if (length(nvals)>1) ndens<-ndens*c(diff(nvals),0)

  sourceSampDens_r<-c()
  for (ei in 1:length(rvals)){
      d<-0
      d1<-0
      for (ni in 1:length(nvals)) {
        switch (doStat,
                "rs"={
                  rp<-vals
                  addition<-rSamplingDistr(vals,rvals[ei],nvals[ni])
                  addition<-addition*(vals[2]-vals[1])
                },
                "re"={
                  rp<-vals
                  addition<-rSamplingDistr(vals+rvals[ei],rvals[ei],nvals[ni])
                  addition<-addition*(vals[2]-vals[1])
                },
                "p"={
                  rp<-tanh(pn2z(vals,nvals[ni]))
                  addition<-pSamplingDistr(vals,rvals[ei],nvals[ni])
                  addition<-addition*(vals[2]-vals[1])
                },
                "ws"={
                  rp<-tanh(wn2z(vals,nvals[ni]))
                  addition<-wSamplingDistr(vals,rvals[ei],nvals[ni])
                  addition<-addition*(vals[2]-vals[1])
                },
                "log(lrs)"={
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])+
                            rSamplingDistr(-rp,rvals[ei],nvals[ni])
                  dzs<-vals*(nvals[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                },
                "log(lrd)"={ #XXXXXXXX
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(nvals[ni]-3)))
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])+
                            rSamplingDistr(-rp,rvals[ei],nvals[ni])
                  dzs<-vals*(nvals[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                },
                "nw"={ 
                  zp<-(qnorm(0.8)-qnorm(braw.env$alphaSig))/sqrt(vals-3)
                  rp<-tanh(zp)
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])+
                            rSamplingDistr(-rp,rvals[ei],nvals[ni])
                  dznw<- zp/(vals-3)/2
                  addition<-addition*dznw*(1-rp^2)
                },
                "wp"={
                  rp<-seq(0,1,length.out=1001)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(braw.env$alphaSig/2)+zp*sqrt(nvals[ni]-3)) + pnorm(qnorm(braw.env$alphaSig/2)-zp*sqrt(nvals[ni]-3))
                  addition<-rPopulationDist(rp,world)
                  if (sum(addition>0,na.rm=TRUE)>1) {
                  dwz<-dnorm(zp,qnorm(braw.env$alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3)) -
                    dnorm(zp,-qnorm(braw.env$alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                  }
                }
        )
        if (logScale) addition<-addition*vals
        addition<-addition*ndens[ni]
        d1<-d1+addition
        if (sigOnly>0) {
          critR<-tanh(qnorm(1-braw.env$alphaSig/2,0,1/sqrt(nvals[ni]-3)))
          addition[abs(rp)<critR]<-addition[abs(rp)<critR]*(1-sigOnly)
          if (sigOnlyCompensate) addition<-addition/sum(addition)
        }
        d<-d+addition
      }
      d<-d/sum(d1,na.rm=TRUE)*rPopdens[ei]
      sourceSampDens_r<-rbind(sourceSampDens_r,d)
  }

  if (separate) {
    r<-sourceSampDens_r
    rn<-nrow(r)
    use<-which(rvals==0)
    if (rn==2) 
      return(list(vals=rvals[1:(rn-1)],dens=colSums(r,na.rm=TRUE),
                  densPlus=rbind(r[1:(rn-1),]),densNull=r[rn,]))
    else 
      return(list(vals=rvals[1:(rn-1)],dens=colSums(r,na.rm=TRUE),
                  densPlus=r[1:(rn-1),],densNull=r[rn,]))
  } else {
    r<-colSums(sourceSampDens_r,na.rm=TRUE)
    
    if (!is.null(quantiles)) {
      r[is.na(r)]<-0
      cs<-cumsum(r)
      use<-!duplicated(cs)
      if (sum(use)<2) {
        print(use)
      }
      vals<-vals+c(diff(vals),0)/2
      return(approx(cs[use],vals[use],xout=quantiles)$y)
    }
  }
  return(r)
}

