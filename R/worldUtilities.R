
# below we use (npops-1)/6 as an integer

zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

rdens2zdens<-function(rdens,rvals){
  rdens*(1-rvals^2)
}

zSamplingDistr<-function(zvals,Z,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-Z)/s)^2)
}

rSamplingDistr<-function(rvals,R,n){
  # map to Fisher-z
  zvals<-atanh(rvals)
  Z<-atanh(R)
  zdens<-zSamplingDistr(zvals,Z,n)
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
  if (is.null(world)) {world<-list(populationPDF="Uniform",populationRZ="r",populationPDFk<-0)}
  k<-world$populationPDFk
  z_s<-atanh(r_s)
  if (is.na(z_s)) {return(NA)}
  switch(world$populationPDF,
         "Uniform"={mlEst<-z_s},
         "Single"={mlEst<-z_s},
         "Gauss"={mlEst<-z_s*k^2*(n-3)/(k^2*(n-3) + 1)},
         "Exp"={
           overEst<-1/k/(n-3)
           if (z_s<0) {
             mlEst<-min(z_s+overEst,0)
           } else {
             mlEst<-max(z_s-overEst,0)
           }
           }
         )
  tanh(mlEst)
}
  
getRList<-function(world) {
  npops=20*6+1 # 2*3*4*5+1

  if (is.null(world)) {
    return(list(pRho=0,pRhogain=1)  )
  }
  if (!world$worldOn) {
    world$populationPDF="Single"
    world$populationRZ="r"
  }
  
  switch (world$populationPDF,
          "Single"={
            if (world$populationRZ=="r") {
              pRho<-world$populationPDFk
            } else {
              pRho<-tanh(world$populationPDFk)
            }
            pRhogain<-1
          },
          "Double"={
            if (world$populationRZ=="r") {
              pRho<-c(-1,1)*world$populationPDFk
            } else {
              pRho<-c(-1,1)*tanh(world$populationPDFk)
            }
            pRhogain<-c(0.5,0.5)
          },
          {
            pRho<-seq(-1,1,length=npops)*braw.env$r_range
            pRhogain<-rPopulationDist(pRho,world)
          }
  )
  
  # if (world$populationNullp>0) {
  #   pRho<-c(pRho,0)
  #   pRhogain<-c(pRhogain/sum(pRhogain)*(1-world$populationNullp),world$populationNullp)
  # } else {
  #   pRho<-c(pRho,0)
  #   pRhogain<-c(pRhogain/sum(pRhogain),0)
  # }
  
  list(pRho=pRho,pRhogain=pRhogain)  
}

getNDist<-function(design,world=NULL,logScale=FALSE,sigOnly=FALSE,HQ=FALSE) {
  if (HQ) npt<-101 else npt=21
  nmax<-5
  if (logScale) {
    nvals<-10^seq(log10(braw.env$minN),log10(nmax*design$sN),length.out=npt)
  }else{
    nvals<-braw.env$minN+seq(0,nmax*design$sN,length.out=npt)
  }
  
  n<-design$sN
  if (design$sNRand) {
    ng<-dgamma(nvals-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
  } else {
    ng<-nvals*0
    use<-which.min(abs(nvals-design$sN))
    ng[use]<-1
  }
  if (sigOnly) {
    nsig<-ng
    pR<-getRList(world)
    pR$pRhogain<-pR$pRhogain/sum(pR$pRhogain)
    for (ni in 1:length(nvals)) {
      psig<-sum(rn2w(pR$pRho,nvals[ni])*pR$pRhogain)
      nsig[ni]<-nsig[ni]*psig
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
  if (design$Replication$On) {
    if (HQ) npt<-101 else npt=21
    nmax<-5
    nvals<-braw.env$minN+seq(0,nmax*design$sN,length.out=npt)
    design$Replication$On<-FALSE
    ndens<-fullRSamplingDist(nvals,world=world,design=design,"nw",logScale=FALSE,sigOnly=FALSE)
    return(list(nvals=nvals,ndens=ndens,ndensSig=ndens))
  } else {
    if (!design$sNRand) {
      return(list(nvals=design$sN,ndens=1,ndensSig=1))
    }
    return(getNDist(design,world=NULL,logScale=FALSE,sigOnly=FALSE,HQ=HQ))
  }
}

rPopulationDist<-function(rvals,world) {
  k<-world$populationPDFk
  switch (paste0(world$populationPDF,"_",world$populationRZ),
          "Single_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1
          },
          "Single_z"={
            rdens<-rvals*0
            rdens[which.min(abs(tanh(k)-rvals))]<-1
          },
          "Double_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1/2
            rdens[which.min(abs(k+rvals))]<-1/2
          },
          "Double_z"={
            rdens<-rvals*0
            rdens[which.min(abs(tanh(k)-rvals))]<-1/2
            rdens[which.min(abs(tanh(k)+rvals))]<-1/2
          },
          "Uniform_r"={
            rdens<-rvals*0
            rdens[abs(rvals)<0.75]<-0.5
          },
          "Uniform_z"={
            zvals<-atanh(rvals)
            zdens<-zvals*0+0.5
            rdens<-zdens2rdens(zdens,rvals)
          },
          "Exp_r"={
            rdens<-exp(-abs(rvals)/k)/(2*k)
          },
          "Exp_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-abs(zvals)/k)/(2*k)
            rdens<-zdens2rdens(zdens,rvals)
          },
          "Gauss_r"={
            rdens<-exp(-0.5*(abs(rvals)/k)^2)/k/sqrt(2*pi)
          },
          "Gauss_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-0.5*(abs(zvals)/k)^2)/k/sqrt(2*pi)
            rdens<-zdens2rdens(zdens,rvals)
          }
  )
rdens
}

fullPSig<-function(world,design,HQ=FALSE,alpha=braw.env$alphaSig) {

  # distribution of population effect sizes
  pR<-getRList(world)
  rvals<-pR$pRho
  rdens<-pR$pRhogain
  if (length(rvals)>1) rdens<-rdens*diff(rvals[1:2])
  # next line is precautionary
  rdens<-rdens/sum(rdens)
  if (world$worldOn && world$populationNullp>0) {
    rvals<-c(rvals,0)
    rdens<-c(rdens*(1-world$populationNullp),world$populationNullp)
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

fullRSamplingDist<-function(vals,world,design,doStat="rs",logScale=FALSE,sigOnly=FALSE,HQ=FALSE,separate=FALSE,quantiles=NULL) {
  # sampling distribution from specified populations (pRho)
  if (is.null(vals)) 
    vals<-seq(-1,1,length=braw.env$worldNPoints)*braw.env$r_range

  # distribution of population effect sizes
  pR<-getRList(world)
  rvals<-pR$pRho
  rdens<-pR$pRhogain
  if (length(rvals)>1) rdens<-rdens*diff(rvals[1:2])
  if (!world$worldOn) world$populationNullp<-0
  # if (any(rvals==0)) {
  #   use<-which(rvals==0)
  #   rdens<-rdens/sum(rdens)*(1-world$populationNullp)
  #   rdens[use]<-rdens[use]+world$populationNullp
  # } else {
    rvals<-c(rvals,0)
    rdens<-c(rdens/sum(rdens)*(1-world$populationNullp),world$populationNullp)
  # }
  
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
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
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
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(braw.env$alphaSig/2)+zp*sqrt(nvals[ni]-3)) + pnorm(qnorm(braw.env$alphaSig/2)-zp*sqrt(nvals[ni]-3))
                  addition<-rPopulationDist(rp,world)
                  if (sum(addition>0)>1) {
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
        if (sigOnly) {
          critR<-tanh(qnorm(1-braw.env$alphaSig/2,0,1/sqrt(nvals[ni]-3)))
          addition[abs(rp)<critR]<-0
        }
        d<-d+addition
      }
      d<-d/sum(d1,na.rm=TRUE)*rdens[ei]
      sourceSampDens_r<-rbind(sourceSampDens_r,d)
  }

  if (separate) {
    r<-sourceSampDens_r
    rn<-nrow(r)
    use<-which(rvals==0)
    if (rn==2) 
      return(list(vals=rvals[1:(rn-1)],dens=colSums(sourceSampDens_r,na.rm=TRUE),
                  densPlus=rbind(r[1:(rn-1),]),densNull=r[rn,]))
    else 
      
      return(list(vals=rvals[1:(rn-1)],dens=colSums(sourceSampDens_r,na.rm=TRUE),
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

