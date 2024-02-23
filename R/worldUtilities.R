
# below we use (npops-1)/6 as an integer

zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

rdens2zdens<-function(rdens,rvals){
  rdens*(1-rvals^2)
}

zSamplingDistr<-function(zvals,zmu,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-zmu)/s)^2)
}

zPopulationDist<-function(zvals,world){
  k<-world$populationPDFk
  switch (paste0(world$populationPDF,"_",world$populationRZ),
          "Single_r"={
            rvals<-tanh(zvals)
            zdens<-rvals*0
            zdens[which.min(abs(k-rvals))]<-1
          },
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]<-1
          },
          "Double_r"={
            rvals<-tanh(zvals)
            zdens<-rvals*0
            zdens[which.min(abs(k-rvals))]<-1/2
            zdens[which.min(abs(k+rvals))]<-1/2
          },
          "Double_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]<-1/2
            zdens[which.min(abs(k+zvals))]<-1/2
          },
          "Uniform_r"={
            rvals<-tanh(zvals)
            zdens<-rdens2zdens(rvals*0+1,rvals)
          },
          "Uniform_z"={
            zdens<-zvals*0+1
          },
          "Exp_r"={
            rvals<-tanh(zvals)
            zdens<-rdens2zdens(exp(-abs(rvals)/k),rvals)
          },
          "Exp_z"={
            zdens<-exp(-abs(zvals)/k)
          },
          "Gauss_r"={
            rvals<-tanh(zvals)
            zdens<-rdens2zdens(exp(-0.5*(abs(rvals)/k)^2),rvals)
          },
          "Gauss_z"={
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
          }
  )
  zdens
}

rSamplingDistr<-function(rvals,rmu,n){
  # map to Fisher-z
  zvals<-atanh(rvals)
  zmu<-atanh(rmu)
  zdens<-zSamplingDistr(zvals,zmu,n)
  zdens2rdens(zdens,rvals)
}


zpopDistr<-function(zvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(atanh(k)-zvals))]<-1
            zdens
          },
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]=1
            zdens
          },
          "Uniform_r"={
            zdens<-atanh(zvals)*0+1
            rdens2zdens(zdens,zvals)
          },
          "Uniform_z"={
            zdens<-zvals*0+1
            zdens
          },
          "Exp_r"={
            exp(-abs(tanh(zvals)/k))
          },
          "Exp_z"={
            zdens<-exp(-abs(zvals)/k)
          },
          "Gauss_r"={
            exp(-0.5*(abs(atanh(zvals))/k)^2)
          },
          "Gauss_z"={
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
          }
  )
}
rpopDistr<-function(rvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
          "Single_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1
            rdens
          },
          "Single_z"={
            rdens<-rvals*0
            rdens[which.min(abs(tanh(k)-rvals))]<-1
            rdens
          },
          "Double_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1/2
            rdens[which.min(abs(k+rvals))]<-1/2
            rdens
          },
          "Double_z"={
            rdens<-rvals*0
            rdens[which.min(abs(tanh(k)-rvals))]<-1/2
            rdens[which.min(abs(tanh(k)+rvals))]<-1/2
            rdens
          },
          "Uniform_r"={
            rvals*0+1
          },
          "Uniform_z"={
            zvals<-atanh(rvals)
            zdens<-zvals*0+1
            zdens<-zdens2rdens(zdens,rvals)
            zdens
          },
          "Exp_r"={
            exp(-abs(rvals)/k)
          },
          "Exp_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-abs(zvals)/k)
            zdens2rdens(zdens,rvals)
          },
          "Gauss_r"={
            exp(-0.5*(abs(rvals)/k)^2)
          },
          "Gauss_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
            zdens2rdens(zdens,rvals)
          }
  )
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
  
populationDensityFunction<-function(rpw,possible){
  if (possible$type=="Populations") {
    switch (possible$UsePrior,
            "none" ={Prior<-list(populationPDF="Uniform",populationRZ=braw.env$RZ,populationPDFk=0,populationNullp=0)},
            "world"={Prior<-possible$world},
            "prior"={Prior<-possible$prior}
            )
  } else {
    Prior<-possible$world
  }
  rpw_dens<-rpopDistr(rpw,Prior$populationPDF,Prior$populationRZ,Prior$populationPDFk)
  rpw_dens<-rpw_dens*(1-Prior$populationNullp)
  if ((Prior$populationPDF=="Single") && Prior$populationNullp>0) {
    use<-which.min(abs(rpw-0))
    rpw_dens[use]<-Prior$populationNullp
  }
  rpw_dens    
}

get_pRho<-function(world,by="r",viewRZ="r") {
  npops=20*6+1 # 2*3*4*n+1
  
  if (is.null(world)) {
    return(list(pRho=0,pRhogain=1)  )
  }
  if (!world$worldOn) {
    world$populationPDF="Single"
    world$populationRZ="r"
  }
  if (by=="z") {
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
              pRho<-seq(-1,1,length=npops)*braw.env$z_range
              pRhogain<-zPopulationDist(pRho,world)
            }
    )
    switch (viewRZ,
            "r" ={
              pRho<-tanh(pRho)
              pRhogain<-zdens2rdens(pRhogain,pRho)
            },
            "z" ={
            }
    )
    
  } else {
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
              pRhogain<-zPopulationDist(pRho,world)
            }
    )
    switch (viewRZ,
            "r" ={
            },
            "z" ={
              pRho<-atanh(pRho)
              pRhogain<-rdens2zdens(pRhogain,pRho)
            }
    )
  }
  # if (world$populationNullp>0) {
  #   pRho<-c(pRho,0)
  #   pRhogain<-c(pRhogain/sum(pRhogain)*(1-world$populationNullp),world$populationNullp)
  # } else {
  #   pRho<-c(pRho,0)
  #   pRhogain<-c(pRhogain/sum(pRhogain),0)
  # }
  
  list(pRho=pRho,pRhogain=pRhogain)  
}

zSampleDist<-function(zs,pRho,pRhogain,source,design,possible) {
  # sampling distributions from specified populations (pRho)
  n<-design$sN
  ndist<-getNDist(design,source,sigOnly=FALSE)
  nis<-ndist$nvals
  ndens<-ndist$ndens
  if (length(nis)>1) ndens<-ndens*c(diff(nis),0)
  
  Dens_z<-matrix(nrow=length(pRho),ncol=length(zs))
  
  for (ei in 1:length(pRho)){
      dplus<-0
      g<-0
      for (ni in 1:(length(nis)-1)) {
        d1<-zSamplingDistr(zs,pRho[ei],nis[ni])
        if (possible$sigOnly) {
          crit_z<-qnorm(0.975,0,1/sqrt(nis[ni]-3))
          d1[abs(zs)<crit_z]<-0
        }
        dplus<-dplus+d1*ndens[ni]
        g<-g+ndens[ni]
      }
      dplus<-dplus/g
      Dens_z[ei,]<-dplus*pRhogain[ei]
    # print(sum(Dens_z[ei,])/pRhogain[ei])
  }

  # and sum of sampling distributions
    Dens_z_plus<-colSums(Dens_z)/sum(pRhogain)

    dnull<-0
    g<-0
    for (ni in 1:(length(nis)-1)) {
      d0<-zSamplingDistr(zs,0,nis[ni])
      if (possible$sigOnly) {
        crit_z<-qnorm(0.975,0,1/sqrt(nis[ni]-3))
        d0[abs(rs)<crit_z]<-0
      }
      dnull<-dnull+d0*ndens[ni]
      g<-g+ndens[ni]
    }
    dnull<-dnull/g
    Dens_z_null<-dnull
    
  list(Dens_z=Dens_z,Dens_z_plus=Dens_z_plus,Dens_z_null=Dens_z_null)
}

getNDist<-function(design,world=NULL,logScale=FALSE,sigOnly=FALSE,HQ=FALSE,asList=FALSE) {
  if (asList && !design$sNRand) {
    return(list(nvals=design$sN,ndens=1,ndensSig=1))
  }
    if (HQ) nmult<-5 else nmult=1
    nmax<-5
    if (logScale) {
      nvals<-10^seq(log10(braw.env$minN),log10(nmax*design$sN),length.out=(braw.env$nNpoints-1)*nmult+1)
    }else{
      nvals<-braw.env$minN+seq(0,nmax*design$sN,length.out=(braw.env$nNpoints-1)*nmult+1)
    }

    n<-design$sN
    if (design$sNRand) {
      ng<-dgamma(nvals-braw.env$minN,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
    } else {
      ng<-nvals*0
      use<-which.min(abs(nvals-design$sN))
      ng[use]<-1
    }
    if (sigOnly) {
      nsig<-ng
      pR<-get_pRho(world,by="r",viewRZ="r")
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

fullRPopulationDist<-function(rvals,world) {
  rpopDistr(rvals,world$populationPDF,world$populationRZ,world$populationPDFk)
}

fullRSamplingDist<-function(vals,world,design,doStat="r",logScale=FALSE,sigOnly=FALSE,HQ=FALSE,separate=FALSE) {
  # sampling distribution from specified populations (pRho)
  if (is.null(vals)) {
    vals<-seq(-1,1,length=braw.env$worldNPoints)*braw.env$r_range
  }
  
  # distribution of population effect sizes
  # we are going to do this in z
  pR<-get_pRho(world,by="r",viewRZ="r")
  rvals<-pR$pRho
  rdens<-pR$pRhogain
  if (world$worldOn && world$populationNullp>0) {
    rvals<-c(rvals,0)
    rdens<-c(rdens/sum(rdens)*(1-world$populationNullp),world$populationNullp)
  }

  # distribution of sample sizes
  ndist<-getNDist(design,HQ=HQ,asList=TRUE)
  nvals<-ndist$nvals
  ndens<-ndist$ndens
  if (length(nvals)>1) ndens<-ndens*c(diff(nvals),0)
  
  sourceSampDens_r<-c()
  for (ei in 1:length(rvals)){
      d<-0
      d1<-0
      for (ni in 1:length(nvals)) {
        switch (doStat,
                "r"={
                  rp<-vals
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])
                },
                "p"={
                  rp<-tanh(qnorm(1-vals/2)/sqrt(nvals[ni]-3))
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])+
                            rSamplingDistr(-rp,rvals[ei],nvals[ni])
                  dzp<-exp(-erfcinv(vals)^2)
                  a<-addition[1]
                  addition<-addition/dzp*(1-rp^2)
                  addition[1]<-a
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
                "w"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(braw.env$alphaSig/2)+zp*sqrt(nvals[ni]-3)) + pnorm(qnorm(braw.env$alphaSig/2)-zp*sqrt(nvals[ni]-3))
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])+
                            rSamplingDistr(-rp,rvals[ei],nvals[ni])
                  dwz<-dnorm(zp,qnorm(braw.env$alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3)) -
                    dnorm(zp,-qnorm(braw.env$alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                },
                "nw"={ 
                  zp<-(qnorm(0.8)-qnorm(braw.env$alphaSig))/sqrt(vals-3)
                  rp<-tanh(zp)
                  addition<-rSamplingDistr(rp,rvals[ei],nvals[ni])+
                            rSamplingDistr(-rp,rvals[ei],nvals[ni])
                  dznw<- -zp/(vals-3)/2
                  addition<-addition*dznw*(1-rp^2)
                },
                "wp"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(braw.env$alphaSig/2)+zp*sqrt(nvals[ni]-3)) + pnorm(qnorm(braw.env$alphaSig/2)-zp*sqrt(nvals[ni]-3))
                  addition<-fullRPopulationDist(rp,world)
                  dwz<-dnorm(zp,qnorm(braw.env$alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3)) -
                    dnorm(zp,-qnorm(braw.env$alphaSig/2)/sqrt(nvals[ni]-3),1/sqrt(nvals[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
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
      d<-d/sum(d1*c(diff(rp[1:2]),diff(rp)),na.rm=TRUE)
      sourceSampDens_r<-rbind(sourceSampDens_r,d*rdens[ei])
  }

  if (separate) {
    return(sourceSampDens_r)
  } else {
    return(colMeans(sourceSampDens_r))
  }
}

