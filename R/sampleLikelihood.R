
SingleSamplingPDF<-function(z,lambda,sigma,shape=0,bias=FALSE,df1=1) {
  # shape is additional normal error distribution
  sigmad<-sigma^2+shape
  sigmad[sigmad<0]<-0
  sigmad<-sqrt(sigmad)
  d1<-exp(-0.5*((z-lambda)^2/sigmad^2))/sqrt(2*pi*sigmad^2)
  if (bias>0) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d1[abs(z)<zcrit]<-d1[abs(z)<zcrit]*(1-bias)
    d0<-1-(pnorm(zcrit,lambda,sigmad)-pnorm(-zcrit,lambda,sigmad))*bias
  } else {
    d0<-1
  }
  return(list(pdf=d1,sig_pdf=d0))
}


UniformSamplingPDF<-function(z,lambda,sigma,shape=0,bias=FALSE,df1=1) {
  # shape is additional normal error distribution
  d1<-z*0+1
  sigmad<-sigma^2+shape
  sigmad[sigmad<0]<-0
  sigmad<-sqrt(sigmad)
  d1<-exp(-0.5*((z-lambda)^2/sigmad^2))/sqrt(2*pi*sigmad^2)
  if (bias>0) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d1[abs(z)<zcrit]<-d1[abs(z)<zcrit]*(1-bias)
    d0<-1-(pnorm(zcrit,lambda,sigmad)-pnorm(-zcrit,lambda,sigmad))*bias
  } else {
    d0<-1
  }
  return(list(pdf=d1,sig_pdf=d0))
}


GaussSamplingPDF<-function(z,lambda,sigma,offset=0,shape=NA,bias=FALSE,df1=1) {
  sigma2<-sqrt(lambda^2+sigma^2)
  d1<-exp(-0.5*(z-offset)^2/sigma2^2)/sqrt(2*pi*sigma2^2)
  
  if (bias>0) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d1[abs(z)<zcrit]<-d1[abs(z)<zcrit]*(1-bias)
    d0<-1-GaussSamplingCDF(zcrit,lambda,sigma)*bias
  } else {
    d0<-1
  }
  return(list(pdf=d1,sig_pdf=d0))
}
GaussSamplingCDF<-function(zcrit,lambda,sigma,offset=0) {
  sigma<-sqrt(lambda^2+sigma^2)
  (pnorm(zcrit,offset,sigma)-pnorm(-zcrit,offset,sigma))
}


ExpSamplingPDF<-function(z,lambda,sigma,shape=NA,bias=FALSE,df1=1) {
  if (lambda==0) return(GaussSamplingPDF(z,lambda,sigma,offset=0,shape=NA,bias=FALSE,df1=1))
  # lambda1<-1/lambda
  # d1a<-0.25*(lambda1*exp(-lambda1*(z-sigma^2*lambda1/2))*(1+erf((z-sigma^2*lambda1)/sqrt(2)/sigma)) +
  #             lambda1*exp(-lambda1*(-z-sigma^2*lambda1/2))*(1+erf((-z-sigma^2*lambda1)/sqrt(2)/sigma)))

  # pnorm approximation breaks down at pnorm(8) and at pnorm(-37)
  sl<-sigma^2/lambda
  zv1<-(z-sl)/sigma
  p1<-pnorm(zv1)
  if (any(zv1>0)) {
    p1[zv1>0]<-1-pnorm(-zv1[zv1>0])
  }
  zv2<-(-z-sl)/sigma
  p2<-pnorm(zv2)
  if (any(zv2>0)) {
    p2[zv2>0]<-1-pnorm(-zv2[zv2>0])
  }
  
  e1<-exp(-( z-sl/2)/lambda)
  e2<-exp(-(-z-sl/2)/lambda)
  e1[e1==Inf]<-1
  e2[e2==Inf]<-1
  d1<-e1*p1+e2*p2
  d1<-d1/lambda/2
  d1[is.na(d1)]<-0.5
  
  if (bias>0) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d1[abs(z)<zcrit]<-d1[abs(z)<zcrit]*(1-bias)
    d0<-1-ExpSamplingCDF(zcrit,lambda,sigma)*bias
  } else {
    d0<-1
  }
  return(list(pdf=d1,sig_pdf=d0))
}
ExpSamplingCDF<-function(zcrit,lambda,sigma) {
  lambda<-1/lambda
  z <- zcrit
  p1<-0.25*(
    exp((lambda*sigma/sqrt(2))^2)*exp(z*lambda) * erfc(lambda*sigma/sqrt(2) + z/sigma/sqrt(2))
    - exp((lambda*sigma/sqrt(2))^2)/exp(z*lambda) * erfc(lambda*sigma/sqrt(2) - z/sigma/sqrt(2))
    + 2*erf(z/sigma/sqrt(2))
  )
  z <- -zcrit
  p2<-0.25*(
    exp((lambda*sigma/sqrt(2))^2)*exp(z*lambda) * erfc(lambda*sigma/sqrt(2) + z/sigma/sqrt(2))
    - exp((lambda*sigma/sqrt(2))^2)/exp(z*lambda) * erfc(lambda*sigma/sqrt(2) - z/sigma/sqrt(2))
    + 2*erf(z/sigma/sqrt(2))
  )
  (p1-p2)
}


convolveWith<-function(zi,zpd,z,sigma) {
  d1<-z*0
  for (i in 1:length(z)) {
    zs<-zpd*dnorm(zi,z[i],sigma[i])
    d1[i]<-sum(zs)*braw.env$dist_zi
  }
  return(d1)
}

removeNonSig<-function(zi,zpd,sigma,df1) {
  zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
  # d2<-GammaSamplingCDF(zcrit,lambda,sigma,gamma_shape)
  d2<-zcrit*0
  zcritUnique<-unique(zcrit)
  for (i in 1:length(zcritUnique)) {
    use<-which(zcrit==zcritUnique[i])
    zi1<-seq(-braw.env$dist_range,-zcritUnique[i],braw.env$dist_zi)
    zi1<-c(zi1,-zcritUnique[i])
    
    d0<-zi1*0
    for (j in 1:length(zi1)) {
      zs<-zpd*dnorm(zi,zi1[j],sigma[use[1]])
      d0[j]<-sum(zs)*braw.env$dist_zi
    }
    areas<-(d0[1:(length(zi1)-1)]+d0[2:length(zi1)])/2*diff(zi1)
    d2[use]<-sum(areas)*2
  }
  return(d2)
}


GammaSamplingPDF<-function(z,lambda,sigma,shape=1,bias=FALSE,df1=1) {
  if (length(sigma)==1) {sigma<-rep(sigma,length(z))}
  
  if (all(sigma==0)) {
    zd<-dgamma(abs(z),shape=shape,scale=lambda/shape)
    zd<-zd/(sum(zd)*(z[2]-z[1]))
    return(zd)
  }
  zi<-seq(-braw.env$dist_range,braw.env$dist_range,braw.env$dist_zi)
  zpd<-dgamma(abs(zi),shape=shape,scale=lambda/shape)
  zpd<-zpd/(sum(zpd)*braw.env$dist_zi)
  
  d1<-convolveWith(zi,zpd,z,sigma)

  if (bias) {
    d2<-removeNonSig(zi,zpd,sigma,df1)
  } else {
    d2<-1
  }
  return(list(pdf=d1,sig_pdf=d2))
  
}

GenExpSamplingPDF<-function(z,lambda,sigma,shape=1,bias=FALSE,df1=1) {
  genExp<-function(z,lambda,shape) {
    if (lambda==0 || shape==0) as.numeric(z==0)+0.1
    else exp(-1/shape*(abs(z)/lambda)^shape)
    }
  zi<-seq(-braw.env$dist_range,braw.env$dist_range,braw.env$dist_zi)
  
  if (is.null(braw.env$genExpGains)) {
    lambdas<-seq(0,3,0.01)
    genexp_shapes<-seq(0,4,0.02)
    gains<-matrix(nrow=length(lambdas),ncol=length(genexp_shapes))
    for (i in 1:length(lambdas))
      for (j in 1:length(genexp_shapes)) {
        zdi<-genExp(zi,lambdas[i],genexp_shapes[j])
        gains[i,j]<-sum(zdi)*braw.env$dist_zi
      }
    setBrawEnv("genExpGains",list(lambdas=lambdas,genexp_shapes=genexp_shapes,gains=gains))
  }

  gain<-interp2(x=braw.env$genExpGains$genexp_shapes,y=braw.env$genExpGains$lambdas,braw.env$genExpGains$gains,shape,lambda)
  if (all(sigma==0)) {
    zd<-genExp(z,lambda,shape)/gain
    return(zd)
  }

  zdi<-genExp(zi,lambda,shape)/gain
  if (length(sigma)==1) {sigma<-rep(sigma,length(z))}

  d1<-convolveWith(zi,zdi,z,sigma)

  if (bias) {
    d2<-removeNonSig(zi,zd,sigma,df1)
  } else {
    d2<-1
  }
  return(list(pdf=d1,sig_pdf=d2))
}


getLogLikelihood<-function(z,n,df1,distribution,location,spread=0,shape=1,bias=FALSE,returnVals=FALSE) {
  if (is.null(spread)) spread<-0
  sigma<-1/sqrt(n-3)
  # if (length(sigma)==1) sigma<-sigma[1,1]
  # if (length(z)==1) z<-z[1,1]
  
  lambda2<-0
  zcrit<-atanh(p2r(braw.env$alphaSig,n,df1))

  if (distribution=="fixed") {
    res<-matrix(-Inf,nrow=length(location),ncol=length(spread))
    lksHold<-c()
    lambda<-location
    for (i in 1:length(lambda)) {
      j<-1
        mainPDF<-SingleSamplingPDF(z,lambda[i],sigma,shape=0,bias=bias,df1=df1)
        # now normalize for the non-sig
        likelihoods<-mainPDF$pdf/mainPDF$sig_pdf
        # likelihoods[(likelihoods<1e-300)]<- 1e-300
        res[i,j]<-sum(log(likelihoods[likelihoods>=1e-300]),na.rm=TRUE)+(-1000*sum(likelihoods<1e-300))
        if (res[i,j]==max(res,na.rm=TRUE)) lksHold<-likelihoods
    }
    if (returnVals) return(lksHold)
    return(res)
  } 
  if (distribution=="random") {
    res<-matrix(-Inf,nrow=length(location),ncol=length(spread))
    lksHold<-c()
    lambda<-location
    for (i in 1:length(lambda)) {
      for (j in 1:length(spread)) {
        mainPDF<-SingleSamplingPDF(z,lambda[i],sigma,shape=spread[j],bias=bias,df1=df1)
        # now normalize for the non-sig
        likelihoods<-mainPDF$pdf/mainPDF$sig_pdf
        likelihoods[(likelihoods<1e-300)]<- 1e-300
        res[i,j]<-sum(log(likelihoods),na.rm=TRUE)
        if (res[i,j]==max(res,na.rm=TRUE)) lksHold<-likelihoods
      }
    }
    if (returnVals) return(lksHold)
    return(res)
  } 
  
  # get nulls ready first
  pRPluss<-spread
  if (any(pRPluss<1)) {
    nullPDF<-SingleSamplingPDF(z,0,sigma,0,bias,df1)
  } else {
    nullPDF<-list(pdf=0,sig_pdf=1)
    zcrit<-0
  } 
  res<-matrix(-Inf,nrow=length(location),ncol=length(pRPluss))
  switch(distribution,
         "Uniform"={
           PDF<-UniformSamplingPDF
         },
         "Single"={
           PDF<-SingleSamplingPDF
         },
         "Gauss"={
           PDF<-GaussSamplingPDF
         },
         "Exp"={
           PDF<-ExpSamplingPDF
         },
         "Gamma"={
           PDF<-GammaSamplingPDF
         },
         "GenExp"={
           PDF<-GenExpSamplingPDF
         }
  )
  for (i in 1:length(location)) {
    lambda<-location[i]
    mainPDF<-PDF(z,lambda,sigma,shape=shape,bias=bias,df1=df1)
    for (j in 1:length(pRPluss)) {
      pRPlus<-pRPluss[j]
      # make the whole source first
      likelihoods<-mainPDF$pdf*pRPlus+nullPDF$pdf*(1-pRPlus)
      # now normalize for the non-sig
      likelihoods<-likelihoods/(mainPDF$sig_pdf*pRPlus+nullPDF$sig_pdf*(1-pRPlus))
      likelihoods[(likelihoods<1e-300)]<- 1e-300
      res[i,j]<-sum(log(likelihoods),na.rm=TRUE)
      if (res[i,j]==max(res,na.rm=TRUE)) lksHold<-log(likelihoods)
    }
  }
  if (returnVals) return(lksHold)
  return(res)
}

