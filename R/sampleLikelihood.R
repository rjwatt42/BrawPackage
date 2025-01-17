
SingleSamplingPDF<-function(z,lambda,sigma,shape=0,remove_nonsig=FALSE,df1=1) {
  # shape is additional normal error distribution
  sigmad<-sqrt(sigma^2+shape^2)
  d1<-exp(-0.5*((z-lambda)^2/sigmad^2))/sqrt(2*pi*sigmad^2)
  if (remove_nonsig) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d0<-1-(pnorm(zcrit,lambda,sigmad)-pnorm(-zcrit,lambda,sigmad))
  } else {
    d0<-1
  }
  return(list(pdf=d1,sig_pdf=d0))
}


GaussSamplingPDF<-function(z,lambda,sigma,offset=0,shape=NA,remove_nonsig=FALSE,df1=1) {
  sigma2<-sqrt(lambda^2+sigma^2)
  d1<-exp(-0.5*(z-offset)^2/sigma2^2)/sqrt(2*pi*sigma2^2)
  
  if (remove_nonsig) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d0<-GaussSamplingCDF(zcrit,lambda,sigma)
  } else {
    d0<-1
  }
  return(list(pdf=d1,sig_pdf=d0))
}
GaussSamplingCDF<-function(zcrit,lambda,sigma,offset=0) {
  sigma<-sqrt(lambda^2+sigma^2)
  1-(pnorm(zcrit,offset,sigma)-pnorm(-zcrit,offset,sigma))
}


ExpSamplingPDF<-function(z,lambda,sigma,shape=NA,remove_nonsig=FALSE,df1=1) {
  lambda1<-1/lambda
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
  
  if (remove_nonsig) {
    zcrit<-atanh(p2r(braw.env$alphaSig,1/sigma^2+3,df1))
    d0<-ExpSamplingCDF(zcrit,lambda,sigma)
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
  1-(p1-p2)
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


GammaSamplingPDF<-function(z,lambda,sigma,gamma_shape=1,remove_nonsig=FALSE,df1=1) {
  if (length(sigma)==1) {sigma<-rep(sigma,length(z))}
  
  zi<-seq(-braw.env$dist_range,braw.env$dist_range,braw.env$dist_zi)
  zpd<-dgamma(abs(zi),shape=gamma_shape,scale=lambda/gamma_shape)
  zpd<-zpd/(sum(zpd)*braw.env$dist_zi)
  
  d1<-convolveWith(zi,zpd,z,sigma)

  if (remove_nonsig) {
    d2<-removeNonSig(zi,zpd,sigma,df1)
  } else {
    d2<-1
  }
  return(list(pdf=d1,sig_pdf=d2))
  
}

GenExpSamplingPDF<-function(z,lambda,sigma,genexp_shape=1,remove_nonsig=FALSE,df1=1) {
  
  if (all(sigma==0)) {
    zd<-1-(1-exp(-abs(z)/lambda))^genexp_shape
    zd<-zd/(sum(zd)*(z[2]-z[1]))
    return(zd)
  }

  if (length(sigma)==1) {sigma<-rep(sigma,length(z))}

  zi<-seq(-braw.env$dist_range,braw.env$dist_range,braw.env$dist_zi)
  zpd<-1-(1-exp(-abs(zi)/lambda))^genexp_shape
  zpd<-zpd/(sum(zpd)*braw.env$dist_zi)

  d1<-convolveWith(zi,zpd,z,sigma)
  
  if (remove_nonsig) {
    d2<-removeNonSig(zi,zpd,sigma,df1)
  } else {
    d2<-1
  }
  return(list(pdf=d1,sig_pdf=d2))
  
}


getLogLikelihood<-function(z,n,df1,distribution,param1,param2=0,remove_nonsig=FALSE) {
  sigma<-1/sqrt(n-3)
  lambda2<-0
  zcrit<-atanh(p2r(braw.env$alphaSig,n,df1))

  if (distribution=="fixed") {
    shape<-NA
    res<-matrix(0,nrow=length(param1),ncol=length(param2))
    lambda<-param1
    for (i in 1:length(lambda)) {
        mainPDF<-SingleSamplingPDF(z,lambda[i],sigma,shape=0,remove_nonsig=remove_nonsig,df1=df1)
        # now normalize for the non-sig
        likelihoods<-mainPDF$pdf/mainPDF$sig_pdf
        # likelihoods[is.infinite(likelihoods)]<-NA
        res[i,1]<-sum(log(likelihoods[likelihoods>1e-300]),na.rm=TRUE)
        if (res[i,1]==Inf) {
          a<-1
      }
    }
    return(res)
  } 
  if (distribution=="random") {
    shape<-NA
    res<-matrix(0,nrow=length(param1),ncol=length(param2))
    lambda1<-param1
    lambda2<-param2
    param2<-0
    for (i in 1:length(lambda1)) {
      for (j in 1:length(lambda2)) {
        mainPDF<-SingleSamplingPDF(z,lambda1[i],sigma,shape=lambda2[j],remove_nonsig=remove_nonsig,df1=df1)
        # now normalize for the non-sig
        likelihoods<-mainPDF$pdf/mainPDF$sig_pdf
        # likelihoods[is.infinite(likelihoods)]<-NA
        res[i,j]<-sum(log(likelihoods[likelihoods>1e-300]),na.rm=TRUE)
        if (res[i,j]==Inf) {
          a<-1
        }
      }
    }
    return(res)
  } 
  
  # get nulls ready first
  if (any(param2>0)) {
    nullPDF<-SingleSamplingPDF(z,0,sigma,NA,remove_nonsig,df1)
  } else {
    nullPDF<-list(pdf=0,sig_pdf=1)
    zcrit<-0
  } 
  shape<-NA
  res<-matrix(0,nrow=length(param1),ncol=length(param2))
  switch(distribution,
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
           shape<-metaAnal$shape
         },
         "GenExp"={
           PDF<-GenExpSamplingPDF
           shape<-metaAnal$shape
         }
  )
  for (i in 1:length(param1)) {
    lambda<-param1[i]
    mainPDF<-PDF(z,lambda,sigma,shape=shape,remove_nonsig=remove_nonsig,df1=df1)
    for (j in 1:length(param2)) {
      nullP<-param2[j]
      # make the whole source first
      sourcePDF<-mainPDF$pdf*(1-nullP)+nullPDF$pdf*nullP
      # now normalize for the non-sig
      likelihoods<-sourcePDF/(mainPDF$sig_pdf*(1-nullP)+nullPDF$sig_pdf*nullP)
      # likelihoods[is.infinite(likelihoods)]<-NA
      res[i,j]<-sum(log(likelihoods[likelihoods>1e-300]),na.rm=TRUE)
      if (res[i,j]==Inf) {
        a<-1
      }
    }
  }
  res
}

