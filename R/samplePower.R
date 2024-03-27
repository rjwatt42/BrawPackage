
dwdz<-function(z,n,t=2,alpha=NA) {
  if (is.na(alpha)) alpha<-braw.env$alphaSig
  if (t==1) {
    dwdz<-dnorm(z,-qnorm(alpha)/sqrt(n-3),1/sqrt(n-3))
    # dwdz<-exp(-(z*sqrt(n-3) + qnorm(braw.env$alphaSig))^2/2)*sqrt(n-3)/sqrt(2*pi)
  } else {
    dwdz<-dnorm(z,-qnorm(alpha/2)/sqrt(n-3),1/sqrt(n-3))
    dwdz<-dwdz+dnorm(z,+qnorm(alpha/2)/sqrt(n-3),1/sqrt(n-3))
    # dwdz<-     exp(-(z*sqrt(n-3) + qnorm(braw.env$alphaSig/2))^2/2)*sqrt(n-3)/sqrt(2*pi)
    # dwdz<-dwdz+exp(-(z*sqrt(n-3) - qnorm(braw.env$alphaSig/2))^2/2)*sqrt(n-3)/sqrt(2*pi)
  }
  return(dwdz)
}

zn2w<-function(z,n,t=2,alpha=NA){
  if (is.na(alpha)) alpha<-braw.env$alphaSig
  z<-abs(z)
  w<-(z+n)*0 # just in case z and n are different lengths
  # one-tailed
    if (t==1) {
      w<-pnorm(qnorm(alpha)+z*sqrt(n-3))
    } else {
      # two-tailed
      pw1<-pnorm(qnorm(alpha/2)+z*sqrt(n-3))
      pw2<-pnorm(qnorm(alpha/2)-z*sqrt(n-3))
      w<-pw1+pw2
    }
  w[z==0]<-alpha
  w[n<3]<-0
  w  
}

rn2w<-function(r,n,t=2,alpha=NA){
  if (is.na(alpha)) alpha<-braw.env$alphaSig
  if (any(abs(r)>1)) {
    print(paste0("rn2w exception: ",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  z<-atanh(r)
  zn2w(z,n,t,alpha)
}

wn2z<-function(w,n,t=2,alpha=NA){
  if (is.na(alpha)) alpha<-braw.env$alphaSig
  if (t==1) {
    # one-tailed
    z<-(qnorm(w)-qnorm(alpha))/sqrt(n-3)
  } else {
    # two tailed
    if (any(w>1)) {
      print("w error")
      w[w>1]<-1
    }
    z<-(qnorm(w)-qnorm(alpha/2))/sqrt(n-3)
  }
  z
}

rw2n<-function(r,w,t=2,alpha=NA){
  if (is.na(alpha)) alpha<-braw.env$alphaSig
  if (any(abs(r)>1)) {
    print("rw2n exception")
    r[r>1]<-1
    r[r < -1]<- -1
  }
  r<-abs(r)
  z<-atanh(r)
  if (t==1) {
    # one-tailed
    nnear<-((qnorm(w)-qnorm(alpha))/z)^2+3
  } else {
    # two tailed
    nnear<-((qnorm(w)-qnorm(alpha/2))/z)^2+3
  }
  nnear<-round(nnear)  
  nnear[nnear>1000000]<-1000000
  nnear[nnear<5]<-5
  nnear
}

rn2p<-function(r,n,t=2) {
  if (any(abs(r)>1)) {
    print("rn2p exception")
    r[r>1]<-1
    r[r < -1]<- -1
  }
  r<-abs(r)
  z<-atanh(r)
  p<-1-pnorm(z,sd=1/sqrt(n-3))
  return(p*t)
}