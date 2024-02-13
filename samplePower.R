
dwdz<-function(z,n,t=2) {
  if (t==1) {
    dwdz<-dnorm(z,-qnorm(alphaSig)/sqrt(n-3),1/sqrt(n-3))
    # dwdz<-exp(-(z*sqrt(n-3) + qnorm(alphaSig))^2/2)*sqrt(n-3)/sqrt(2*pi)
  } else {
    dwdz<-dnorm(z,-qnorm(alphaSig/2)/sqrt(n-3),1/sqrt(n-3))
    dwdz<-dwdz+dnorm(z,+qnorm(alphaSig/2)/sqrt(n-3),1/sqrt(n-3))
    # dwdz<-     exp(-(z*sqrt(n-3) + qnorm(alphaSig/2))^2/2)*sqrt(n-3)/sqrt(2*pi)
    # dwdz<-dwdz+exp(-(z*sqrt(n-3) - qnorm(alphaSig/2))^2/2)*sqrt(n-3)/sqrt(2*pi)
  }
  return(dwdz)
}

zn2w<-function(z,n,t=2){
  z<-abs(z)
  w<-(z+n)*0 # just in case z and n are different lengths
  # one-tailed
    if (t==1) {
      w<-pnorm(qnorm(alphaSig)+z*sqrt(n-3))
    } else {
      # two-tailed
      pw1<-pnorm(qnorm(alphaSig/2)+z*sqrt(n-3))
      pw2<-pnorm(qnorm(alphaSig/2)-z*sqrt(n-3))
      w<-pw1+pw2
    }
  w[z==0]<-alphaSig
  w[n<3]<-0
  w  
}

rn2w<-function(r,n,t=2){
  if (any(abs(r)>1)) {
    print(paste0("rn2w exception: ",format(max(abs(r)),digits=3)))
    r[r>1]<-1
    r[r < -1]<- -1
  }
  z<-atanh(r)
  zn2w(z,n,t)
}

wn2z<-function(w,n,t=2){
  if (t==1) {
    # one-tailed
    z<-(qnorm(w)-qnorm(alphaSig))/sqrt(n-3)
  } else {
    # two tailed
    if (any(w>1)) {
      print("w error")
      w[w>1]<-1
    }
    z<-(qnorm(w)-qnorm(alphaSig/2))/sqrt(n-3)
  }
  z
}

rw2n<-function(r,w,t=2){
  if (any(abs(r)>1)) {
    print("rw2n exception")
    r[r>1]<-1
    r[r < -1]<- -1
  }
  r<-abs(r)
  z<-atanh(r)
  if (t==1) {
    # one-tailed
    nnear<-((qnorm(w)-qnorm(alphaSig))/z)^2+3
  } else {
    # two tailed
    nnear<-((qnorm(w)-qnorm(alphaSig/2))/z)^2+3
  }
  nnear<-round(nnear)  
  nnear[nnear>1000000]<-1000000
  nnear[nnear<5]<-5
  nnear
}
