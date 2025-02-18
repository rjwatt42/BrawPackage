rr<-function(nr,r,design=braw.def$design) {
  n<-round(nDistrRand(nr,design))
  r<-tanh(rnorm(nr,atanh(r),1/sqrt(n-3)))
  return(data.frame(rIV=r,nval=n))
}
