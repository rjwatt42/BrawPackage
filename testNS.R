sampleLK<-function(nsamp,targetS,targetN,sigOnly=FALSE) {

npts<-1001

rp<-seq(-0.99,0.99,length.out=npts)

slr<-rSamplingDistr(targetS,rp,targetN,sigOnly)

bins<-seq(0,1,length.out=npts)
cdf<-cumsum(slr)/sum(slr)
use<-diff(cdf)>0
qlr<-approx(cdf[use],rp[use],bins)$y

rq<-runif(nsamp)
rq1<-approx(bins,qlr,rq)$y
return(rq1)

}

