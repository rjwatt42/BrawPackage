isSignificant<-function(method="NHST",p,r,n,df1,evidence,alphaLocal=BrawOpts$alphaSig) {
  if (length(alphaLocal)>1) {
    alphaLocal<-rep(alphaLocal,each=nrow(p))
    BrawOpts$alphaLLR<-0.5*qnorm(1-alphaLocal/2)^2
  }
  switch (method,
          "NHST"={
            sig<-p<alphaLocal
          },
          "sLLR"={
            s<-r2llr(r,n,df1,"sLLR",evidence$llr,evidence$prior)
            sig<-s>BrawOpts$alphaLLR
          },
          "dLLR"={
            r[abs(r)>1]<-1
            d<-r2llr(abs(r),n,df1,"dLLR",evidence$llr,evidence$prior)
            sig<-(abs(d)>BrawOpts$alphaLLR)*sign(d)
          }
  )
  return(sig)
}
