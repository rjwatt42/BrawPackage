isSignificant<-function(method="NHST",p,r,n,df1,evidence,alphaLocal=alphaSig) {
  if (length(alphaLocal)>1) {
    alphaLocal<-rep(alphaLocal,each=nrow(p))
    alphaLLR<-0.5*qnorm(1-alphaLocal/2)^2
  }
  switch (method,
          "NHST"={
            sig<-p<alphaLocal
          },
          "sLLR"={
            s<-r2llr(r,n,df1,"sLLR",evidence$llr,evidence$prior)
            sig<-s>alphaLLR
          },
          "dLLR"={
            r[abs(r)>1]<-1
            d<-r2llr(abs(r),n,df1,"dLLR",evidence$llr,evidence$prior)
            sig<-(abs(d)>alphaLLR)*sign(d)
          }
  )
  return(sig)
}
