alphaLLR<-function(alphaSig=braw.env$alphaSig) 0.5*qnorm(1-alphaSig/2)^2

isSignificant<-function(method="NHST",p,r,n,df1,evidence,alphaLocal=braw.env$alphaSig) {
  if (length(alphaLocal)>1) {
    alphaLocal<-rep(alphaLocal,each=nrow(p))
  }
  switch (method,
          "NHST"={
            sig<-p<alphaLocal
          },
          "sLLR"={
            s<-r2llr(r,n,df1,"sLLR",evidence$llr,evidence$prior)
            sig<-s>alphaLLR(alphaLocal)
          },
          "dLLR"={
            r[abs(r)>1]<-1
            d<-r2llr(abs(r),n,df1,"dLLR",evidence$llr,evidence$prior)
            sig<-abs(d)>(alphaLLR(alphaLocal)*sign(d))
          }
  )
  return(sig)
}
