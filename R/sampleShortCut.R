
sampleShortCut<-function(hypothesis,design,evidence,nsims,appendData,oldanalysis=c(),sigOnly=FALSE) {
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  
  # make some population values according to the specified a priori distribution
  r_effects<-c()
  rp_effects<-c()
  p_effects<-c()
  n_effects<-c()
  if (IV$type=="Categorical") {
    df1<-IV$ncats-1
  } else {
    df1<-1
  }
  
  while (length(r_effects)<nsims) {
    sample_increase<-min(nsims-length(r_effects),nsims)
    if (!effect$world$worldOn) {
      effect$world$populationPDF<-"Single"
      effect$world$populationRZ<-"r"
      effect$world$populationPDFk<-effect$rIV
      effect$world$populationNullp<-0
    }
    switch (paste0(effect$world$populationPDF,"_",effect$world$populationRZ),
            "Single_r"={
              pops<-rep(effect$world$populationPDFk,sample_increase)
            },
            "Single_z"={
              pops<-rep(effect$world$populationPDFk,sample_increase)
              pops<-tanh(pops)
            },
            "Double_r"={
              pops<-rep(effect$world$populationPDFk,sample_increase)*sign(rnorm(sample_increase))
            },
            "Double_z"={
              pops<-rep(effect$world$populationPDFk,sample_increase)*sign(rnorm(sample_increase))
              pops<-tanh(pops)
            },
            "Exp_z"={
              pops<-rexp(sample_increase,rate=1/effect$world$populationPDFk)
              pops<-pops*sign(rnorm(length(pops)))
              pops<-tanh(pops)
            },
            "Exp_r"={
              pops<-rexp(1.5*sample_increase,rate=1/effect$world$populationPDFk)
              pops<-pops[pops<1]
              if (length(pops)>sample_increase) {
                pops<-pops[1:(sample_increase)]
              }
              pops<-pops*sign(rnorm(length(pops)))
            },
            "Gauss_z"={
              pops<-rnorm(sample_increase,sd=effect$world$populationPDFk)
              pops<-tanh(pops)
            },
            "Gauss_r"={
              pops<-rnorm(sample_increase,sd=effect$world$populationPDFk)
              pops<-pops[abs(pops)<1]
            },
            "Uniform_r"={
              pops<-runif(sample_increase,min=-1,max=1)
            },
            "Uniform_z"={
              pops<-runif(sample_increase,min=-uniformZrange,max=uniformZrange)
              pops<-tanh(pops)
            }
    )
    popsOld<-pops
    if (effect$world$populationNullp>0) {
      change<-rand(length(pops),1)<=effect$world$populationNullp
      pops[change]<-0
    }
    # make some sample sizes
    if (design$sN<1) {
      pops1<-pops
      pops1[pops==0]<-popsOld[pops==0]
      ns<-rw2n(pops1,design$sN)
    } else {
      ns<-rep(design$sN,sample_increase)
      if (design$sNRand) {
        ns<-braw.env$minN+rgamma(sample_increase,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
        ns<-as.integer(round(ns))
      }
    }
    
    s1<-1/sqrt(ns-3)
    rs<-tanh(rnorm(sample_increase,mean=atanh(pops),sd=s1))
    ps<-(1-pnorm(atanh(abs(rs)),0,s1))*2
    
    if (sigOnly) {
      keep<-isSignificant(braw.env$STMethod,ps,rs,ns,df1,evidence)
      pops<-pops[keep]
      rs<-rs[keep]
      ps<-ps[keep]
      ns<-ns[keep]
    }
    r_effects=c(r_effects,rs)
    rp_effects=c(rp_effects,pops)
    p_effects=c(p_effects,ps)
    n_effects=c(n_effects,ns)
  }
  ra_effects=r_effects
  if (appendData && !isempty(oldanalysis)) {
    analysis<-list(rIV=rbind(matrix(r_effects[1:nsims],ncol=1),oldanalysis$rIV),
                   pIV=rbind(matrix(p_effects[1:nsims],ncol=1),oldanalysis$pIV),
                   rpIV=rbind(matrix(r_effects[1:nsims],ncol=1),oldanalysis$rpIV),
                   nval=rbind(matrix(n_effects[1:nsims],ncol=1),oldanalysis$nval),
                   df1=rbind(matrix(rep(df1,nsims),ncol=1),oldanalysis$df1),
                   dv=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$dv)
    )
    
  } else {
  analysis<-list(rIV=matrix(r_effects[1:nsims],ncol=1),
                 pIV=matrix(p_effects[1:nsims],ncol=1),
                 rpIV=matrix(rp_effects[1:nsims],ncol=1),
                 nval=matrix(n_effects[1:nsims],ncol=1),
                 df1=matrix(rep(df1,nsims),ncol=1),
                 dv=rep(0,nsims)
  )
  }
  analysis$participant<-1:length(analysis$rIV)
  analysis$hypothesis<-effect
  analysis$design<-design
  analysis$evidence<-evidence
  analysis
}

