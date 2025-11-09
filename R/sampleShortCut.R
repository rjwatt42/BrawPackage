
sampleShortCut<-function(hypothesis,design,evidence,nsims,appendData,oldanalysis=c()) {
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
    if (!effect$world$On) {
      effect$world$PDF<-"Single"
      effect$world$RZ<-"r"
      effect$world$PDFk<-tanh(atanh(effect$rIV)+rnorm(1,0,atanh(effect$rSD)))
      effect$world$pRplus<-1
    }
    pops<-rRandomValue(effect$world,sample_increase)
    popsOld<-pops$old
    pops<-pops$use
    # make some sample sizes
    if (design$sN<1) {
      pops1<-pops
      pops1[pops==0]<-popsOld[pops==0]
      ns<-rw2n(pops1,design$sN)
    } else {
      ns<-rep(design$sN,sample_increase)
      if (design$sNRand) {
        ns<-nDistrRand(sample_increase,design)
        ns<-as.integer(round(ns))
      }
    }
    
    s1<-1/sqrt(ns-3)
    rs<-tanh(rnorm(sample_increase,mean=atanh(pops),sd=s1))
    ps<-(1-pnorm(atanh(abs(rs)),0,s1))*2
    ps<-r2p(rs,ns)
    
    if (evidence$sigOnly>0) {
      keep1<-isSignificant(braw.env$STMethod,ps,rs,ns,df1,evidence)
      keep0<-runif(length(keep1))>evidence$sigOnly
      keep<-keep1 | keep0
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
                   iv.mn=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$iv.mn),
                   iv.sd=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$iv.sd),
                   iv.sk=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$iv.sk),
                   iv.kt=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$iv.kt),
                   dv.mn=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$dv.mn),
                   dv.sd=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$dv.sd),
                   dv.sk=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$dv.sk),
                   dv.kt=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$dv.kt),
                   er.mn=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$er.mn),
                   er.sd=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$er.sd),
                   er.sk=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$er.sk),
                   er.kt=rbind(matrix(rep(0,nsims),ncol=1),oldanalysis$er.kt),
                   AIC=rbind(matrix(rep(NA,nsims),ncol=1),oldanalysis$AIC),
                   AICnull=rbind(matrix(rep(NA,nsims),ncol=1),oldanalysis$AICnull)
    )
    
  } else {
  analysis<-list(rIV=matrix(r_effects[1:nsims],ncol=1),
                 pIV=matrix(p_effects[1:nsims],ncol=1),
                 rpIV=matrix(rp_effects[1:nsims],ncol=1),
                 nval=matrix(n_effects[1:nsims],ncol=1),
                 df1=matrix(rep(df1,nsims),ncol=1),
                 iv.mn=matrix(rep(0,nsims),ncol=1),
                 iv.sd=matrix(rep(0,nsims),ncol=1),
                 iv.sk=matrix(rep(0,nsims),ncol=1),
                 iv.kt=matrix(rep(0,nsims),ncol=1),
                 dv.mn=matrix(rep(0,nsims),ncol=1),
                 dv.sd=matrix(rep(0,nsims),ncol=1),
                 dv.sk=matrix(rep(0,nsims),ncol=1),
                 dv.kt=matrix(rep(0,nsims),ncol=1),
                 er.mn=matrix(rep(0,nsims),ncol=1),
                 er.sd=matrix(rep(0,nsims),ncol=1),
                 er.sk=matrix(rep(0,nsims),ncol=1),
                 er.kt=matrix(rep(0,nsims),ncol=1),
                 AIC=NA,AICnull=NA
  )
  }
  analysis$participant<-1:length(analysis$rIV)
  analysis$hypothesis<-effect
  analysis$design<-design
  analysis$evidence<-evidence
  analysis
}

