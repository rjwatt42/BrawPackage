discreteVals<-function(ivr,ng,pp,type="continuous",cases=NULL) {
  if (is.character(pp)) pp<-as.numeric(unlist(strsplit(pp,",")))
  
  if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
  proportions<-c(0,pp)
  breaks<-qnorm(cumsum(proportions)/sum(proportions))
  # not sure we should do this.
  # while (all(ivr<breaks[2])) breaks[2]<-breaks[2]-0.1
  # while (all(ivr>breaks[ng])) breaks[ng]<-breaks[ng]+0.1
  ivDiscrete=ivr*0
  for (i in 1:ng) {ivDiscrete=ivDiscrete+(ivr>breaks[i])}
  if (type=="continuous") ivDiscrete<-(ivDiscrete-mean(1:ng))*ng
  if (type=="discrete") {
    if (length(cases)==1) cases<-strsplit(cases,",")[[1]]
    ivDiscrete<-factor(ivDiscrete,levels=1:ng,labels=cases)
  }
  return(ivDiscrete)
}

sampleLK<-function(nsamp,world) {
  
  npts<-1001
  
  rp<-seq(-0.99,0.99,length.out=npts)
  slr<-rPopulationDist(rp,world)

  bins<-seq(0,1,length.out=npts)
  cdf<-cumsum(slr)/sum(slr)
  use<-diff(cdf)>0
  qlr<-approx(cdf[use],rp[use],bins)$y
  
  rq1<-NA
  while (any(is.na(rq1))) {
  rq<-runif(nsamp)
  rq1<-approx(bins,qlr,rq)$y
  }
  
  return(rq1)
  
}

getWorldEffect<-function(nsamples=1,effect=braw.def$hypothesis$effect) {
  if (effect$world$On) {
    if (effect$world$PDFsample) {
      rho<-sampleLK(nsamples,effect$world)
    } else {
      rho<-rRandomValue(effect$world,1)$use
    }
  } else    rho<-effect$rIV
  return(rho)
}

getWorldN<-function(nsamps,design=braw.env$design) {

  if (!design$sNRand)  return(rep(design$sN,nsamps))
  else                 return(nDistrRand(nsamps,design))
}

drawCatPositions<-function(ncats){
  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-exp(-qnorm(pbreaks)^2/2)
  -1/sqrt(2*pi)*diff(ebreaks)/diff(pbreaks)
}

makeSampleVals<-function(n,mn,sdv,MV,distr="normal"){
  switch (distr,
          "normal"= {
            ivr<-rnorm(n,0,1)
            if (MV$type=="Interval" && (MV$skew!=0 || MV$kurtosis!=0)){
              if (MV$kurtosis<1.05-3) MV$kurtosis<-1.05-3
              change<-MV$skew!=0 & (MV$kurtosis)>MV$skew^2
              MV$kurtosis[change]<-MV$skew[change]^2
              
              a<-f_johnson_M(0,1,MV$skew,MV$kurtosis+3)
              ivr<-f_johnson_z2y(ivr,a$coef,a$type)
              # ivr<-rJohnson(n,parms=a)
            }
            ivr*sdv+mn
          },
          "skewed"={
            skew<-2
            kurtosis<-0
            ivr<-rnorm(n,0,1)
            a<-f_johnson_M(0,sdv,skew,kurtosis+3)
            ivr<-f_johnson_z2y(ivr,a$coef,a$type)
            ivr*sdv+mn
          },
          "uniform"={
            ivr=runif(n,min=-1,max=1)*sdv*sqrt(3)+mn
          },
          "cauchy"={
            ivr=rcauchy(n,location=0,scale=1)*qnorm(0.75)
            ivr=ivr*sdv+mn
          },
          "t(3)"={
            ivr<-rt(n,3,0)/sqrt(3/(3-2))
            ivr<-ivr*sdv+mn
          }
  )
}

makeSampleVar<-function(design,effect,n,MV,MV2){
  ivr=c()
  ivr2=c()
  dvr_s<-c()
  dvr_m<-c()
  if (design$sRangeProb<runif(1)) design$sIVRangeOn<-design$sIV2RangeOn<-FALSE
    
  while (length(ivr)<n) {
    switch (design$sMethod$type,
            "Random"={
              # purely random sample from whole range
              ivr1<-makeSampleVals(n,0,1,MV)
              dvr1_m<-rep(0,n)
              dvr1_s<-rep(1,n)
            },
            "Stratified"={
              # sampled at specific intervals
              if (MV$type=="Categorical") {
                ivr1<-((1:MV$ncats) - ceil(MV$ncats/2))/floor(MV$ncats/2)
              } else {
                r<-seq(-design$sMethod$sStrata_rRange,design$sMethod$sStrata_rRange,length.out=design$sMethod$sStrata_n)
                dens<-dnorm(r)
                dens<-round(dens/sum(dens)*n)
                ivr1<-rep(r,dens)
                ivr<-ivr1[sample(length(ivr1),length(ivr1))]
              }
              dvr1_m<-rep(0,n)
              dvr1_s<-rep(1,n)
            },
            "Limited"={
              ivr1<-c()
              while (length(ivr1)<n) {
                ivr2<-makeSampleVals(n*10,0,1,MV)
                use<-abs(ivr2)<3*(1-design$sMethodSeverity*design$sMethod$sLimitedRange)
                ivr1<-c(ivr1,ivr2[use])
              }
              dvr1_m<-rep(0,n)
              dvr1_s<-rep(1,n)
            },
            {
              method<-design$sMethod
              
              sMethodSeverity<-design$sMethodSeverity
              Cluster_n<-ceiling(method$Cluster_n*sMethodSeverity+1)
              Contact_n<-ceiling(method$Contact_n*sMethodSeverity)
              nClusts<-ceiling(n/Cluster_n/(1+Contact_n))
              
              Cluster_rad<-1-(1-method$Cluster_rad)*(sMethodSeverity)
              Contact_rad<-1-(1-method$Contact_rad)*(sMethodSeverity)
              nsims<-1000
              
              # note that all points come from rnorm(mean=0,sd=1)
              # ie the whole population
              ivr1<-c()
              dvr1_m<-c()
              dvr1_s<-c()
              for (i in 1:nClusts) {
                # location of cluster
                x_cluster_centre<-rnorm(1)
                y_cluster_centre<-rnorm(1)
                
                if (sMethodSeverity==0) {
                  ivr1<-c(ivr1,x_cluster_centre)
                  dvr1_m<-c(dvr1_m,y_cluster_centre)
                } else {
                for (j in 1:Cluster_n) {
                  # location of contact group
                  use<-FALSE
                  while (sum(use)<1) {
                    x_contact<-rnorm(nsims)
                    y_contact<-rnorm(nsims)
                    d<-sqrt((x_contact-x_cluster_centre)^2+(y_contact-y_cluster_centre)^2)
                    use<-d<Cluster_rad*rnorm(nsims)
                  }
                  x_contact<-x_contact[which(use)[1]]
                  y_contact<-y_contact[which(use)[1]]
                  ivr1<-c(ivr1,x_contact)
                  dvr1_m<-c(dvr1_m,y_contact)

                  # track any contacts
                  if (Contact_n>0)
                  for (k in 1:Contact_n) {
                    use<-FALSE
                    while (sum(use)<1) {
                      x_contact1<-rnorm(nsims)
                      y_contact1<-rnorm(nsims)
                      d<-sqrt((x_contact1-x_contact)^2+(y_contact1-y_contact)^2)
                      use<-d<Contact_rad*rnorm(nsims)
                    }
                    x_contact<-x_contact1[which(use)[1]]
                    y_contact<-y_contact1[which(use)[1]]
                    ivr1<-c(ivr1,x_contact)
                    dvr1_m<-c(dvr1_m,y_contact)
                  }
                }
                }
              }
              
              use<-sample(length(ivr1),length(ivr1))
              ivr1<-ivr1[use]
              dvr1_m<-dvr1_m[use]
              dvr1_m<-dvr1_m/std(dvr1_m)
              dvr1_s<-rep(0,n)
            }
    )

    # make iv2 (if needed)
    if (!is.null(MV2)){
      rho2<-effect$rIV2
      rho12<-effect$rIVIV2
      rhoInter<-effect$rIVIV2DV
      ivr2_resid<-makeSampleVals(n,0,sqrt(1-rho12^2),MV2)
      ivr21<-ivr1*rho12+ivr2_resid
    } else {
      rho2<-0
      rho12<-0
      rhoInter<-0
      ivr21<-rep(0,n)
    }
    
    if ((design$sIVRangeOn || design$sIV2RangeOn)) {
      # sRangeWidth<-max(0.01,rgamma(1,5,scale=1/5)*(1-design$sRangeVary))
      sRangeWidth<-0.0
      sIVRange<-design$sIVRange+c(-1,1)*sRangeWidth
      sIV2Range<-design$sIV2Range+c(-1,1)*sRangeWidth
      if (design$sRangeVary>0) {
        sIVRange<-sIVRange+rnorm(1,0,design$sRangeVary)
        sIV2Range<-sIV2Range+rnorm(1,0,design$sRangeVary)
      }
      condition<-rep(TRUE,length(ivr1))
      if (design$sIVRangeOn) {
        if (sIVRange[1]==sIVRange[2])
          ivr1<-ivr*0+design$sIVRange[1]
        else
          condition<-condition & (ivr1>sIVRange[1] & ivr1<sIVRange[2])
      }
      if (!is.null(MV2) && design$sIV2RangeOn) {
        if (sIV2Range[1]==sIV2Range[2])
          ivr21<-ivr21*0+sIV2Range[1]
        else
          condition<-condition & (ivr21>sIV2Range[1] & ivr21<sIV2Range[2])
      }
      ivr<-c(ivr, ivr1[condition])
      # if (!is.null(MV2))
        ivr2<-c(ivr2,ivr21[condition])
      dvr_m<-c(dvr_m, dvr1_m[condition])
      dvr_s<-c(dvr_s, dvr1_s[condition])
    } else
    { 
      ivr<-c(ivr,ivr1)
      ivr2<-c(ivr2,ivr21)
      dvr_m<-c(dvr_m,dvr1_m)
      dvr_s<-c(dvr_s,dvr1_s)
    }
  }

  data<-list(ivr=ivr[1:n],ivr2=ivr2[1:n],dvr_m=dvr_m[1:n],dvr_s=dvr_s[1:n])
}

#' make a simulated sample
#' 
#' @returns a sample object
#' @seealso showSample() reportSample()
#' @examples
#' sample<-doSample(hypothesis=makeHypothesis(),design=makeDesign(),autoShow=braw.env$autoShow)
#' @export
doSample<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,autoShow=braw.env$autoShow){
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  effect<-hypothesis$effect
  # if (effect$rSD>0 && effect$rIV!=0) effect$rIV<-tanh(atanh(effect$rIV)+rnorm(1,0,atanh(effect$rSD)))
  
  # check effect sizes before going any further
  fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
  while (fullES>=1) {
    effect$rIV<-effect$rIV*0.9
    effect$rIV2<-effect$rIV2*0.9
    effect$rIVIV2<-effect$rIVIV2*0.9
    effect$rIVIV2DV<-effect$rIVIV2DV*0.9
    fullES<-effect$rIV^2+effect$rIV2^2+2*effect$rIV*effect$rIV2*effect$rIVIV2+effect$rIVIV2DV^2
  }
  
  total1<-effect$rIV+effect$rIV2*effect$rIVIV2
  while (total1>=1) {
    effect$rIV<-effect$rIV*0.9
    effect$rIV2<-effect$rIV2*0.9
    effect$rIVIV2<-effect$rIVIV2*0.9
    total1<-effect$rIV+effect$rIV2*effect$rIVIV2
  }
  total2<-effect$rIV2+effect$rIV*effect$rIVIV2
  while (total2>=1) {
    effect$rIV<-effect$rIV*0.9
    effect$rIV2<-effect$rIV2*0.9
    effect$rIVIV2<-effect$rIVIV2*0.9
    total1<-effect$rIV2+effect$rIV*effect$rIVIV2
  }
  
  rho<-getWorldEffect(1,effect)
  if (effect$rSD>0 && rho!=0) rho<-tanh(atanh(rho)+rnorm(1,0,atanh(effect$rSD)))
  
  n<-design$sN
  if (n<1) {
    if (effect$world$On && rho==0) {
      n<-rw2n(rhoOld,n)
    } else {
    n<-rw2n(rho,n)
    }
  }
  if (design$sNRand) {
    n<-nDistrRand(1,design)
    while (n>100000) {n<-nDistrRand(1,design)}
  }
  n<-round(n)

  if (n==0){
    iv<-array(0,0)  
    dv<-iv
    xplot<-iv
    yplot<-xplot
    sampleRho<-0
    samplePval<-0
    IV<-IV
    IV2<-IV2
    DV<-DV
    
  }  else {
    
    if (design$sMethod$type=="Resample"){
      use=ceiling(runif(n,min=0,max=1)*n)
      id<-1:n
      if (is.null(braw.env$lastSample)) {
        useIV<-match(IV$name,variables$name)
        iv<-braw.res$importedData[[useIV+1]]    
        
        useDV<-match(DV$name,variables$name)
        dv<-braw.res$importedData[[useDV+1]] 
        
        if (!is.null(IV2)) {
          useIV2<-match(IV2$name,variables$name)
          iv2<-braw.res$importedData[[useIV2+1]]    
        } else {
          iv2<-rep(0,length(iv))
        }
        
        braw.env$lastSample$iv<-iv
        braw.env$lastSample$iv2<-iv2
        braw.env$lastSample$dv<-dv
      }
      iv<-braw.env$lastSample$iv[use]
      if (!is.null(IV2)){
        iv2<-braw.env$lastSample$iv2[use]
      } else{
        iv2<-0
      }
      dv<-braw.env$lastSample$dv[use]
      rho<-0
      sampleRho<-0
      samplePval<-0
      
    } else {
      
    if (IV$process=="data" && DV$process=="data"){
      variables<-braw.res$importedData$variables
      importedData<-braw.res$importedData$data
      useIV<-match(IV$name,variables[,"name"])
      useDV<-match(DV$name,variables[,"name"])

      id<-importedData[[1]]
      iv<-importedData[[useIV+1]]    
      dv<-importedData[[useDV+1]]    
      sampleRho<-0
      samplePval<-0
      
      waste<-(is.na(iv) | is.na(dv))

      if (!is.null(IV2)) {
        useIV2<-match(IV2$name,variables[,"name"])
        iv2<-importedData[[useIV2+1]]    
        waste<-waste | is.na(iv2)
      } else {
        iv2<-rep(0,length(iv))
      }
      keep<-!waste
      iv<-iv[keep]
      iv2<-iv2[keep]
      dv<-dv[keep]
      id<-id[keep]
      
      if (variables[useIV,]$type=="Categorical")
      { problem<-FALSE
       cases<-str_split(variables[useIV,]$cases,",")[[1]]
        for (i in 1:variables[useIV,]$ncats){
          if (sum(iv==cases[i])<3) {
            problem<-TRUE
            errorText<-paste("Not enough samples with ", variables[useIV,]$name, "==", cases[i])
            return(NULL)
          }
        }
        }
      # remove duplicates that arise when there is an unused within variable
      # remove duplicated rows (from covariates of within designs)
        waste<-duplicated(data.frame(pt=id,iv=iv,iv2=iv2,dv=dv))
        iv<-iv[!waste]
        iv2<-iv2[!waste]
        dv<-dv[!waste]
        id<-id[!waste]

      # any type conversions required?
        if (is.numeric(iv) && IV$type=="Categorical") {
          iv<-discreteVals(iv,IV$ncats,IV$proportions,"discrete",IV$cases)
        } else {
          if (!is.numeric(iv) && IV$type!="Categorical") {
            iv<-as.numeric(iv)
          }
        }
        if (!is.null(IV2)) {
          if (is.numeric(iv2) && IV2$type=="Categorical") {
            iv2<-discreteVals(iv2,IV2$ncats,IV2$proportions,"discrete",IV2$cases)
          } else {
            if (!is.numeric(iv2) && IV2$type!="Categorical") {
              iv2<-as.numeric(iv2)
            }
          }
        }
        if (is.numeric(dv) && DV$type=="Categorical") {
          dvr<-discreteVals(dv,DV$ncats,DV$proportions,"discrete",DV$cases)
        } else {
          if (!is.numeric(dv) && DV$type!="Categorical") {
            iv<-as.numeric(dv)
          }
        }
        
      rho<-0
      # save the result
      braw.env$lastSample<-list(participant=id, iv=iv, iv2=iv2, dv=dv)
      
    } else {
      
      # make id
      id<-factor(1:n)
      # effect sizes for IV2 (if needed)
      if (!is.null(IV2)){
        rho2<-effect$rIV2
        rho12<-effect$rIVIV2
        rhoInter<-effect$rIVIV2DV
      } else {
        rho2<-0
        rho12<-0
        rhoInter<-0
      }
      
      # make iv
      data<-makeSampleVar(design,effect,n,IV,IV2)
      ivr<-data$ivr
      iv2r<-data$ivr2
      dvr_m<-data$dvr_m
      dvr_s<-data$dvr_s
      
      switch(IV$type,
             "Interval"={
               ivDiscrete<-ivr
             },
             "Categorical"={
               ivDiscrete<-discreteVals(ivr,IV$ncats,IV$proportions)
             },
             "Ordinal"={
               ivDiscrete<-discreteVals(ivr,IV$nlevs,OrdProportions(IV))
             })
      
      if (!is.null(IV2)) {
        switch(IV2$type,
               "Interval"={
                 iv2Discrete<-iv2r
               },
               "Categorical"={
                 iv2Discrete<-discreteVals(iv2r,IV2$ncats,IV2$proportions)
               },
               "Ordinal"={
                 iv2Discrete<-discreteVals(iv2r,IV2$nlevs,OrdProportions(IV2))
               })
      }
      
      if (!is.null(effect$rM1) && !effect$rM1) rho<-0
      if (!is.null(effect$rM2) && !effect$rM2) rho2<-0
      
      # make residuals
      variance_explained=rho^2+rho2^2+rhoInter^2+2*rho*rho2*rho12
      residual<-makeSampleVals(n,0,sqrt(1-variance_explained),DV,effect$ResidDistr)
      residual<-residual*dvr_s+dvr_m

      # non-independence  
      if (design$sDependence>0) {
        dependenceVal=0.1
        change<-round(n*design$sDependence/2)
        if (change>0) {
          ivr[1:change]<-ivr[change+(1:change)]+rnorm(change,0,1)*dependenceVal
          if (!is.null(IV2)) {
            iv2r[1:change]<-iv2r[change+(1:change)]+rnorm(change,0,1)*dependenceVal
            iv12r[1:change]<-iv12r[change+(1:change)]+rnorm(change,0,1)*dependenceVal
          }
          residual[1:change]<-residual[change+(1:change)]+rnorm(change,0,1)*dependenceVal
        }
      }
      
      # switch(IV$type,
      #        "Interval"={
      #        },
      #        "Ordinal"={
      #        },
      #        "Categorical"={
      #          if (IV$catSource=="discrete") {
      #            ivr<-discreteVals(ivr,IV$ncats,IV$proportions,"discrete",IV$cases)
      #          }
      #        }
      # )
      # if (!is.null(IV2)){
      #   switch(IV2$type,
      #          "Interval"={
      #          },
      #          "Ordinal"={
      #          },
      #          "Categorical"={
      #            if (IV2$catSource=="discrete") {
      #              iv2<-discreteVals(iv2r,IV$ncats,IV$proportions)
      #            }
      #          }
      #   )
      # }
      
      # do within design
      if (IV$type=="Categorical" && design$sIV1Use=="Within") {
        b<-drawCatPositions(IV$ncats)
        b<-b/(sd(b)*sqrt((IV$ncats-1)/IV$ncats))
        rsd<-residual

        ivr_new<-c()
        ivDiscrete_new<-c()
        residual<-c()
        for (i in 1:IV$ncats) {
          ivr_new<-c(ivr_new,rep(b[i],n))
          ivDiscrete_new<-c(ivDiscrete_new,rep(b[i],n))
          residual<-c(residual,rsd*sqrt(design$sWithinCor)^2+sqrt(1-design$sWithinCor^2)*rnorm(n))
        }
        ivr<-ivr_new
        ivDiscrete<-ivDiscrete_new
        id<-rep(id,IV$ncats)
        if (!is.null(IV2) && IV2$type=="Categorical") {
          iv2r<-rep(iv2r,IV$ncats)
          iv2Discrete<-rep(iv2Discrete,IV$ncats)
        }
        
        n<-n*IV$ncats
      } 
      
      if (!is.null(IV2) && IV2$type=="Categorical" && design$sIV2Use=="Within") {
        b<-drawCatPositions(IV2$ncats)
        b<-b/(sd(b)*sqrt((IV2$ncats-1)/IV2$ncats))
        rsd<-residual
        
        iv2r_new<-c()
        iv2Discrete_new<-c()
        residual<-c()
        for (i in 1:IV2$ncats) {
          iv2r_new<-c(iv2r_new,rep(b[i],n))
          iv2Discrete_new<-c(iv2Discrete_new,rep(b[i],n))
          residual<-c(residual,rsd*sqrt(design$sWithinCor)^2+sqrt(1-design$sWithinCor^2)*rnorm(n))
        }
        iv2r<-iv2r_new
        iv2Discrete<-iv2Discrete_new
        ivDiscrete<-rep(ivDiscrete,IV2$ncats)
        ivr<-rep(ivr,IV2$ncats)
        id<-rep(id,IV2$ncats)
        
        n<-n*IV2$ncats
      } 
      
      # make the interaction term
      if (!is.null(IV2)) iv12r<-ivr*iv2r
      else               iv12r<-ivr*0
      
      if (effect$Heteroscedasticity!=0){
        localVar<- abs(ivr/3) * sign(ivr)
        residual<-residual*(1+localVar*effect$Heteroscedasticity)
      }
      
      # make dv  
      dvr<- rho*ivr + rho2*iv2r + rhoInter*iv12r + residual
      # proceed  
      sampleRho<-0
      samplePval<-1
      # sampleRho<-cor(ivr,dvr)
      # p<-cor.test(ivr,dvr)
      # samplePval<-p$p.value

      # non-responders
      if (design$sNonResponse>0) {
        change<-round(n*design$sNonResponse)
        dvr[1:change]<-residual[1:change]
      }
      
      # outliers - as errors
      if (design$sOutliers>0) {
        outlierValue=4
        change<-round(n*design$sOutliers)
        dvr[1:change]<-sign(runif(change,-1,1))*outlierValue
      }
      
      # trim DV values
      if (design$sIVRangeOn) {
        keep<-dvr<=design$sDVRange[2] & dvr>=design$sDVRange[1]
        dvr<-dvr[keep]
        ivr<-ivr[keep]
        if (!is.null(IV2))  ivr2<-ivr2[keep]
        id<-id[keep]
      }
    
      switch(IV$type,
             "Interval"={
               iv<-ivr*IV$sd+IV$mu
             },
             "Ordinal"={
               iv<-discreteVals(ivr,IV$nlevs,OrdProportions(IV),"ordinal")
             },
             "Categorical"={
               iv<-discreteVals(ivr,IV$ncats,IV$proportions,"discrete",IV$cases)
             }
      )

      if (!is.null(IV2)) {
      switch(IV2$type,
             "Interval"={
               iv2<-iv2r*IV2$sd+IV2$mu
             },
             "Ordinal"={
               iv2<-discreteVals(iv2r,IV2$nlevs,OrdProportions(IV2),"Ordinal")
             },
             "Categorical"={
               iv2<-discreteVals(iv2r,IV2$ncats,IV2$proportions,"discrete",IV2$cases)
               }
      )
      } else {
        iv2<-iv2r
      }
      
      switch(DV$type,
             "Interval"={
               dv<-dvr*DV$sd+DV$mu
             },
             "Ordinal"={
               dv<-discreteVals(dvr,DV$nlevs,OrdProportions(DV),"Ordinal")
             },
             "Categorical"={
               dv<-discreteVals(dvr,DV$ncats,DV$proportions,"discrete",DV$cases)
             }
      )
      braw.env$lastSample<-list(participant=id, iv=iv, iv2=iv2, dv=dv)
      
    }
    } # end of simulate

    switch(IV$type,
           "Interval"={
             IV<-list(mu=mean(iv),sd=sd(iv),name=IV$name,type=IV$type,vals=iv)
           },
           "Ordinal"={
             IV<-list(mu=IV$median, sd=IV$iqr/2, name=IV$name,type=IV$type,nlevs=IV$nlevs,median=IV$median,iqr=IV$iqr,ordSource=IV$ordSource,vals=iv)
           },
           "Categorical"={
             IV<-list(mu=0, sd=1, name=IV$name,type=IV$type,ncats=IV$ncats,cases=IV$cases,proportions=IV$proportions,vals=iv)
           }
    )
    
    
    if (!is.null(IV2)) {
      switch(IV2$type,
           "Interval"={
             IV2<-list(name=IV2$name,type=IV2$type,mu=mean(iv2),sd=sd(iv2),vals=iv2)
           },
           "Ordinal"={
             IV2<-list(mu=IV2$median, sd=IV2$iqr/2, name=IV2$name,type=IV2$type,nlevs=IV2$nlevs,median=IV2$median,iqr=IV2$iqr,ordSource=IV2$ordSource,vals=iv2)
           },
           "Categorical"={
             IV2<-list(name=IV2$name,type=IV2$type,mu=0, sd=1, ncats=IV2$ncats,cases=IV2$cases,proportions=IV2$proportions,vals=iv2)
           }
    )
    } else{
      IV2<-NULL
    }
    
    switch(DV$type,
           "Interval"={
             DV<-list(mu=mean(dv),sd=sd(dv),name=DV$name,type=DV$type,vals=dv)
           },
           "Ordinal"={
             DV<-list(mu=DV$median, sd=DV$iqr/2, name=DV$name,type=DV$type,nlevs=DV$nlevs,median=DV$median,iqr=DV$iqr,discrete=DV$discrete,vals=dv)
           },
           "Categorical"={
             DV<-list(mu=0, sd=1, name=DV$name,type=DV$type,ncats=DV$ncats,cases=DV$cases,proportions=DV$proportions,vals=dv)
           }
    )
  
    yplot<-dv

    switch(IV$type,
           "Interval"={xplot<-iv},
           "Ordinal"={xplot<-iv},
           "Categorical"={xplot<-match(iv,levels(iv))}
    )
    
    if (IV$type=="Categorical"){
      xp<-xplot
      for (i in 1:IV$ncats) {
        use1=(xp==i)
        if (sum(use1,na.rm=TRUE)>1) {
        if (DV$type=="Interval"){
          mn1=mean(dv[use1])
          sd1=sd(dv[use1])
          xplot[use1]<-i+rnorm(length(xplot[use1]),mean=0,sd=exp(-0.5*((dv[use1]-mn1)/sd1)^2))*0.15*2*sum(use1,na.rm=TRUE)/length(xp)
        } 
          if (DV$type=="Ordinal") {
            for (j in 1:DV$nlevs) {
              use2=(as.numeric(dv)==j)
              mn1<-mean(use2)
              jitter<-runif(length(xplot[use1&use2]),-1,1)*mean(use1&use2)*2
              xplot[use1&use2]<-i+jitter
            }
          }
          if (DV$type=="Categorical") {
            for (j in 1:DV$ncats) {
              use2=(as.numeric(dv)==j)
              mn1<-mean(use2)
              jitter<-runif(length(xplot[use1&use2]),-1,1)*sqrt(mean(use1&use2))/2
              xplot[use1&use2]<-i+jitter
            }
          }
        }
      }
      # xplot<-xplot-(IV$ncats+1)/2
    }
    if (IV$type=="Ordinal"){
      jitter<-runif(length(xplot),-1,1)*0.25
      xplot<-xplot+jitter
    }
    
    if (!is.null(IV2)){
      switch(IV2$type,
             "Interval"={x2plot<-iv2},
             "Ordinal"={x2plot<-iv2},
             "Categorical"={x2plot<-match(iv2,levels(iv2))}
      )
      if (IV2$type=="Categorical"){
        xp2<-x2plot
        for (i in 1:IV2$ncats) {
          use1=(xp2==i)
          if (sum(use1,na.rm=TRUE)>1) {
          if (DV$type=="Interval" || DV$type=="Ordinal"){
            mn1=mean(dv[use1])
            sd1=sd(dv[use1])
            x2plot[use1]<-i+rnorm(length(x2plot[use1]),mean=0,sd=exp(-0.5*((dv[use1]-mn1)/sd1)^2))*0.15*2*sum(use1,na.rm=TRUE)/length(x2plot)
          } else {
            x2plot[use1]<-i+runif(length(x2plot[use1]),-1,1)*mean(use1)*0.5
          }
          }
        }
      }
    } else {x2plot=iv2}
    
    switch(DV$type,
           "Interval"={yplot<-dv},
           "Ordinal"={yplot<-dv},
           "Categorical"={yplot<-match(dv,levels(dv))-1}
    )
    
    if (DV$type=="Ordinal"){
      jitter<-runif(length(xplot),-1,1)*0.125
      yplot<-yplot+jitter
    }
    
    if (DV$type=="Categorical"){
      for (i in 1:DV$ncats) {
        use1=(as.numeric(dv)==i)
        if (IV$type=="Interval"){
          mn1<-mean(iv[use1])
          sd1<-sd(iv[use1])
          jitter<-rnorm(length(yplot[use1]),mean=0,sd=exp(-0.5*((iv[use1]-mn1)/sd1)^2))*0.15*2*sum(use1,na.rm=TRUE)/length(yplot)
          yplot[use1]<-yplot[use1]+jitter
        } 
        if (IV$type=="Ordinal") {
          for (j in 1:IV$nlevs) {
            use2=(as.numeric(iv)==j)
            mn1<-mean(use2)
            jitter<-runif(length(yplot[use1&use2]),-1,1)*mean(use1&use2)*2
            yplot[use1&use2]<-yplot[use1&use2]+jitter
          }
        }
        if (IV$type=="Categorical") {
          for (j in 1:IV$ncats) {
            use2=(as.numeric(iv)==j)
            mn1<-mean(use2)
            jitter<-runif(length(yplot[use1&use2]),-1,1)*sqrt(mean(use1&use2))/2
            yplot[use1&use2]<-yplot[use1&use2]+jitter
          }
        }
      }
      # yplot<-yplot-(DV$ncats+1)/2
    }
  } 
  
  sample<-list(participant=id, iv=iv,iv2=iv2, dv=dv,ivplot=xplot,iv2plot=x2plot,dvplot=yplot,
               sampleRho=sampleRho,samplePval=samplePval,effectRho=rho,nval=design$sN,
               hypothesis=hypothesis, design=design)
  if (autoShow) print(showSample(sample))
  setBrawRes("result",sample)
  sample
}
