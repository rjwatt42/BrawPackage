
drawCatPositions<-function(ncats){
  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-exp(-qnorm(pbreaks)^2/2)
  -1/sqrt(2*pi)*diff(ebreaks)/diff(pbreaks)
}

makeSampleVals<-function(n,mn,sdv,MV,distr="normal"){
  switch (distr,
          "normal"= {
            ivr<-rnorm(n,0,1)
            if (MV$type=="Interval" && (MV$skew!=0 || MV$kurtosis!=3)){
              if (MV$kurtosis<1.05) MV$kurtosis<-1.05
              change<-MV$skew!=0 & (MV$kurtosis-3)>MV$skew^2
              MV$kurtosis[change]<-MV$skew[change]^2 + 3
              
              a<-f_johnson_M(0,1,MV$skew,MV$kurtosis)
              ivr<-f_johnson_z2y(ivr,a$coef,a$type)
              # ivr<-rJohnson(n,parms=a)
            }
            ivr*sdv+mn
          },
          "skewed"={
            skew<-2
            kurtosis<-3
            ivr<-rnorm(n,0,1)
            a<-f_johnson_M(0,sdv,skew,kurtosis)
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

makeSampleVar<-function(design,n,MV){
  ivr=c()
  dvr_s<-c()
  dvr_m<-c()
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
                ivr<-ivr[sample(length(ivr1),length(ivr1))]
              }
              dvr1_m<-rep(0,n)
              dvr1_s<-rep(1,n)
            },
            {
              method<-design$sMethod
              ivr1<-c()
              dvr1_m<-c()
              dvr1_s<-c()
              
              nClusts<-ceil(n/method$Cluster_n/method$Contact_n)
              Main_rad<-sqrt(1-method$Cluster_rad^2)*method$Main_rad
              for (i in 1:nClusts) {
                # location of cluster
                rad_new<-rnorm(1,0,Main_rad)
                dir_new<-pi+runif(1,0,2*pi)
                x_cluster_centre<-cos(dir_new)*rad_new
                y_cluster_centre<-sin(dir_new)*rad_new
                
                for (j in 1:method$Cluster_n) {
                  # location of contact group
                  rad_new<-rnorm(1,0,method$Cluster_rad)
                  dir_new<-pi+rnorm(1,atan2(y_cluster_centre,x_cluster_centre),pi*0.5)
                  x_contact<-x_cluster_centre+cos(dir_new)*rad_new
                  y_contact<-y_cluster_centre+sin(dir_new)*rad_new
                  
                  # track any contacts
                  for (k in 1:method$Contact_n) {
                    ivr1<-c(ivr1,x_contact)
                    dvr1_m<-c(dvr1_m,y_contact)
                    rad_new<-rnorm(1,0,method$Contact_rad)
                    dir_new<-pi+rnorm(1,atan2(y_contact,x_contact),pi*0.5)
                    x_contact<-x_contact+cos(dir_new)*rad_new
                    y_contact<-y_contact+sin(dir_new)*rad_new
                  }
                }
              }
              
              use<-sample(length(ivr1),length(ivr1))
              ivr1<-ivr1[use]
              dvr1_m<-dvr1_m[use]
              dvr1_s<-rep(0,n)
            }
    )

    if (design$sRangeOn) {
      ivr<-c(ivr, ivr1[ivr1>design$sIVRange[1] & ivr1<design$sIVRange[2]])
      dvr_m<-c(dvr_m, dvr1_m[ivr1>design$sIVRange[1] & ivr1<design$sIVRange[2]])
      dvr_s<-c(dvr_s, dvr1_s[ivr1>design$sIVRange[1] & ivr1<design$sIVRange[2]])
    } else
    { 
      ivr<-c(ivr,ivr1)
      dvr_m<-c(dvr_m,dvr1_m)
      dvr_s<-c(dvr_s,dvr1_s)
    }
  }
  data<-list(ivr=ivr[1:n],dvr_m=dvr_m[1:n],dvr_s=dvr_s[1:n])
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
  
  rho<-effect$rIV
  
  if (effect$world$worldOn) {
    if (!is.na(effect$world$populationRZ) && !isempty(effect$world$populationRZ)){
      switch (effect$world$populationRZ,
              "r"={
                switch (effect$world$populationPDF,
                        "Single"={rho<-effect$world$populationPDFk},
                        "Double"={rho<-effect$world$populationPDFk*sign(runif(1,-1,1))},
                        "Uniform"={rho<-runif(1,min=-1,max=1)},
                        "Exp"={rho<-rexp(1,rate=1/effect$world$populationPDFk)*sign((runif(1)*2-1))},
                        "Gauss"={rho<-rnorm(1,mean=0,sd=effect$world$populationPDFk)*sign((runif(1)*2-1))},
                        ">"={rho<-runif(1,min=effect$world$populationPDFk,max=1)*sign(runif(1,min=-1,max=1))},
                        "<"={rho<-runif(1,min=-1,max=1)*effect$world$populationPDFk}
                )
              },
              "z"={
                switch (effect$world$populationPDF,
                        "Single"={rho<-effect$world$populationPDFk},
                        "Double"={rho<-effect$world$populationPDFk*sign(runif(1,-1,1))},
                        "Uniform"={rho<-runif(1,min=-uniformZrange,max=uniformZrange)},
                        "Exp"={rho<-rexp(1,rate=1/effect$world$populationPDFk)*sign((runif(1)*2-1))},
                        "Gauss"={rho<-rnorm(1,mean=0,sd=effect$world$populationPDFk)*sign((runif(1)*2-1))},
                        ">"={rho<-runif(1,min=effect$world$populationPDFk,max=10)*sign(runif(1,min=-1,max=1))},
                        "<"={rho<-runif(1,min=-1,max=1)*effect$world$populationPDFk}
                )
                rho<-tanh(rho)
              }
      )
      rhoOld<-rho
      if (effect$world$populationNullp>0) {
        if (runif(1)<=effect$world$populationNullp)
        {rho<-0}
      }
      rho<-max(min(rho,0.99),-0.99)
    }
  }
  
  n<-design$sN
  if (n<1) {
    if (effect$world$worldOn && rho==0) {
      n<-rw2n(rhoOld,n)
    } else {
    n<-rw2n(rho,n)
    }
  }
  if (design$sNRand) {
    n<-braw.env$minN+rgamma(1,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)
    while (n>100000) {n<-braw.env$minN+rgamma(1,shape=design$sNRandK,scale=(design$sN-braw.env$minN)/design$sNRandK)}
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
          ivr<-iv
          ng<-IV$ncats
          pp<-as.numeric(unlist(strsplit(IV$proportions,",")))
          if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
          proportions<-c(0,pp)
          breaks<-qnorm(cumsum(proportions)/sum(proportions))
          vals=ivr*0
          for (i in 1:IV$ncats) {vals=vals+(ivr>breaks[i])}
          iv<-factor(vals,levels=1:IV$ncats,labels=strsplit(IV$cases,",")[[1]])
        } else {
          if (!is.numeric(iv) && IV$type!="Categorical") {
            iv<-as.numeric(iv)
          }
        }
        if (!is.null(IV2)) {
        if (is.numeric(iv2) && IV2$type=="Categorical") {
          iv2r<-iv2
          ng<-IV2$ncats
          pp<-as.numeric(unlist(strsplit(IV2$proportions,",")))
          if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
          proportions<-c(0,pp)
          breaks<-qnorm(cumsum(proportions)/sum(proportions))
          vals=iv2r*0
          for (i in 1:IV2$ncats) {vals=vals+(iv2r>breaks[i])}
          iv12<-factor(vals,levels=1:IV2$ncats,labels=strsplit(IV2$cases,",")[[1]])
        } else {
          if (!is.numeric(iv2) && IV2$type!="Categorical") {
            iv2<-as.numeric(iv2)
          }
        }
        }
        if (is.numeric(dv) && DV$type=="Categorical") {
          dvr<-dv
          ng<-DV$ncats
          pp<-as.numeric(unlist(strsplit(DV$proportions,",")))
          if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
          proportions<-c(0,pp)
          breaks<-qnorm(cumsum(proportions)/sum(proportions))
          vals=dvr*0
          for (i in 1:DV$ncats) {vals=vals+(dvr>breaks[i])}
          dv<-factor(vals,levels=1:DV$ncats,labels=strsplit(DV$cases,","[[1]]))
        } else {
          if (!is.numeric(dv) && DV$type!="Categorical") {
            iv<-as.numeric(dv)
          }
        }
        
      rho<-0
      # save the result
      braw.env$lastSample<-list(participant=id, iv=iv, iv2=iv2, dv=dv)
      
    } else {
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
      
      # make id
      id<-factor(1:n)
      
      # make iv
      data<-makeSampleVar(design,n,IV)
      ivr<-data$ivr
      dvr_m<-data$dvr_m
      dvr_s<-data$dvr_s
      
      # make iv2 (if needed)
      if (!is.null(IV2)){
        rho2<-effect$rIV2
        rho12<-effect$rIVIV2
        ivr2_resid<-makeSampleVals(n,0,sqrt(1-rho12^2),IV2)
        iv2r<-ivr*rho12+ivr2_resid
      } else {
        rho2<-0
        rho12<-0
        iv2r<-0
      }
      
      # make the interaction term
      if (!is.null(IV2)){
        rhoInter<-effect$rIVIV2DV
        iv12r<-ivr*iv2r
      } else {
        iv12r<-ivr*0
        rhoInter<-0
      }
      
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
      
      switch(IV$type,
             "Interval"={
             },
             "Ordinal"={
             },
             "Categorical"={
               if (IV$catSource=="discrete") {
                 ng<-IV$ncats
                 pp<-IV$proportions
                 # pp<-as.numeric(unlist(strsplit(IV$proportions,",")))
                 if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
                 proportions<-c(0,pp)
                 breaks<-qnorm(cumsum(proportions)/sum(proportions))
                 # not sure we should do this.
                 while (all(ivr<breaks[2])) breaks[2]<-breaks[2]-0.1
                 while (all(ivr>breaks[ng])) breaks[ng]<-breaks[ng]+0.1
                 vals=ivr*0
                 for (i in 1:IV$ncats) {vals=vals+(ivr>breaks[i])}
                 ivr<-(vals-(IV$ncats+1)/2)/std((1:IV$ncats),1)
               }
             }
      )
      if (!is.null(IV2)){
        switch(IV2$type,
               "Interval"={
               },
               "Ordinal"={
               },
               "Categorical"={
                 if (IV2$catSource=="discrete") {
                   ng<-IV2$ncats
                   pp<-IV2$proportions
                   if (is.character(pp))
                     pp<-as.numeric(unlist(strsplit(IV2$proportions,",")))
                   if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
                   proportions<-c(0,pp)
                   breaks<-qnorm(cumsum(proportions)/sum(proportions))
                   vals=iv2r*0
                   for (i in 1:IV2$ncats) {vals=vals+(iv2r>breaks[i])}
                   iv2r<-(vals-(IV2$ncats+1)/2)/std((1:IV2$ncats),1)
                 }
               }
        )
      }
      
      # do within design
      if (IV$type=="Categorical" && design$sIV1Use=="Within") {
        b<-drawCatPositions(IV$ncats)
        b<-b/(sd(b)*sqrt((IV$ncats-1)/IV$ncats))
        rsd<-residual

        ivr_new<-c()
        iv2r_new<-c()
        residual<-c()
        for (i in 1:IV$ncats) {
          ivr_new<-c(ivr_new,rep(b[i],n))
          if (!is.null(IV2)){iv2r_new<-c(iv2r_new,iv2r)} else {iv2r_new<-0}
          residual<-c(residual,rsd*design$sWithinCor+sqrt(1-design$sWithinCor^2)*rnorm(n,0,sqrt(1-rho^2)))
        }
        ivr<-ivr_new
        iv2r<-iv2r_new
        id<-rep(id,IV$ncats)
        
        n<-n*IV$ncats
      } 
      
      if (!is.null(IV2) && IV2$type=="Categorical" && design$sIV2Use=="Within") {
        b<-drawCatPositions(IV2$ncats)
        b<-b/(sd(b)*sqrt((IV2$ncats-1)/IV2$ncats))
        rsd<-residual
        
        ivr_new<-c()
        iv2r_new<-c()
        residual<-c()
        for (i in 1:IV2$ncats) {
          iv2r_new<-c(iv2r_new,rep(b[i],n))
          ivr_new<-c(ivr_new,ivr)
          residual<-c(residual,rsd*design$sWithinCor+sqrt(1-design$sWithinCor^2)*rnorm(n,0,sqrt(1-rho^2)))
        }
        ivr<-ivr_new
        iv2r<-iv2r_new
        id<-rep(id,IV2$ncats)
        
        n<-n*IV2$ncats
      } 
      
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
      if (design$sRangeOn) {
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
                 pp<-OrdProportions(IV)
                 ng<-IV$nlevs
                 if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
                 proportions<-c(0,pp)
                 breaks<-qnorm(cumsum(proportions)/sum(proportions))
                 vals=ivr*0
                 for (i in 1:ng) {vals=vals+(ivr>breaks[i])}
                 if (!IV$ordSource=="discrete") {
                   vals=vals+runif(length(vals),min=-0.5,max=0.5)
                 }
                 iv<-vals
             },
             "Categorical"={
               if (IV$catSource=="continuous") {
                 ng<-IV$ncats
                 pp<-IV$proportions
                 # pp<-as.numeric(unlist(strsplit(IV$proportions,",")))
                 if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
                 proportions<-c(0,pp)
                 breaks<-qnorm(cumsum(proportions)/sum(proportions))
                 vals=ivr*0
                 for (i in 1:IV$ncats) {vals=vals+(ivr>breaks[i])}
                 iv<-factor(vals,levels=1:IV$ncats,labels=IV$cases)
                 # iv<-factor(vals,levels=1:IV$ncats,labels=strsplit(IV$cases,",")[[1]])
               } else {
                 ivr<-ivr*std((1:IV$ncats),1)+(IV$ncats+1)/2
                 iv<-factor(ivr,levels=1:IV$ncats,labels=IV$cases)
                 # iv<-factor(ivr,levels=1:IV$ncats,labels=strsplit(IV$cases,",")[[1]])
               }
             }
      )

      if (!is.null(IV2)) {
      switch(IV2$type,
             "Interval"={
               iv2<-iv2r*IV2$sd+IV2$mu
             },
             "Ordinal"={
               pp<-OrdProportions(IV2)
               ng<-IV2$nlevs
               if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
               proportions<-c(0,pp)
               breaks<-qnorm(cumsum(proportions)/sum(proportions))
               vals=iv2r*0
               for (i in 1:ng) {vals=vals+(iv2r>breaks[i])}
               if (!IV2$ordSource=="discrete") {
                 vals=vals+runif(length(vals),min=-0.5,max=0.5)
               }
               iv2<-vals
             },
             "Categorical"={
               if (IV$catSource=="continuous") {
                 ng<-IV2$ncats
                 pp<-IV2$proportions
                 # pp<-as.numeric(unlist(strsplit(IV2$proportions,",")))
                 if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
               proportions<-c(0,pp)
               breaks<-qnorm(cumsum(proportions)/sum(proportions))
               vals=iv2r*0
               for (i in 1:IV2$ncats) {vals=vals+(iv2r>breaks[i])}
               iv2<-factor(vals,levels=1:IV2$ncats,labels=IV2$cases)
               # iv2<-factor(vals,levels=1:IV2$ncats,labels=strsplit(IV2$cases,",")[[1]])
               } else {
                 iv2r<-iv2r*std((1:IV2$ncats),1)+(IV2$ncats+1)/2
                 iv2<-factor(iv2r,levels=1:IV2$ncats,labels=IV2$cases)
                 # iv2<-factor(iv2r,levels=1:IV2$ncats,labels=strsplit(IV2$cases,",")[[1]])
               }
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
               pp<-OrdProportions(DV)
               ng<-DV$nlevs
               if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
               proportions<-c(0,pp)
               breaks<-qnorm(cumsum(proportions)/sum(proportions))
               vals=dvr*0
               for (i in 1:ng) {vals=vals+(dvr>breaks[i])}
               if (!DV$ordSource=="discrete") {
                 vals=vals+runif(length(vals),min=-0.5,max=0.5)
               }
               dv<-vals
             },
             "Categorical"={
               pp<-DV$proportions
               # pp<-as.numeric(unlist(strsplit(DV$proportions,",")))
               ng<-DV$ncats
               if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
               proportions<-c(0,pp)
               breaks<-qnorm(cumsum(proportions)/sum(proportions))
               vals=dvr*0
               for (i in 1:ng) {vals=vals+(dvr>breaks[i])}
               dv<-factor(vals,levels=1:ng,labels=DV$cases)
               # dv<-factor(vals,levels=1:ng,labels=strsplit(DV$cases,",")[[1]])
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
              jitter<-runif(length(xplot[use1&use2]),-1,1)*mean(use1&use2)
              xplot[use1&use2]<-i+jitter
            }
          }
          if (DV$type=="Categorical") {
            for (j in 1:DV$ncats) {
              use2=(as.numeric(dv)==j)
              mn1<-mean(use2)
              jitter<-runif(length(xplot[use1&use2]),-1,1)*mean(use1&use2)
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
           "Categorical"={yplot<-length(levels(dv))+1-match(dv,levels(dv))}
    )
    
    if (DV$type=="Ordinal" && IV$type=="Ordinal"){
      xplot<-xplot+rnorm(length(xplot))*0.05
      yplot<-yplot+rnorm(length(yplot))*0.05
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
            jitter<-runif(length(yplot[use1&use2]),-1,1)*mean(use1&use2)
            yplot[use1&use2]<-yplot[use1]+jitter
          }
        }
        if (IV$type=="Categorical") {
          for (j in 1:IV$ncats) {
            use2=(as.numeric(iv)==j)
            mn1<-mean(use2)
            jitter<-runif(length(yplot[use1&use2]),-1,1)*mean(use1&use2)
            yplot[use1&use2]<-yplot[use1]+jitter
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
  sample
}
