fit_sem_model<-function(pathmodel,model_data) {

  nloop<-1

  stages<-pathmodel$path$stages
  stages<-stages[!sapply(stages,isempty)]
  n_stages<-length(stages)
  if (n_stages==0) return(NULL)
  m_stages<-max(sapply(stages,length))

  pathLocalModel<-matrix(0,n_stages,m_stages)
  sem<-path2sem(pathmodel,model_data)

  n_obs<-nrow(sem$data)
  P<-sem$P
  Q<-sem$Q

  # from here onwards we need these variables:
  # Ldesign Bdesign
  # data
  # endogenous exogenous

  Lstart<-zeros(1,sum(sem$Ldesign!=0))
  Bstart<-zeros(1,sum(sem$Bdesign!=0))
  LBstart<-c(Lstart, Bstart)

  nan_action<-"pairwise.complete.obs" # "complete.obs"
  use<-1:n_obs

  L<-matrix(0,nloop,1)
  B<-matrix(0,nloop,1)

  for (loop in 1:nloop) {
    use_data<-sem$data[use,]

    S<-cov(use_data[,c(sem$endogenous, sem$exogenous)],use=nan_action)

    if (length(sem$exogenous)>1) phi<-cov(use_data[,sem$exogenous],use=nan_action)
    else phi<-matrix(var(use_data[,sem$exogenous]))
    if (length(sem$endogenous)>1) {
      psy<-cov(use_data[,sem$endogenous],use=nan_action)
      psy=diag(diag(psy))
    } else psy<-matrix(var(use_data[,sem$endogenous]))
    
    az1<-get_ml_fitfcn(LBstart,S=S,phi=phi,psy=psy,Ldesign=sem$Ldesign,Bdesign=sem$Bdesign,data=sem$data)
    if (length(LBstart)>1) {
      out<-optim(LBstart,get_ml_fitfcn,gr=NULL,S=S,phi=phi,psy=psy,Ldesign=sem$Ldesign,Bdesign=sem$Bdesign,data=sem$data)
      LBend<-out$par
    } else {
      out<-fminbnd(get_ml_fitfcn,a=-10,b=10,S=S,phi=phi,psy=psy,Ldesign=sem$Ldesign,Bdesign=sem$Bdesign,data=sem$data)
      LBend<-out$xmin
    }
    fVal<-get_ml_fitfcn(LBend,S=S,phi=phi,psy=psy,Ldesign=sem$Ldesign,Bdesign=sem$Bdesign,data=sem$data,debug=TRUE)

    Ln<-sum(sem$Ldesign!=0)
    Bn<-sum(sem$Bdesign!=0)
    Lend<-LBend[1:Ln]
    Bend<-LBend[Ln+(1:Bn)]
    if (loop==1) {
      L<-matrix(Lend,nrow=nloop,byrow=TRUE)
      B<-matrix(Bend,nrow=nloop,byrow=TRUE)
      Fm<-matrix(fVal,nrow=nloop,byrow=TRUE)
    } else {
      L[loop,]<-Lend
      B[loop,]<-Bend
      Fm[loop,1]<-out$value
    }

    use<-ceiling(rand(n_obs,1)*n_obs)
  }

  sem$Fmin<-Fm[1,1]
  sem$loglike<-get_ml_fitfcn(LBend,S,phi,psy,sem$Ldesign,sem$Bdesign,sem$data)

  L_est<-L[1,]
  B_est<-B[1,]

  sem$Lresult<-sem$Ldesign
  sem$Bresult<-sem$Bdesign

  Luse<-sem$Ldesign==1
  Buse<-sem$Bdesign==1
  sem$Lresult[Luse]<-L_est
  sem$Lresult[!Luse]<-NA
  sem$Bresult[Buse]<-B_est
  sem$Bresult[!Buse]<-NA

  B<-sem$Bresult
  B[is.na(B)]<-0
  L<-sem$Lresult
  L[is.na(L)]<-0
  Y<-t(sem$data[,1:P])
  Y[is.na(Y)]<-0
  X<-t(sem$data[,(P+1):ncol(sem$data)])
  X[is.na(X)]<-0
  Z<-B%*%Y+L%*%X
  Yactual<-Y-matrix(rep(rowMeans(Y),ncol(Y)),ncol=ncol(Y))
  Zactual<-Z-matrix(rep(rowMeans(Z),ncol(Z)),ncol=ncol(Z))
  error<-t(Yactual-Zactual)
  Rsquared<-1-var(error)/var(t(Yactual))


  sem$coefficients=zeros(P,P+Q)
  for (iw in 1:P)
    for (ix in 1:Q)
      if (!is.na(sem$Lresult[iw,ix]))
        sem$coefficients[iw,ix+P]=sem$Lresult[iw,ix]

  for (inn in 1:P)
    if (!is.na(sem$Bresult[iw,inn]))
      sem$coefficients[iw,inn]=sem$Bresult[iw,inn]

  switch(pathmodel$path$depth,
         'd1'= depth<-1,
         'd2'= depth<-2,
         'all'= depth<-length(stages)
  )

  path<-c()
  for (is in 2:length(pathmodel$path$stages)) {
    thisStage<-pathmodel$path$stages[[is]]
    if (!all(sapply(thisStage,isempty))) {
      use_stages<-is-(1:depth)
      use_stages<-use_stages[use_stages<1]
      predictors<-stages[use_stages]
      for (iv in 1:length(thisStage)) {
        thisVariable<-thisStage[iv]
        use_vars=which(is.element(model_data$varnames,c(predictors, thisVariable)))
        mdl<-list(
         ResponseName=thisVariable,
        PredictorNames=predictors,
        VariableNames=model_data$varnames[use_vars],
        IsCategorical<-model_data$varcat[use_vars],
        # 
        # for (ip in 1:length(use_vars)) {
        #   d<-model_data$data[,use_vars[ip]]
        #   d<-d[!is.na(d)]
        #   if (model_data$varcat(use_vars[ip]))
        #     mdl$VariableInfo$Range[ip]<-unique(d)
        #   else
        #     mdl$VariableInfo$Range[ip]<-c(min(d),max(d))
        # }
        # # safe_name<-model_data.varnames[use_vars[ip]]
         Data=model_data$data[,use_vars]
        )
      }

      # this is complicated
      use_y=which(mdl$ResponseName==sem$endo_names)
      if (!isempty(use_y)) {
        use_x<-which(is.element(c(sem$endo_names,sem$exo_names),sem$stages[use_stages]))
        mdl$Coefficients<-sem$coefficients[use_y,use_x]
        mdl$NumObservations<-nrow(model_data$data)
        mdl$Rsquared.Ordinary<-Rsquared[is-1]
      } else {
        mdl$ResponseName<-''
        mdl$PredictorNames<-c()
        mdl$Coefficients<-c()
        mdl$NumObservations<-0
        mdl$Rsquared$Ordinary<-NA
      }
      # pathLocalModel[is,iv]<-mdl
    }
  }
  sem$mdl<-pathLocalModel


  sem$P<-P
  sem$Q<-Q
  sem$n_obs<-n_obs
  sem$nan_action<-nan_action
  # return(sem)
  
  sem<-sem_results(pathmodel,sem)

  return(sem)
}


get_ml_fitfcn<-function(LB,S,phi,psy,Ldesign,Bdesign,data,debug=FALSE) {

  P<-nrow(psy) 
  if (is.null(P)) P<-1
  Q<-nrow(phi)

  Ln<-sum(Ldesign!=0)
  Bn<-sum(Bdesign!=0)
  L<-LB[1:Ln]
  B<-LB[Ln+(1:Bn)]

  Ldesign[Ldesign==1]<-L
  Bdesign[Bdesign==1]<-B

  Y<-t(data[,1:P])
  X<-t(data[,(P+1):ncol(data)])
  z<-Bdesign%*%Y+Ldesign%*%X
  error<-t(Y-z)
  
  if (ncol(error)>1)  psy<-diag(diag(var(error,na.rm=TRUE)))
  else psy=matrix(diag(var(error,na.rm=TRUE)))
  
  Stheta<-get_Stheta(Ldesign,Bdesign,phi,psy)
  ml<-log(det(Stheta))+sum(diag(S%*%inv(Stheta)))-log(det(S))- (P+Q)
  return(ml)
}

get_Stheta<-function(L,B=NULL,phi=NULL,psy=NULL) {
  
  nan_action<-"pairwise.complete.obs" # "complete.obs"
  
  if (is.null(B)) {
    sem=L;
    L=sem$Lresult; L[is.na(L)]=0;
    B=sem$Bresult; B[is.na(B)]=0;
    data=sem$data;
    P=sem$P;
    Q=sem$Q;
    if (length(sem$exogenous)>1) phi<-cov(data[,sem$exogenous],use=nan_action)
    else phi<-matrix(1)

    Y<-t(data[,1:P])
    X<-t(data[,(P+1):ncol(data)])
    z<-B%*%Y+L%*%X
    error<-t(Y-z)
    if (ncol(error)>1) psy<-diag(diag(var(error,na.rm=TRUE)))
    else psy<-matrix(var(error,na.rm=TRUE))
  }
  P<-nrow(psy)
  Q<-nrow(phi)

  term1<-eye(nrow(B))-B
  term2<-L%*%phi%*%t(L)+psy
  term3<-t(inv(term1))

  SYYtheta<-(solve((term1),(term2)))%*%term3
  SXXtheta<-phi
  SYXtheta<-phi%*%t(L)%*%term3
  Stheta<-rbind(cbind(SYYtheta, zeros(P,Q)),cbind(SYXtheta, SXXtheta))
  Stheta<-tril(Stheta)+t(tril(Stheta,-1))

}

sample2ModelData<-function(sample) {
  data<-cbind(sample$participant,sample$dv,sample$iv)
  varnames<-c("ID",sample$hypothesis$DV$name,sample$hypothesis$IV$name)
  varcats<-c(FALSE,c(sample$hypothesis$DV$type,sample$hypothesis$IV$type)=="Categorical")
  if (!is.null(sample$hypothesis$IV2)) {
    data<-cbind(data,sample$iv2)
    varnames<-c(varnames,sample$hypothesis$IV2$name)
    varcats<-c(varcats,sample$hypothesis$IV2$type=="Categorical")
  }
  return(list(data=data,varnames=varnames,varcats=varcats))
}

path2sem<-function(pathmodel,model_data) {

  # firstly get all the details local
  stages<-pathmodel$path$stages
  stages<-stages[!sapply(stages,isempty)]

  using<-is.element(model_data$varnames,unlist(stages))

  full_data<-model_data$data
  original_vartypes<-model_data$varcat
  original_varnames<-model_data$varnames
  full_vartypes<-model_data$varcat
  full_varnames<-model_data$varnames
  
  only_ivs<-pathmodel$path$only_ivs
  only_dvs<-pathmodel$path$only_dvs
  within_stage<-pathmodel$path$within_stage

  switch(pathmodel$path$depth,
         'd1'= depth<-1,
         'd2'= depth<-2,
         'all'= depth<-length(stages)
  )

  # now we expand Categorical variables
  new_data<-matrix(nrow=nrow(full_data),ncol=0)
  new_names<-c()
  for (iv in 1:length(full_vartypes)) {
    if (!full_vartypes[iv]) {
      nv<-full_data[,iv]
      new_data<-cbind(new_data,nv)
      new_names<-c(new_names,full_varnames[iv])
    } else {
      cases<-unique(full_data[,iv])
      cases<-cases[!is.na(cases)]
      nv<-zeros(nrow(full_data),length(cases)-1)
      nv[is.na(full_data[,iv]),]<-NA
      for (ic in 2:length(cases)) {
        nv[,ic-1]<-unlist(full_data[,iv])==cases[ic]
      }
      nn<-paste0(full_varnames[iv],'=',cases[2:length(cases)])
      # colnames(nv)<-nn
      new_data<-cbind(new_data,nv)
      new_names<-cbind(new_names,nn)
      for (is in 1:length(stages)){
        s<-stages[[is]]
        change<-which(full_varnames[iv]==s)
        if (!isempty(change)) {
          new_s<-c()
          if (change>1) new_s<-s[1:(change-1)]
          new_s<-c(new_s,nn)
          if (change<length(s)) new_s<-c(new_s,s[(change+1):length(s)])
          stages[[is]]<-new_s
        }
      }
      s<-only_ivs
      change<-which(full_varnames[iv]==s)
      if (!isempty(change))
        only_ivs<-cbind(only_ivs[1:(change-1)],nn,only_ivs[(change+1):length(s)])
      s<-only_dvs
      change<-which(full_varnames[iv]==s)
      if (!isempty(change))
        only_dvs<-cbind(only_dvs[1:(change-1)],nn,only_dvs[(change+1):length(s)])
    }
  }
  full_varnames<-new_names
  full_data<-new_data
  colnames(full_data)<-new_names

  exo_names<-stages[[1]]
  endo_names<-c()
  for (ist in 2:length(stages)) {
    dests<-stages[[ist]]
    for (iv in 1:length(dests)) {
      if (!isempty(dests[iv])){
        if (is.element(dests[iv],pathmodel$path$only_ivs))
          exo_names<-c(exo_names,dests[iv])
        else
          endo_names<-rbind(endo_names,dests[iv])
      }
    }
  }
  exo_names<-unique(exo_names)
  endo_names<-unique(endo_names)
  varnames<-c(endo_names,exo_names)
  P<-length(endo_names)
  Q<-length(exo_names)
  endogenous<-1:P
  exogenous<-P+(1:Q)

  use<-c()
  for (i in 1:length(varnames)) {
    use<-c(use,which(full_varnames==varnames[i]))
  }
  data<-full_data[,use]


  Bdesign<-zeros(P,P); rownames(Bdesign)<-endo_names; colnames(Bdesign)<-endo_names
  Ldesign<-zeros(P,Q); rownames(Ldesign)<-endo_names; colnames(Ldesign)<-exo_names
  Bresult<-Bdesign+NA
  Lresult<-Ldesign+NA

  models=pathmodel$path$mdl
  if (!is.null(models)) {
    for (i1 in 1:nrow(models)){
      for (i2 in 1:ncol(models)){
        if (isempty(models[i1,i2])){
          models[i1,i2]=list(ResponseName="");
        }
      }
    }
  }

  # now we build the SEM structures: LDesign and BDesign
  for (ist in 2:length(stages)) {
    use_stages=ist-(1:depth)
    use_stages<-use_stages[use_stages>0]
    sources=unlist(stages[use_stages])
    dests=stages[ist][[1]]
    if (!isempty(models)){
      rnames=models[ist,]$ResponseName
    }
    for (iv in 1:length(dests)) {
      if (!isempty(dests[iv]) && !is.element(dests[iv],exo_names)) {
        iDest=which(dests[iv]==endo_names);
        if (!is.null(pathmodel$path$mdl) && ~isempty(pathmodel$path$mdl)){
          use=which(dests[iv]==rnames)
          q=pathmodel$path$mdl[ist,use]$Coefficients;
          coefficient_names=q$Properties$RowNames;
          coefficient_estimates=q$Estimate;
        } else {
          coefficient_names=c()
        }
        for (iso in 1:length(sources)){
          if (any(is.element(sources[iso],exo_names))){
            iSource=which(sources[iso]==exo_names)
            Ldesign[iDest,iSource]=1;
            if (!isempty(coefficient_names)){
              iSource1=which(sources[iso]==coefficient_names);
              Lresult[iDest,iSource]=coefficient_estimates[iSource1];
            }
          } else {
            iSource=which(sources[iso]==endo_names);
            Bdesign[iDest,iSource]=1;
            if (!isempty(coefficient_names)){
              iQ1=which(sources[iso]==coefficient_names);
              Bresult[iDest,iSource]=coefficient_estimates[iQ1];
            }
          }
        }
      }
    }

    if (within_stage==1){
      for (iso1 in 1:length(dests)){
        iQ1=which(sources[iso1]==endo_names);
        for (iso2 in (iso1+1):length(dests)){
          iQ2=which(dests[iso2]==endo_names);
          if (iQ2>0){
            Bdesign[iQ2,iQ1]=1;
            Bresult[iQ2,iQ1]=1;
          }
        }
      }
    }
  }
  
  if (!is.null(pathmodel$path$add) && !isempty(pathmodel$path$add)) {
    for (iadd in 1:length(pathmodel$path$add)) {
      dest<-pathmodel$path$add[[iadd]][2]
      iDest<-which(dest==gsub("=[^ ]*","",endo_names))
      source<-pathmodel$path$add[[iadd]][1]
      iSource<-which(source==gsub("=[^ ]*","",exo_names))
      if (isempty(iSource)) {
        iSource<-which(source==gsub("=[^ ]*","",endo_names))
        Bdesign[iDest,iSource]<-1
      } else {
        Ldesign[iDest,iSource]<-1
      }
    }
  }

  if (!isempty(only_dvs)) {
    use<-is.element(unlist(only_dvs),colnames(Bdesign))
    Bdesign[,unlist(only_dvs[use])]<-0
  }
  
  if (!is.null(pathmodel$path$remove) && !isempty(pathmodel$path$remove)) {
    for (iadd in 1:length(pathmodel$path$remove)) {
      dest<-pathmodel$path$remove[[iadd]][2]
      iDest<-which(dest==endo_names)
      source<-pathmodel$path$remove[[iadd]][1]
      iSource<-which(source==gsub("=[^ ]*","",exo_names))
      if (isempty(iSource)) {
        iSource<-which(source==gsub("=[^ ]*","",endo_names))
        Bdesign[iDest,iSource]<-0
      } else {
        Ldesign[iDest,iSource]<-0
      }
    }
  }
  
  
  sem<-list(
    stages=stages,
    depth=depth,
    add=pathmodel$path$add,
    
    P=P,
    Q=Q,
    Ldesign=Ldesign,
    Bdesign=Bdesign,
    Lresult=Lresult,
    Bresult=Bresult,
    
    endogenous=endogenous,
    exogenous=exogenous,
    endo_names=endo_names,
    exo_names=exo_names,
    
    data=data,
    varnames=varnames,
    
    full_varnames=model_data$varnames,
    full_varcases=model_data$varcases
  )
  
  return(sem)
}

sem_results<-function(pathmodel,sem) {
  output_field='sem_result';
  
  P=sem$P;
  Q=sem$Q;
  n_obs=sem$n_obs;
  
  switch(pathmodel$path$depth,
         'd1'= depth<-1,
         'd2'= depth<-2,
         'all'= depth<-length(stages)
  )
  
  nan_action<-"pairwise.complete.obs" # "complete.obs"
  sem$covariance<-cov(sem$data,use=nan_action)
  sem$cov_model=get_Stheta(sem);
  
  # full model stats
  
  # chi2 exact fit
  model_chisqr=(n_obs-1)*sem$Fmin;
  # rows are: Fixed labeled unlabeled
  numweights=rbind(P, 0, sum(!is.na(sem$Lresult)+sum(!is.na(sem$Bresult))))
  numcovs=rbind(0, 0, Q*(Q-1)/2)
  numvariances=rbind(0, 0, (P+Q))
  nummeans=rbind(P, 0, Q)
  numintercepts=rbind(0, 0, P)
  # %     num_est_pars=numweights + numcovs + numvariancess;
  # %     num_obs_pars=(P+Q)*(P+Q+1)/2;
  # %     model_chi_df=num_obs_pars - num_est_pars;
  df=cbind(numweights, numcovs, numvariances, nummeans, numintercepts)
  model_chi_df=sum(sum(df)-df[nrow(df),]);
  model_chi_p=1-pchisq(abs(model_chisqr), model_chi_df);
  # non-central chi2
  model_chi_noncentrality=max(model_chisqr-model_chi_df,0)/(n_obs-1);
  rmsea=sqrt(model_chi_noncentrality/model_chi_df);
  model_rmsea_p=1-pchisq(model_chisqr, model_chi_df, model_chi_noncentrality);
  # 
  ds=sem$cov_model-sem$covariance;
  # [a,b]=meshgrid(diag(sem$covariance));
  a<-matrix(diag(sem$covariance),nrow=nrow(sem$covariance),ncol=ncol(sem$covariance),byrow=TRUE)
  b<-t(a)
  dc=ds/sqrt(a*b);
  model_srmr=sqrt(sum(abs(dc^2)));
  #
  # 
  B=sem$Bresult; B[is.na(B)]=0;
  L=sem$Lresult; L[is.na(L)]=0;
  Y=t(sem$data[,1:P]); Y[is.na(Y)]=0;
  X=t(sem$data[,(P+1):ncol(sem$data)]); X[is.na(X)]=0;
  Z=B%*%Y+L%*%X;
  Yactual=Y-rowMeans(Y)
  Zactual=Z-rowMeans(Z)
  error=t(Yactual-Zactual)
  Rsquared=1-sum(diag(var(error)))/sum(diag(var(t(Y))))
  #
  k=(P+Q); n=n_obs;
  Resid2=sum(error^2);
  AIC=k+n*(log(2*pi*sum(error^2)/(n-k))+1);
  AICc=AIC+(2*k*k+2*k)/(n_obs-k-1);
  BIC=k*log(n)+AIC-2*k;
  CAIC=k*(log(n_obs)+1)+AIC-2*k;
  # 
  sem$stats<-list(model_chisqr=model_chisqr,
                 model_chi_df=model_chi_df,
                 model_chi_p=model_chi_p,
                 model_chi_noncentrality=model_chi_noncentrality,
                 rmsea=rmsea,
                 model_rmsea_p=model_rmsea_p,
                 model_srmr=model_srmr
  )
  sem$eval<-list(Rsquared=Rsquared,
                 k=k,
                 n=n,
                 AIC=AIC,
                 AICc=AICc,
                 BIC=BIC,
                 CAIC=CAIC
  )
  # 

  # model coefficients
  CF_table=cbind(sem$Bresult,sem$Lresult);
  # use_cf=!apply(is.na(CF_table),2,all)
  # CF_table=CF_table[,use_cf]
  rownames(CF_table)<-sem$endo_names
  colnames(CF_table)<-sem$varnames#[use_cf]
  addRows<-setdiff(sem$varnames,sem$endo_names)
  addMatrix<-matrix(NA,nrow=length(addRows),ncol=ncol(CF_table))
  rownames(addMatrix)<-addRows
  CF_table<-rbind(CF_table,addMatrix)
  CF_table<-CF_table[,order(colnames(CF_table))]
  CF_table<-CF_table[order(rownames(CF_table)),]
  
  sem$CF_table=CF_table;
  
  # effect sizes
  sem$covariance<-sem$covariance[,order(colnames(sem$covariance))]
  sem$covariance<-sem$covariance[order(rownames(sem$covariance)),]
  
  v<-matrix(diag(sem$covariance),nrow(sem$covariance),ncol(sem$covariance))
  sem$ES_table<-CF_table/sqrt(v/t(v))
  
  sem$Rtotal<-sem$covariance/sqrt(v*t(v))
  return(sem)
  
  # source_names=pathmodel$path$stages[[1]];
  # dest_names=pathmodel$path$stages[[length(pathmodel$path$stages)]];
  # for (is in 2:length(pathmodel$path$stages)-1){
  #   dest_names=c(dest_names, pathmodel$path$stages[[is]]);
  #   source_names=c(source_names, pathmodel$path$stages[[is]]);
  # }
  # dest_names=setdiff(dest_names,pathmodel$path$only_ivs);
  # dest_names<-dest_names[!sapply(dest_names,isempty)]
  # source_names<-source_names[!sapply(source_names,isempty)]
  # 
  # es=zeros(length(dest_names),length(source_names))+NA
  # for (is in 2:length(pathmodel$path$stages)) {
  #   if (!isempty(pathmodel$path$stages[[is]])) {
  #     for (iv in 1:length(pathmodel$path$stages[[is]])){
  #       if (!isempty(pathmodel$path$stages[[is]][iv])) {
  #         if (iscell(sem$mdl))
  #           local_mdl=sem$mdl[is,iv]
  #         else
  #           local_mdl=sem$mdl[is-1,iv];
  #       # if isempty(local_mdl) || isempty(local_mdl.ResponseName), continue; end
  #           
  #           use_stages=is-(1:depth); use_stages<-use_stages[use_stages>0]
  #           all_sources=pathmodel$path$stages[[use_stages]];
  # 
  #           es_local=glm2es(local_mdl);
  #           # [~,use_x]=ismember(all_sources,source_names);
  #           # [~,use_y]=ismember(pathmodel$path$stages[[is]][iv],dest_names);
  #           if (!all(use_y==0))
  #           es(use_y,use_x)=es_local.direct;
  #       }
  #     }
  #   }
  # }
  # 
  # sources=regexprep(source_names,{'[^\w]*'},{''});
  # [sources,s_use]=unique(sources);
  # dests=regexprep(dest_names,{'[^\w]*'},{''});
  # [dests,d_use]=unique(dests);
  # 
  # ES_table=array2table(es(d_use,s_use),'variablenames',sources,'rownames',dests);
  # 
  # sem$ES_table=ES_table;
  
  
  # pathmodel.path.esvals={};
  # pathmodel.path.pvals={};
  # for is=2:length(pathmodel$path$stages)
  # if isempty(pathmodel$path$stages{is}) || isempty(pathmodel$path$stages{is}{1}), continue; end
  # use2=~cellfun(@(x) isempty(x) || isempty(x.ResponseName),sem$mdl(is,:));
  # if any(use2)
  # use=find(use2,1,'first');
  # [~,sources_i]=ismember(sem$mdl{is,use}.PredictorNames,sources);
  # [~,dests_i]=ismember(regexprep(pathmodel$path$stages{is},{'[^\w]*'},{''}),dests);
  # for iv=1:length(dests_i)
  # if dests_i(iv)==0, continue; end
  # es=sem$ES_table{dests_i(iv),sources_i};
  # pathmodel.path.esvals{is,iv}=es;
  # pathmodel.path.pvals{is,iv}=convert_effectsize(es,n_obs,'r','p');
  # end
  # end
  # end
  # 
  
  return(sem)
}


# 
# 
# 
# 
# 

# %%%%%%%%%%%%%%%
# 
# %% sample covariances
# 
# safe_names=regexprep(sem$varnames,{'[^\w]*'},{''});
# allnames=safe_names(use_all); 
# [~,use]=unique(allnames);
# sem$S_table=array2table(sem$covariance(use,use),'variablenames',allnames(use),'rownames',allnames(use));
# 
# %% model covariances
# 
# sem$cov_model_table=array2table(sem$cov_model(use,use),'variablenames',allnames(use),'rownames',allnames(use));
# sem$E_table=array2table(sem$cov_model(use,use)-sem$covariance(use,use),'variablenames',allnames(use),'rownames',allnames(use));
# 
# 
# 
# %% equations
# 
# equations={}; 
# for iw=1:P
# use=find(strcmp(regexprep(sem$endo_names{iw},{'[^\w]*'},{''}),sem$varnames));
# coeff=nanmean(sem$data(:,use));
# %     cases=sem$varcases{use};
# name=sem$endo_names{iw}; % name=regexprep(name,'_([0-9]{1,100})','($1)');
# name=catname(name,hypothesis.model_data);
# equation=['\bf' name '\rm' ' ' options.assign_char]; 
# if coeff>0, coeff_sign='+'; else coeff_sign='-'; end
# equation=[equation ' ' coeff_sign num2str(abs(coeff),3)];
# 
# for ix=1:Q
# if ~isnan(sem$Lresult(iw,ix))
# coeff=sem$Lresult(iw,ix);
# if coeff>0, coeff_sign='+'; else coeff_sign='-'; end
# name=sem$exo_names{ix}; % name=regexprep(name,'_([0-9]{1,100})','($1)');
# name=catname(regexprep(name,{'[^\w]*'},{''}),hypothesis.model_data);
# equation=[equation ' ' coeff_sign ' ' num2str(abs(coeff),2) ' ' '\bf' name '\rm' ];
# end
# end
# for in=1:P
# if ~isnan(sem$Bresult(iw,in))
# coeff=sem$Bresult(iw,in);
# if coeff>0, coeff_sign='+'; else coeff_sign='-'; end
# name=sem$endo_names{in}; % name=regexprep(name,'_([0-9]{1,100})','($1)');
# name=catname(regexprep(name,{'[^\w]*'},{''}),hypothesis.model_data);
# equation=[equation ' ' coeff_sign ' ' num2str(abs(coeff),2) ' ' '\bf' name '\rm' ];
# end
# end
# equations{iw}=equation;
# end
# sem$equations=equations;
# 
# %%
#   
#   f=fieldnames(sem);
# for fi=1:length(f)
# pathmodel.(output_field).(f{fi})=sem$(f{fi});
# end
# 


