
#' fit a SEM model to existing data
#' @return sample object 
#' @examples
#' fit_sem_model<-function(pathmodel,model_data,fixedCoeffs=NULL)
#' @export
fit_sem_model<-function(pathmodel,model_data,fixedCoeffs=NULL,doinglavaan=TRUE) {
# this follows the notation in 
  # Flora D.B. (2018) Statistical Methods for the Social and Behavioural Sciences
#  
#  Q,P = no of exogenous, endogenous variables
#  S = sample covariance matrix 
#  Stheta = predicted covariance matrix
#  the process is to optimize the match S & Stheta
  # the ML for a given estimate of Stheta
  # is given by log(abs(Stheta)) - trace(S%/%Stheta) - log(abs(S)) -(P+Q)
#
#  Stheta is obtained from a given set of possible model coefficients
  
  if (doinglavaan) {
    pathmodel$path$stages<-sapply(pathmodel$path$stages,function(x) gsub("[^0-9a-zA-Z_.]","",x))
    pathmodel$path$only_ivs<-sapply(pathmodel$path$only_ivs,function(x) gsub("[^0-9a-zA-Z_.]","",x))
    pathmodel$path$only_dvs<-sapply(pathmodel$path$only_dvs,function(x) gsub("[^0-9a-zA-Z_.]","",x))
    pathmodel$path$add<-sapply(pathmodel$path$add,function(x) gsub("[^0-9a-zA-Z_.]","",x))
    pathmodel$path$remove<-sapply(pathmodel$path$remove,function(x) gsub("[^0-9a-zA-Z_.]","",x))
    
    colnames(model_data$data)<-gsub("[^0-9a-zA-Z_.]","",colnames(model_data$data))
    model_data$variables$name<-gsub("[^0-9a-zA-Z_.]","",model_data$variables$name)
    model_data$varnames<-gsub("[^0-9a-zA-Z_.]","",model_data$varnames)
  }
  
  stages<-pathmodel$path$stages
  stages<-stages[!sapply(stages,isempty)]

  n_stages<-length(stages)
  if (n_stages==0) return(NULL)
  m_stages<-max(sapply(stages,length))

  pathLocalModel<-matrix(0,n_stages,m_stages)
  sem<-path2sem(pathmodel,model_data,doinglavaan)

  edges<-cbind(sem$Ldesign,sem$Bdesign)
  path<-""
  for (idv in 1:nrow(edges)) {
    if (any(edges[idv,]>0)) {
      ivs<-colnames(edges)[edges[idv,]>0]
      thiseq<-paste0(rownames(edges)[idv],"~",paste0(ivs,collapse="+"))
      path<-paste(path,thiseq,sep="\n")
    }
  }
  
  semResult <- lavaan::sem(path, data=sem$data)
  nullpath<-""
  for (i in 1:length(colnames(edges))) {
    nullpath<-paste0(nullpath,colnames(edges)[i],"~~",colnames(edges)[i],"\n")
  }
  semResultNull<-lavaan::sem(nullpath, data=sem$data)
  
  coefs<-lavInspect(semResult,"coef")$beta
  
  Rsquared<-mean(lavInspect(semResult,"rsquare"))

  fit<-lavaan::fitMeasures(semResult)
  fitNull<-lavaan::fitMeasures(semResultNull)
  
  sem$mdl<-semResult
  sem$Fmin<-fit["fmin"]
  sem$loglike<-fit["logl"]
  sem$coefficients<-coefs
  sem$n_obs<-lavInspect(semResult,"nobs")
  sem$covariance<-lavInspect(semResult,"cov.ov")
  sem$cov_model<-lavInspect(semResult,"cov.all")

  sem$CF_table=lavInspect(semResult,"coef")$beta
  sem$ES_table=lavInspect(semResult,"std")$beta
  sem$ES_table[sem$ES_table==0]<-NA
  
  sem$stats<-list(chisqr=fit["chisq"],
                  chi_df=fit["df"],
                  chi_p=fit["pvalue"],
                  rmsea=fit["rmsea"],
                  rmsea_p=fit["rmsea.pvalue"],
                  srmr=fit["srmr"]
  )
  sem$statsNull<-list(chisqr=fitNull["chisq"],
                  chi_df=fitNull["df"],
                  chi_p=fitNull["pvalue"],
                  rmsea=fitNull["rmsea"],
                  rmsea_p=fitNull["rmsea.pvalue"],
                  srmr=fitNull["srmr"]
  )
  sem$result<-list(fmin=fit["fmin"],
                   Rsquared=Rsquared,
                   r.full=sqrt(Rsquared),
                   k=fit["npar"],
                   kNull=fitNull["npar"],
                   n_data=lavInspect(semResult,"nobs"),
                   n_obs=lavInspect(semResult,"npar"),
                   llk=fit["logl"],
                   llkNull=fitNull["logl"],
                   # resid2=sum(error^2,na.rm=TRUE),
                   AIC=fit["aic"],
                   AICnull=fitNull["aic"],
                   BIC=fit["bic"],
                   BICnull=fitNull["bic"]
  )
  
  sem$Rtotal<-NA
  
  return(sem)
}

path2sem<-function(pathmodel,model_data,doinglavaan=TRUE) {

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
      # make dummy variables
          if (is.factor(full_data[,iv]))
            cases<-levels(full_data[,iv])
          else cases<-unique(full_data[,iv])
          cases<-cases[!is.na(cases)]
          nv<-zeros(nrow(full_data),length(cases)-1)
          nv[is.na(full_data[,iv]),]<-NA
          for (ic in 2:length(cases)) {
            nv[,ic-1]<-unlist(full_data[,iv])==cases[ic]
          }
          # nn<-paste0(full_varnames[iv],'=',cases[2:length(cases)])
          nn<-paste0(full_varnames[iv],cases[2:length(cases)])
          colnames(nv)<-nn
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

  if (length(stages)==1) {
    endo_names<-stages[[1]]
    exo_names<-c()
  } else {
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
  }
  exo_names<-unique(exo_names)
  endo_names<-unique(endo_names)
  varnames<-c(endo_names,exo_names)
  P<-length(endo_names)
  Q<-length(exo_names)
  endogenous<-1:P
  if (Q>0)  exogenous<-P+(1:Q)
  else exogenous<-c()

  use<-c()
  for (i in 1:length(varnames)) {
    use<-c(use,which(full_varnames==varnames[i]))
  }
  data<-full_data[,use]
  if (length(use)==1) data<-matrix(data,ncol=1)

  Bdesign<-zeros(P,P); rownames(Bdesign)<-endo_names; colnames(Bdesign)<-endo_names
  if (Q>0) {
    Ldesign<-zeros(P,Q); rownames(Ldesign)<-endo_names; colnames(Ldesign)<-exo_names
  } else Ldesign<-c()
    
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
  if (length(stages)>1)
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
        if (!isempty(iQ1))
        for (iso2 in (iso1+1):length(dests)){
          iQ2=which(dests[iso2]==endo_names);
          if (!isempty(iQ2)){
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
      # if (all(Bdesign[iDest,]==0) && all(Ldesign[iDest,]==0)) {
      #   keep<-setdiff(1:nrow(Bdesign),iDest)
      #   Bdesign<-Bdesign[keep,]
      #   Ldesign<-Ldesign[keep,]
      # }
    }
  }
  
  
  sem<-list(
    stages=stages,
    depth=depth,
    add=pathmodel$path$add,
    remove=pathmodel$path$remove,
    
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


