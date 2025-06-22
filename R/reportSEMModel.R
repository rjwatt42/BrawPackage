
#' @export
makeModelFormula<-function(sem) {
  # paste(DV$name,"=",paste(IVs$name,collapse="+")),
  stagesString<-""
  
  if (is.null(sem$stages)) 
    sem$stages<-list(sem$IVs,sem$DV)
  for (stage in sem$stages) {
    if (nchar(stagesString)>0) stagesString<-paste0(stagesString,"~")
    stagesString<-paste0(stagesString,paste0("{",paste(sapply(stage,truncateName,unlist(sem$stages)),collapse=","),"}"))
  }
  if (!is.null(sem$depth)) {
    stagesString<-paste0(stagesString,"@",sem$depth)
    
    addString<-""
    nAdd<-length(sem$add)
    if (nAdd>0) {
      addString<-" + "
      for (add in sem$add) {
        addString<-paste0(addString,"(",paste(sapply(add,substr,1,3),collapse=":"),")")
      }
    }
    
    removeString<-""
    nRemove<-length(sem$remove)
    if (nRemove>0){
      removeString<-" - "
      for (remove in sem$remove){
        removeString<-paste0(removeString,"(",paste(sapply(remove,substr,1,3),collapse=":"),")")
      }
    }
    stagesString<-paste0(stagesString,addString,removeString)
  }
  return(stagesString)
}

truncateName<-function(name,names) {
  n=3
  # while(1==2) {
  #   subnames<-sapply(names,substr,1,n)
  #   if (length(unique(subnames))==length(subnames)) break
  #   n=n+1
  # }
  substr(name,1,n)
}

#' report a fitted SEM model 
#' @return sample object 
#' @examples
#' reportSEMModel<-function(sem,showType)
#' @export
reportSEMModel<-function(sem,showType) {
  digits<-3
  
  switch(showType,
         "CF"={showData<-sem$CF_table;title="coefficients"},
         "ES"={showData<-sem$ES_table;title="effect sizes"},
         "cov"={showData<-sem$covariance;title="covariance"}
  )
  showData<-t(showData)
  # if (ncol(showData)>1){
  #   keep<-colSums(is.na(showData))<ncol(showData)
  #   showData<-showData[,keep]
  # }

  useVars<-unlist(sem$stages)
  nc<-ncol(showData)+1
  if (nc<11) nc<-11
  
  outputText<-c(paste0("!T",title),rep("",nc-1))
  outputText<-c(outputText,"!H!C ",colnames(showData),rep("",nc-1-ncol(showData)))
  for (i in 1:nrow(showData)) {
    if (any(!is.na(showData[i,]))) {
      outputText<-c(outputText,rownames(showData)[i])
      for (j in 1:ncol(showData)) {
        if (is.na(showData[i,j])) outputText<-c(outputText," ")
        else outputText<-c(outputText,brawFormat(showData[i,j],digits=digits))
      }
      outputText<-c(outputText,rep("",nc-1-ncol(showData)))
    }
  }
  
  outputText<-c(outputText,rep("",nc))

  tableOutput<-list(Model=makeModelFormula(sem),
               AIC=sem$result$aic,
               AICc=sem$result$aicc,
               BIC=sem$result$BIC,
               AICnull=sem$result$aicNull,
               Rsqr=sem$result$Rsquared,
               r=sqrt(sem$result$Rsquared),
               resid2=sem$result$resid2,
               llr=sem$result$llr,
               k=sem$result$k,
               n=sem$result$n_obs,
               obs=sem$result$n_data/sem$result$n_obs
  )

  columns<-c("Model","AIC","AICnull","Rsqr","r","llr","k","n","obs")
  nc1<-length(columns)
  tableText<-c("!TStatistics",rep("",nc-1),columns,rep("",nc-nc1))
  tableText[nc+1]<-paste0("!H",tableText[nc+1])
  tableText[which(tableText=="Rsqr")]<-"R^2"
  digitsE<-c(0,1,1,1,1,3,3,3,1,0,0,0)
  prefix<-"!r"
    for (column in columns) {
        j<-which(column==names(tableOutput))
        val<-unlist(tableOutput[j])
        if (is.numeric(val)) val<-brawFormat(val,digits=digitsE[j])
      tableText<-c(tableText,val)
    }
    tableText<-c(tableText,rep("",nc-nc1))
  outputText<-c(outputText,tableText)
  outputText<-c(outputText,rep("",nc))
  
  
  tableOutput<-braw.res$historySEM
  newRow<-list(model=makeModelFormula(sem),AIC=sem$result$aic,Rsqr=sem$result$r.full^2,r=sem$result$r.full)
  if (is.null(tableOutput)) tableOutput<-rbind(newRow)
  else                      tableOutput<-rbind(newRow,tableOutput)
  setBrawRes("historySEM",tableOutput)
  
  ne<-nrow(tableOutput)
  if (ne>15) {
    use1<-which.min(tableOutput[15:ne,1])
    use<-c(1:14,use1)
  } else {
    use<-1:ne
  }
  
  outputText<-c(outputText,"!THistory",rep("",nc-1))
  outputText<-c(outputText,"model","AIC","R^2","r",rep("",nc-4))
  
  for (i in 1:length(use)) {
    f2<-f3<-""
    if (use[i]==which.min(tableOutput[,2])) f2<-"\r"
    if (use[i]==which.max(tableOutput[,3])) f3<-"\r"
    row<-c(tableOutput[[use[i],1]],
           paste0(f2,brawFormat(tableOutput[[use[i],2]],1)),
           paste0(f3,brawFormat(tableOutput[[use[i],3]],3)),
           brawFormat(tableOutput[[use[i],4]],3)
           )
    outputText<-c(outputText,row,rep("",nc-4))
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)
  
}
