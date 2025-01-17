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
  
  outputText<-c(paste0("\b",title),rep("",nc-1))
  outputText<-c(outputText,rep("",nc))
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

  stagesString<-""
  for (stage in sem$stages) {
      stagesString<-paste0(stagesString,paste0("{",paste(sapply(stage,substr,1,3),collapse=","),"}"))
  }
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
  st<-paste0(stagesString,addString,removeString)
  
  
  tableOutput<-braw.env$tableSEM
  newRow<-list(Model=st,
               AIC=sem$eval$AIC,
               AICc=sem$eval$AICc,
               BIC=sem$eval$BIC,
               AICnull=sem$eval$AICnull,
               Rsqr=sem$eval$Rsquared,
               r=sqrt(sem$eval$Rsquared),
               resid2=sem$eval$resid2,
               llr=sem$eval$llr,
               k=sem$eval$k,
               n=sem$eval$n_obs,
               obs=sem$eval$n_data/sem$eval$n_obs
  )
  if (is.null(tableOutput) || (!identical(newRow,tableOutput[1,]))) {
    tableOutput<-rbind(newRow,tableOutput)
    setBrawEnv("tableSEM",tableOutput)
  }

  ne<-nrow(tableOutput)
  if (ne>15) {
    use1<-which.min(tableOutput[15:ne,1])
    use<-c(1:14,use1[1])
  } else {
    use<-1:ne
  }

  columns<-c("Model","AIC","AICnull","llr[+]","Rsqr","r","llr","k","n","obs")
  nc1<-length(columns)
  tableText<-c(columns,rep("",nc-nc1))
  tableText[1]<-paste0("!H",tableText[1])
  tableText[which(columns=="Rsqr")]<-"R^2"
  digitsE<-c(0,1,1,1,1,3,3,3,1,0,0,0)
  prefix<-"!r"
  for (i in use) {
    for (column in columns) {
      if (column=="llr[+]") {
        val<-log(exp(-0.5*(unlist(tableOutput[i,2])-unlist(tableOutput[i,5]))))
        val<-brawFormat(val,digits=3)
      } else {
        j<-which(column==colnames(tableOutput))
        val<-unlist(tableOutput[i,j])
        if (is.numeric(val)) val<-brawFormat(val,digits=digitsE[j])
        # if (is.element(column,c("AIC","AICc","BIC")) && i==which.min(tableOutput[,j])) val<-paste0(prefix,val)
        # if (is.element(column,c("Rsqr")) && i==which.max(tableOutput[,j])) val<-paste0(prefix,val)
      }
      tableText<-c(tableText,val)
    }
    tableText<-c(tableText,rep("",nc-nc1))
  }
  outputText<-c(outputText,tableText)
  outputText<-c(outputText,rep("",nc))
  
  if (nrow(tableOutput)>1) {
  p1<-exp(-0.5*(unlist(tableOutput[1,2])-unlist(tableOutput[2,2])))
  outputText<-c(outputText,paste0('!lcurrent model is ',brawFormat(p1,2),' times as probable as previous'),rep("",nc-1))
  p2<-exp(-0.5*(unlist(tableOutput[1,2])-min(unlist(tableOutput[,2]))))
  if (p2==1) outputText<-c(outputText,paste0('!lcurrent model is the best'),rep("",nc-1))
    else     outputText<-c(outputText,paste0('!lcurrent model is ',brawFormat(p2,2),' times as probable as best'),rep("",nc-1))
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)
  
}
