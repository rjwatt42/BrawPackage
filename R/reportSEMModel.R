reportSEMModel<-function(sem,showType) {
  digits<-3
  
  switch(showType,
         "CF"={showData<-sem$CF_table;title="coefficients"},
         "ES"={showData<-sem$ES_table;title="effect sizes"},
         "cov"={showData<-sem$covariance;title="covariance"}
  )
  showData<-t(showData)
  
  useVars<-unlist(sem$stages)
  nc<-ncol(showData)+1
  if (nc<10) nc<-10
  
  outputText<-c(paste0("\b",title),rep(" ",nc-1))
  outputText<-c(outputText,rep(" ",nc))
  outputText<-c(outputText,"!H!C ",colnames(showData),rep(" ",nc-1-ncol(showData)))
  for (i in 1:nrow(showData)) {
    iu<-which(rownames(showData)==useVars[i])
    if (any(!is.na(showData[iu,]))) {
      outputText<-c(outputText,rownames(showData)[iu])
      for (j in 1:ncol(showData)) {
        ju<-which(rownames(showData)==useVars[j])
        if (is.na(showData[iu,ju])) outputText<-c(outputText," ")
        else outputText<-c(outputText,brawFormat(showData[iu,ju],digits=digits))
      }
      outputText<-c(outputText,rep(" ",nc-1-ncol(showData)))
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
  tableOutput<-rbind(list(AIC=sem$eval$AIC,
                          AICc=sem$eval$AICc,
                          BIC=sem$eval$BIC,
                          Rsqr=sem$eval$Rsquared,
                          r=sqrt(sem$eval$Rsquared),
                          k=sem$eval$k,
                          llr=sem$eval$llr,
                          model=st
  ),
  tableOutput
  )
  setBrawEnv("tableSEM",tableOutput)

  ne<-nrow(tableOutput)
  if (ne>15) {
    use1<-which.min(tableOutput[15:ne,1])
    use<-c(1:14,use1[1])
  } else {
    use<-1:ne
  }

  outputText<-c(outputText,"!H!C","AIC","AICc","BIC","Rsqr","r","k","llr","model",rep(" ",nc-1-8))
  digitsE<-c(1,1,1,3,3,0,1)
  prefix<-"!r"
  for (i in use) {
    outputText<-c(outputText," ")
    for (j in 1:8) {
      val<-unlist(tableOutput[i,j])
      if (is.numeric(val)) val<-brawFormat(val,digits=digitsE[j])
      if (j<=3 && i==which.min(tableOutput[,j])) val<-paste0(prefix,val)
      if (j==4 && i==which.max(tableOutput[,j])) val<-paste0(prefix,val)
      outputText<-c(outputText,val)
    }
    outputText<-c(outputText,rep(" ",nc-1-ncol(tableOutput)))
    # if (tableOutput[i,1]==which.min(tableOutput[,1])) vals[1]<-paste0(prefix,vals[1])
    # if (tableOutput[i,2]==which.min(tableOutput[,2])) vals[2]<-paste0(prefix,vals[2])
    # if (tableOutput[i,3]==which.min(tableOutput[,3])) vals[3]<-paste0(prefix,vals[3])
    # if (tableOutput[i,4]==which.max(tableOutput[,4])) vals[4]<-paste0(prefix,vals[4])
  }

  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)
  
}