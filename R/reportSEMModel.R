
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
reportSEMModel<-function(sem,showType="CF",evalType="AIC",showFit=FALSE) {
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
               AIC=sem$result$AIC,
               AICc=sem$result$AICc,
               BIC=sem$result$BIC,
               AICnull=sem$result$AICnull,
               BICnull=sem$result$BICnull,
               Rsqr=sem$result$Rsquared,
               r=sqrt(sem$result$Rsquared),
               resid2=sem$result$resid2,
               llr=sem$result$llr,
               k=sem$result$k,
               n=sem$result$n_obs,
               obs=sem$result$n_data/sem$result$n_obs
  )
  digitsE<-c(0,1,1,1,1,1,3,3,3,1,0,0,0)
  
  columns<-c("Model",evalType,paste0(evalType,"null"),"Rsqr","r","llr","k","n","obs")
  nc1<-length(columns)
  tableText<-c("!TStatistics",rep("",nc-1),columns,rep("",nc-nc1))
  tableText[nc+1]<-paste0("!H!l",tableText[nc+1])
  tableText[which(tableText=="Rsqr")]<-"R^2"
  prefix<-"!r"
    for (column in columns) {
        j<-which(column==names(tableOutput))
        val<-unlist(tableOutput[j])
        if (is.numeric(val)) val<-brawFormat(val,digits=digitsE[j])
        if (column=="Model") val<-paste0("!l",val)
      tableText<-c(tableText,val)
    }
    tableText<-c(tableText,rep("",nc-nc1))
  outputText<-c(outputText,tableText)
  outputText<-c(outputText,rep("",nc))
  
  if (showFit) {
    outputText<-c(outputText,"!TFit",rep("",nc-1))
    outputText<-c(outputText,"!HStatistic","value","df","p",rep("",nc-4))
    outputText<-c(outputText,"Chi^2",
                  brawFormat(sem$stats$chisqr,3),
                  brawFormat(sem$stats$chi_df,0),
                  brawFormat(sem$stats$chi_p,3),
                  rep("",nc-4))
    outputText<-c(outputText,"RMSEA",
                  brawFormat(sem$stats$rmsea,3),
                  " ",
                  brawFormat(sem$stats$rmsea_p,3),
                  rep("",nc-4))
    outputText<-c(outputText,"SRMR",
                  brawFormat(sem$stats$srmr,3),
                  " ",
                  " ",
                  rep("",nc-4))
    
    outputText<-c(outputText,rep("",nc))
  }
  
  tableOutput<-braw.res$historySEM
  newRow<-list(model=makeModelFormula(sem),AIC=sem$result$AIC,BIC=sem$result$BIC,Rsqr=sem$result$r.full^2,r=sem$result$r.full)
  if (is.null(tableOutput)) tableOutput<-rbind(newRow)
  else           
    if (!identical(newRow,tableOutput[1,])) tableOutput<-rbind(newRow,tableOutput)
  setBrawRes("historySEM",tableOutput)
  
  ne<-nrow(tableOutput)
  if (ne>15) {
    use1<-which.min(tableOutput[15:ne,1])
    use<-c(1:14,use1)
  } else {
    use<-1:ne
  }
  
  outputText<-c(outputText,"!THistory",rep("",nc-1))
  outputText<-c(outputText,"!H!lModel",evalType,"R^2","r",rep("",nc-4))
  
  if (evalType=="BIC") io<-3 else io<-2
  for (i in 1:length(use)) {
    f2<-f3<-""
    if (use[i]==which.min(tableOutput[,io])) f2<-"!r"
    if (use[i]==which.max(tableOutput[,4])) f3<-"!r"
    row<-c(paste0("!l",tableOutput[[use[i],1]]),
           paste0(f2,brawFormat(tableOutput[[use[i],io]],1)),
           paste0(f3,brawFormat(tableOutput[[use[i],4]],3)),
           brawFormat(tableOutput[[use[i],5]],3)
           )
    outputText<-c(outputText,row,rep("",nc-4))
  }
  outputText<-c(outputText,rep("",nc))
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)
  
}
