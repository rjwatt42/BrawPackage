makeText<-function(r,p,p_or_r) {
  if (p_or_r=="r") to_show<-r
  else to_show<-p
  if (p<braw.env$alphaSig) {
    if (r>0) {
      paste0("\b!r",brawFormat(to_show,digits=3))
    } else {
      paste0("\b!b",brawFormat(to_show,digits=3))
    }
  } else {
    brawFormat(to_show,digits=3)
  }
}


#' report a fitted GLM model 
#' @return sample object 
#' @examples
#' reportGLM<-function(lm,p_or_r)
#' @export
reportGLM<-function(lm,p_or_r="r") {
  DV<-lm$DV
  IVs<-lm$IVs
  result<-lm$result
  
  nc<-10
  nIVs<-length(result$r.direct)
  k<-nIVs+2 # no coefficients

  switch(p_or_r,
         "r"={title<-paste0("effect sizes"," (","DV = ",DV$name,")")},
         "p"={title<-paste0("p-values"," (","DV = ",DV$name,")")}
         )
  outputText<-c(paste0("!T",title),rep("",nc-1))

  outputText<-c(outputText,"!H!C ","Direct","Unique","Total",rep("",nc-4))
  for (i in 1:nIVs) {
    outputText<-c(outputText,
                  paste0(" ",IVs$name[i],"    "),
                  makeText(result$r.direct[i],result$p.direct[i],p_or_r),
                  makeText(result$r.unique[i],result$p.unique[i],p_or_r),
                  makeText(result$r.total[i],result$p.total[i],p_or_r),
                  rep("",nc-4)
    )
  }
  
  if (p_or_r=="r") {
    outputText<-c(outputText,rep("",nc))
    
    outputText<-c(outputText,
                  "!TStatistics",rep("",nc-1),
                  "!H!lModel","AIC","BIC","R^2","r","llr","k","n",
                  rep("",nc-8)
    )
    
    model<-makeModelFormula(list(stages=list(list(IVs$name),DV$name)))

    outputText<-c(outputText,
                  model,
                  brawFormat(result$AIC,digits=1),
                  brawFormat(result$BIC,digits=1),
                  brawFormat(result$r.full^2,digits=3),
                  brawFormat(result$r.full,digits=3),
                  brawFormat(result$llk,digits=3),
                  brawFormat(k),
                  brawFormat(result$nval),
                  rep("",nc-8)
    )
  }

  tableOutput<-braw.res$historyLM
  newRow<-list(model=makeModelFormula(list(DV=DV,IVs=IVs)),
               AIC=result$AIC,BIC=result$BIC,Rsqr=result$r.full^2,r=result$r.full,llk=result$llk)
  if (is.null(tableOutput)) tableOutput<-rbind(newRow)
  else           
    if (!identical(newRow,tableOutput[1,])) tableOutput<-rbind(newRow,tableOutput)
  setBrawRes("historyLM",tableOutput)
  
  ne<-nrow(tableOutput)
  if (ne>15) {
    use1<-which.min(tableOutput[15:ne,1])
    use<-c(1:14,use1)
  } else {
    use<-1:ne
  }
  
  outputText<-c(outputText,rep("",nc))
  outputText<-c(outputText,"!THistory",rep("",nc-1))
  outputText<-c(outputText,"!H!lModel","AIC","BIC","R^2","r","llk",rep("",nc-6))
  
  for (i in 1:length(use)) {
    f2<-f3<-""
    if (use[i]==which.min(tableOutput[,2])) f2<-"!r"
    if (use[i]==which.min(tableOutput[,3])) f2<-"!r"
    if (use[i]==which.max(tableOutput[,4])) f3<-"!r"
    row<-c(paste0("!l",tableOutput[[use[i],1]]),
           paste0(f2,brawFormat(tableOutput[[use[i],2]],1)),
           paste0(f3,brawFormat(tableOutput[[use[i],3]],3)),
           paste0(f3,brawFormat(tableOutput[[use[i],4]],3)),
           brawFormat(tableOutput[[use[i],5]],3),
           brawFormat(tableOutput[[use[i],6]],3)
    )
    outputText<-c(outputText,row,rep("",nc-6))
  }
  outputText<-c(outputText,rep("",nc))
  
  
  reportPlot(outputText,nc,length(outputText)/nc)        
}
