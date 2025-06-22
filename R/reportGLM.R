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
#' reportGLM<-function(DV,IVs,result,p_or_r)
#' @export
reportGLM<-function(DV,IVs,result,p_or_r) {
  
  nc<-10
  nIVs<-length(result$r.direct)
  k<-nIVs+2 # no coefficients

  switch(p_or_r,
         "r"={title<-paste0("\beffect sizes"," (","DV = ",DV$name,")")},
         "p"={title<-paste0("\bp-values"," (","DV = ",DV$name,")")}
         )
  outputText<-c(paste0("\b!T",title),rep("",nc-1))

  outputText<-c(outputText,"!H!C","\bDirect","\bUnique","\bTotal",rep("",nc-4))
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
                  "!HModel","AIC","AICnull","R^2","r","llr","k","n","obs",
                  rep("",nc-9)
    )
    
    model<-makeModelFormula(list(stages=list(list(IVs$name),DV$name)))

    outputText<-c(outputText,
                  model,
                  brawFormat(result$aic,digits=1),
                  brawFormat(result$aicNull,digits=1),
                  brawFormat(result$r.full^2,digits=3),
                  brawFormat(result$r.full,digits=3),
                  brawFormat(result$llk,digits=3),
                  brawFormat(k),
                  brawFormat(result$nval),
                  brawFormat(1),
                  rep("",nc-9)
    )
  }

  
  reportPlot(outputText,nc,length(outputText)/nc)        
}
