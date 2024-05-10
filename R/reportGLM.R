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

reportGLM<-function(DV,IVs,result,p_or_r) {
  
  nc<-4

  outputText<-c(
    paste0("\b",p_or_r," values"," (","DV = ",DV$name,")"),"","","",
    " ","","","",
    "\b!jEffect    ","\bDirect","\bUnique","\bTotal"
    )
  for (i in 1:length(result$r.direct)) {
    outputText<-c(outputText,
                  paste0("\b!j",IVs$name[i],"    "),
                  makeText(result$r.direct[i],result$p.direct[i],p_or_r),
                  makeText(result$r.unique[i],result$p.unique[i],p_or_r),
                  makeText(result$r.total[i],result$p.total[i],p_or_r)
    )
  }
  
  if (p_or_r=="r") {
    outputText<-c(outputText,
                  "\b!jFull model    ",paste0("\b",brawFormat(result$r.full,digits=3))," ","",
                  "!j\bAIC:",paste0("\b",brawFormat(AIC(result$lmNormC),digits=3)),"",""
    )
  }
  reportPlot(outputText,nc,length(outputText)/nc)        
}
