reportPathModel<-function(sem,showType) {
  digits<-2
  
  switch(showType,
         "CF"={showData<-sem$CF_table;title="coefficients"},
         "ES"={showData<-sem$ES_table;title="effect sizes"},
         "cov"={showData<-sem$covariance;title="covariance"}
  )
  
  nc<-ncol(showData)+1
  outputText<-c(paste0("\b",title),rep(" ",nc-1))
  outputText<-c(outputText,rep(" ",nc))
  outputText<-c(outputText,"!H!C ",colnames(showData))
  
  for (i in 1:nrow(showData)) {
    outputText<-c(outputText,rownames(showData)[i])
    for (j in 1:ncol(showData)) {
      if (is.na(showData[i,j])) outputText<-c(outputText," ")
      else outputText<-c(outputText,brawFormat(showData[i,j],digits=digits))
    }
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)
  
}