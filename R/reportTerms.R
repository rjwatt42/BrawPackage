
reportTerms<-function() {
  
  terms<-c("\bStandard", "","\bSymbol "," ",
           "rs","sample effect size","!j!>>>\u25CF","data point",
           "rp","population effect size"," "," ",
           "re","sample error","!j!>>>\u25A0","sample effect",
           "n","sample size"," "," ",
           "p","p-value","!j!>\u25B2","metaAnalysis",
           " "," "," "," ",
           "\bPower"," "," "," ",
           "ws","sample power (using rs: bad)"," "," ",
           "wp","population power (using rp: good)"," "," ",
           "nw","sample size from ws"," "," ",
           " "," "," "," ",
           "\bReplication"," "," "," ",
           "ro","original sample effect size"," "," ",
           "po","original p-value"," "," ",
           "no","original sample size"," "," "
  )
  
  nc=4
  nr=length(terms)/nc
  reportPlot(terms,nc,nr)        
  
}
