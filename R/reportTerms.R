
reportTerms<-function() {
  
  terms<-c("\bStandard", "",
           "rs","sample effect size",
           "rp","population effect size",
           "re","sample error",
           "n","sample size",
           "p","p-value",
           " "," ",
           "\bPower"," ",
           "ws","sample power (using rs: bad)",
           "wp","population power (using rp: good)",
           "nw","sample size from ws",
           " "," ",
           "\bReplication"," ",
           "ro","original sample effect size",
           "po","original p-value",
           "no","original sample size"
  )
  
  nc=2
  nr=length(terms)/nc
  reportPlot(terms,nc,nr)        
  
}
