sourceGit<-function(owner="rjwatt42",repo="BrawPackage",file="example0.R") {
  
  source(curl::curl(paste0("https://raw.github.com/",owner,"/",repo,"/master/",file)))
  
}