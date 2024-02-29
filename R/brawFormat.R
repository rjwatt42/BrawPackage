brawFormat<-function(numbers,digits=braw.env$report_precision) {
  if (is.null(numbers) || is.na(numbers)) return("NULL")
  
  pad<-function(x) if(x>=0) paste0(" ",x) else x
  
  if (all(numbers==round(numbers))) {
    r<-sprintf(numbers,fmt="%d")
  } else {
    r<-sprintf(numbers,fmt=paste0("%0.",digits,"f"))
  }
  r<-unname(sapply(r,pad))
  r
}
