brawFormat<-function(numbers,digits=braw.env$report_precision) {

  if (is.null(numbers) || is.na(numbers)) return("NULL")
  
  pad<-function(x) if(x>=0) paste0(" ",x) else x
  
  trim<-function(x) sub("0+$", "", x) 
  
  if (all(abs(numbers-round(numbers))<10^(-digits))) {
    r<-sprintf(round(numbers),fmt="%d")
  } else {
    r<-sprintf(numbers,fmt=paste0("%0.",digits,"f"))
  }
  change<-numbers<0
  if (any(change)) r[change]<-unname(sapply(r[change],pad))
  change<-numbers!=0 && numbers!=round(numbers)
  if (any(change)) r[change]<-unname(sapply(r[change],trim))
  r
}
