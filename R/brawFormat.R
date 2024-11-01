brawFormat<-function(numbers,digits=braw.env$report_precision) {

  if (any(is.infinite(numbers))) return(format(numbers))
  if (is.null(numbers) || any(is.na(numbers))) return("NULL")
  
  pad<-function(x) if(x>=0) paste0(" ",x) else x
  trim<-function(x) sub(".0+$", "", x) 
  
  if (digits<0) {
    digits<-(-digits)-max(floor(log10(abs(numbers))))
    if (digits<0) digits<-0
  }
  if (all(abs(numbers-round(numbers))<10^(-digits))) {
    r<-sprintf(round(numbers),fmt="%d")
  } else {
    r<-sprintf(numbers,fmt=paste0("%0.",digits,"f"))
  }
  change<-numbers<0
  if (any(change)) r[change]<-unname(sapply(r[change],pad))
  
  # change<- r!="0" && numbers!=round(numbers,digits)
  change<-grepl("\\.0+$",r)
  if (any(change)) r[change]<-unname(sapply(r[change],trim))
  r
}
