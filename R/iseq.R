
is.equalLists<-function(l1,l2) {
  
  n1<-names(l1)
  n2<-names(l2)
  if (length(n1)!=length(n2)) return(FALSE)
  if (!isempty(setdiff(n1,n2))) return(FALSE)
  for (n in n1) {
    a<-l1[[n]]
    b<-l2[[n]]
    if (is.null(a) && is.null(b)) checked<-TRUE
    else {
      if (is.list(a)) checked<-is.equalLists(a,b)
      else {
        if ((sum(is.na(a))==sum(is.na(b))) &&
          all(is.na(a)==is.na(b)) && all(a[!is.na(a)]==b[!is.na(b)])) checked<-TRUE
        else checked<-FALSE
      }
    }
    if (!checked) return(FALSE)
  }
  return(TRUE)
}

