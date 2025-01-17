

n=107

result<-c()
for (j in 1:1000) {
  r<-ceiling(runif(1000000)*n)
  a<-c()
  for (i in 1:n) {
    b<-which(r==i)
    if (length(b)>0) a<-c(a,min(b))
  }
result<-c(result,max(a))
}

