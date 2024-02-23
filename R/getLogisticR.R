get_logistic_r<-function(effect1_size,n_cases,xv,pp=1,width=0,mn=NULL) {
  
  if (is.null(mn)) {
    dn<-2
    mn<-qnorm(seq(width/dn,1-width/dn,length.out=n_cases+1))
    mn<-mn[2:(length(mn)-1)]
    mn<-mn*(n_cases+abs(effect1_size))
  }

  r_effect1_size<-effect1_size
  d_effect1_size<-r_effect1_size/sqrt(1-r_effect1_size^2)/(n_cases-1)
  logodds_effect1_size<- -d_effect1_size*pi/sqrt(3)

  rr<-rep(logodds_effect1_size,n_cases-1)*2
  b <- rbind(mn,rr)

  n<-length(xv)
  p<-1
  pstar <- length(b[,1])
  k<-length(b[1,])+1
  
  
 eta<-c()
  for (i in 1:length(b[1,])) {
    v<-b[1,i]+xv*b[2,i]
   eta<-cbind(eta,v)  
  }

 gam<-1/(1+exp(-eta))
 result<-gam[,1]
 if (n_cases>2) {
   for (i in 2:(n_cases-1)) {
     result<-cbind(result,gam[,i]-gam[,i-1])
   }
 }
 result<-cbind(result,1-gam[,k-1])
 
 for (i in 1:n_cases) {
   result[,i]<-result[,i]/sum(result[,i])
 }
 
 for (i in 1:length(xv)) {
   result[i,]<-result[i,]/sum(result[i,])
 }
 result
 }
  
