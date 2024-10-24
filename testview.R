viewmtx<-function(az,el,d=0) {
  
  az<-az*pi/180
  el<-el*pi/180
  T <- matrix(c( 
          cos(az),sin(az),0,0,
         -sin(el)*sin(az),sin(el)*cos(az),cos(el),0,
          cos(el)*sin(az),-cos(el)*cos(az),sin(el),0,
          0,0,0,1
         ),nrow=4,ncol=4,byrow=TRUE)
  if (d==0) return(T)
  target<-0.5 + sqrt(3)/2*matrix(c(cos(el)*sin(az),-cos(el)*cos(az),sin(el),1),ncol=1,nrow=4);
  mv<-matrix(c(1,0,0,
               0,1,0,
               0,0,1,
               0,0,0),nrow=4,ncol=3,byrow=TRUE)
  O1 <- cbind(mv,-T%*%target);
  
  P <- matrix(c(1,0,0,0,
               0,1,0,0,
               0,0,1,0,
               0,0,-1/d,1),nrow=4,ncol=4,byrow=TRUE)

  return(P%*%O1%*%T)
}