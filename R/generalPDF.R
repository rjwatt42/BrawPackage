##################################################

z2r<-function(z) tanh(z)
r2z<-function(r) atanh(r)

zn2p<-function(z,n)  2*pnorm(-abs(z)*sqrt(n-3))
pn2z<-function(p,n)  -qnorm(p/2)/sqrt(n-3)

zn2w<-function(z,n,alpha=0.05)  pnorm(qnorm(alpha/2),z*sqrt(n-3))+pnorm(qnorm(alpha/2),-z*sqrt(n-3))

quant_z <- function(quant,Z,n)   qnorm(quant,Z,1/sqrt(n-3))
quant_r <- function(quant,R,n)   z2r(quant_z(quant,atanh(R),n))
quant_p <- function(quant,R,n) {
  # find the value of z that satisfies pq=quant
  # ie. pnorm(z,Z,1/sqrt(n-3))-pnorm(-z,Z,1/sqrt(n-3))=quant
  Z<-atanh(R)*sqrt(n-3)
  z<-seq(0,4,0.1)
  pq<-pnorm(z-Z)-pnorm(-z-Z)
  2*pnorm(-approx(pq,z,quant)$y)
}
quant_w <- function(quant,R,n,alpha=0.05) {
  # find the value of z that satisfies w=quant
  # ie. pnorm(z,Z,1/sqrt(n-3))-pnorm(-z,Z,1/sqrt(n-3))=quant
  Z<-atanh(R)*sqrt(n-3)
  z<-seq(0,4,0.1)
  pq<-pnorm(z-Z)-pnorm(-z-Z)
  z<-approx(pq,z,quant)$y
  pnorm(qnorm(alpha/2),z)+pnorm(qnorm(alpha/2),-z)
}

dens_z <- function(z,Z,n) {
  sqrt(n-3)/sqrt(2*pi)*exp(-0.5*(n-3)*(z-Z)^2)
}

dens_r <- function(r,R,n) {
  Z<-atanh(R)*sqrt(n-3)
  z<-atanh(r)*sqrt(n-3)
  drdz<-1-r^2
  sqrt(n-3)/sqrt(2*pi)*exp(-0.5*(z-Z)^2)/drdz
}

dens_p_long<-function(p,r_p,n) {
  z<- -qnorm(p/2)/sqrt(n-3)
  dpdz <- 2/sqrt(2*pi)*sqrt(n-3)*exp(-0.5*(z*sqrt(n-3))^2)
  dpdz[dpdz==0]<-1
  dens_z(z,atanh(r_p),n)/dpdz
}

dens_p<-function(p,R,n) {
  Z<-atanh(R)*sqrt(n-3)
  z<-qnorm(p/2)
  dpdz<-exp(-0.5*(z)^2)
  dens<-(exp(-0.5*(z+Z)^2)+exp(-0.5*(z-Z)^2))/2/dpdz
  min_p<-min(p[p>0])
  psum<-pnorm(qnorm(min_p/2),Z)+pnorm(qnorm(min_p/2),-Z)
  gain<-(1-psum)/sum(dens*c(0,diff(p)))
  dens*gain
}

dens_logp<-function(p,R,n) {
  Z<-atanh(R)*sqrt(n-3)
  z<-qnorm(p/2)
  dpdz<-exp(-0.5*(z)^2)
  dldp<-1/p
  dens<-(exp(-0.5*(z+Z)^2)+exp(-0.5*(z-Z)^2))/2/dpdz/dldp
  min_p<-min(p)
  psum<-pnorm(qnorm(min_p/2),Z)+pnorm(qnorm(min_p/2),-Z)
  gain<-(1-psum)/sum(dens*c(0,diff(log10(p))))
  dens*gain
}

dens_w<-function(w,R,n,alpha=0.05) {
  # we need to do it this way, alas
  z<-seq(0,10,0.05)
  wz<-pnorm(qnorm(alpha/2),z)+pnorm(qnorm(alpha/2),-z)
  Z <- atanh(R)*sqrt(n-3)
  dwdz<-sqrt(n-3)/(2*pi)*(exp(-0.5*(z+qnorm(alpha/2))^2)-exp(-0.5*(z-qnorm(alpha/2))^2))
  densW<-(exp(-0.5*(z-Z)^2)+exp(-0.5*(z+Z)^2))/dwdz
  approx(wz,densW,w)$y
}

sig_r<-function(R,n,alpha=0.05) {
  Z<-atanh(R)*sqrt(n-3)
  pnorm(qnorm(alpha/2),Z)+pnorm(qnorm(alpha/2),-Z)
}

worldSampling<-function(world,design,type="z") {
  
}
