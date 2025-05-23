##################################################
#

R<-0.3

n<-42

np<-1000000
zs<-rnorm(np,atanh(R),1/sqrt(n-3))
rs<-tanh(zs)
ps<-rn2p(rs,n)
ws<-zn2w(zs,n,alpha=0.05)

nb<-51
quants<-c(0.1,0.9)

#######################
# z values

zbins<-seq(-1,1,length.out=nb)*1.2
zvals<-(zbins[1:(nb-1)]+zbins[2:nb])/2
zbinwidth<-zbins[2]-zbins[1]

hist(zs,breaks=zbins)
abline(v=quantile(zs,quants),col="#000000",lwd=2)
lines(zvals,dens_z(zvals,atanh(R),n)*np*zbinwidth,col="red",lwd=2)
abline(v=quant_z(quants,atanh(R),n),col="red",lwd=2,lty="dotted")

#######################
# r values

nb<-51
rbins<-seq(-1,1,length.out=nb)
rvals<-(rbins[1:(nb-1)]+rbins[2:nb])/2
rbinwidth<-rbins[2]-rbins[1]

hist(rs,breaks=rbins)
abline(v=quantile(rs,quants),col="#000000",lwd=2)
lines(rvals,dens_r(rvals,R,n)*np*rbinwidth,col="red",lwd=2)
abline(v=quant_r(quants,R,n),col="red",lwd=2,lty="dotted")


#######################
# p values

pbins<-seq(0,1,length.out=nb)
pvals<-(pbins[1:(nb-1)]+pbins[2:nb])/2
pbinwidth<-pbins[2]-pbins[1]

hist(ps,breaks=pbins)
abline(v=quantile(ps,quants),col="#000000",lwd=2)
lines(pvals,dens_p(pvals,R,n)*np*pbinwidth,col="red",lwd=2)
abline(v=quant_p(quants,R,n),col="red",lwd=2,lty="dotted")


#######################
# log(p) values

plogbins<-seq(-6,0,length.out=nb)
plogvals<-(plogbins[1:(nb-1)]+plogbins[2:nb])/2
plogbinwidth<-plogbins[2]-plogbins[1]

hist(log10(ps[log10(ps)>=-6]),breaks=plogbins)
abline(v=log10(quantile(ps,quants)),col="#000000",lwd=2)
lines(plogvals,dens_logp(10^plogvals,R,n)*np*plogbinwidth,col="red",lwd=2)
abline(v=log10(quant_p(quants,R,n)),col="red",lwd=2,lty="dotted")

#######################
# w values

wbins<-seq(0.05,1,length.out=nb)
wvals<-seq(0.051,0.999,length.out=nb*2+1)
wbinwidth<-wbins[2]-wbins[1]

hist(ws,breaks=wbins)
abline(v=quantile(ws,quants),col="#000000",lwd=2)
lines(wvals,dens_w(wvals,R,n)*np*wbinwidth,col="red",lwd=2)
abline(v=quant_w(quants,R,n),col="red",lwd=2,lty="dotted")


#############################

R<-c(0.1,0.5)
Rgain<-c(1,2.5)
n<-42

target<-0.1
rvals<-seq(-1,1,length.out=101)*0.999
rdens<-0
for (i in 1:length(R)) {
rdens<-rdens+dens_r(rvals,R[i],n)*Rgain[i]
}
rdens<-rdens/sum(Rgain)

e1<-approx(cumsum(rdens)/sum(rdens),rvals,target)$y+diff(rvals[1:2])/2

zvals<-seq(-1,1,length.out=101)*5
zdens<-0
for (i in 1:length(R)) {
  zdens<-zdens+dens_z(zvals,atanh(R[i]),n)*Rgain[i]
}
zdens<-zdens/sum(Rgain)

e2<-z2r(approx(cumsum(zdens)/sum(zdens),zvals,target)$y+diff(zvals[1:2])/2)
print(c(e1,e2))

#############################
# pluttering

dens_pg<-function(p,R,n) {
  Z<-atanh(R)*sqrt(n-3)
  z<-qnorm(p/2)
  dpdz<-exp(-0.5*(z)^2)
  dens<-(exp(-0.5*(z+Z)^2)+exp(-0.5*(z-Z)^2))/2/dpdz
  min_p<-min(p[p>0])
  psum<-pnorm(qnorm(min_p/2),Z)+pnorm(qnorm(min_p/2),-Z)
  gain<-(1-psum)/sum(dens*c(0,diff(p)))
}

R<-seq(0,0.8,0.1)
n<-42
res<-c()
for (Ri in R){
  res<-c(res,dens_pg(pvals,Ri,n))
}

plot(atanh(R),res,"l")

#############################
# pluttering

dens_pg<-function(p,R,n) {
  Z<-atanh(R)*sqrt(n-3)
  z<-qnorm(p/2)
  dpdz<-exp(-0.5*(z)^2)
  dens<-(exp(-0.5*(z+Z)^2)+exp(-0.5*(z-Z)^2))/2/dpdz
  min_p<-min(p[p>0])
  psum<-pnorm(qnorm(min_p/2),Z)+pnorm(qnorm(min_p/2),-Z)
  gain<-(1-psum)/sum(dens*diff(p[1:2]))
}

R<-0.2
n<-seq(20,1000,20)
res<-c()
for (ni in n){
  res<-c(res,dens_pg(pvals,R,ni))
}

plot(n-3,res,"l")

