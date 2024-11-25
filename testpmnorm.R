n<-1000000
rho<-0.3
proportions<-c(1,3)
proportions<-proportions/sum(proportions)

x<-rnorm(n)
y<-x*rho+sqrt(1-rho^2)*rnorm(n)

xbreak<-qnorm(proportions[1])
print(mean((x<xbreak & y<0)))

sigma<-matrix(c(1,rho,rho,1),nrow=2)
mu<-c(0,0)
print(pmnorm(c(xbreak,0),mu,sigma))

