
stages1<-list(c("a"))
stages2<-list(c("a","b"))
stages3<-list(c("a"),c("b"))

pathmodel<-list(path=
                  list(
                    stages=stages1,
                    depth="d1",
                    only_ivs=c(),
                    only_dvs=c(),
                    within_stage=0,
                    add=c(),
                    remove=c()
                  )
)

rval=0.4
nsims=100
res<-matrix(NA,nsims,6)
for (i in 1:nsims) {
model_data<-list(pid=1:100,
                 data=rmnorm(100,c(0,0),matrix(c(1,rval,rval,1),2,2)),
                 varnames=c("a","b"),
                 varcat=c(FALSE,FALSE)
)

pathmodel$path$stages<-stages1
sem<-fit_sem_model(pathmodel,model_data)
res[i,1]<-sem$result$AIC
res[i,2]<-sem$result$AICnull

pathmodel$path$stages<-stages2
sem<-fit_sem_model(pathmodel,model_data)
res[i,3]<-sem$result$AIC
res[i,4]<-sem$result$AICnull

pathmodel$path$stages<-stages3
sem<-fit_sem_model(pathmodel,model_data)
res[i,5]<-sem$result$AIC
res[i,6]<-sem$result$AICnull
}

print(colMeans(res))
print(exp(-0.5*c(mean(res[,1])-mean(res[,2]),mean(res[,3])-mean(res[,4]),mean(res[,5])-mean(res[,6]))))
# print(c(sd(res[,1]),sd(res[,2]),sd(res[,3])))

# print(plotSEMModel(sem))

