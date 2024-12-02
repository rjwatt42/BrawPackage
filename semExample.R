
pathmodel<-list(path=
                  list(
                    stages=c(c("a"),c("b")),
                    # stages=c(c("a")),
                    depth="d1",
                    only_ivs=c(),
                    only_dvs=c(),
                    within_stage=0,
                    add=c(),
                    remove=c()
                  )
)

model_data<-list(pid=1:100,
                 data=matrix(rnorm(200),ncol=2),
                 varnames=c("a","b"),
                 varcat=c(FALSE,FALSE)
)

sem<-fit_sem_model(pathmodel,model_data)
plotSEMModel(sem)

