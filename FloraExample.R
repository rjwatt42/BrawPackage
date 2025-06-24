

sem1<-makeSEM("lennox.dat",makeSEMPath(stages=list(c("size","density","homophily"),c("pass"),c("cesd"))))
semResult<-fit_sem_model(sem1$path,sem1$data)
print(plotSEMModel(semResult))

g1<-reportSEMModel(semResult,"CF","AIC",TRUE)
print(g1)

############################

ch9data <- read.table("lennox.dat", header=T)

ch9mod="cesd ~ pass + density + homophily
# pass ~ density
density ~ homophily"

semResult <- lavaan::sem(ch9mod, data=ch9data)
lavaan::summary(semResult, fit.measures=T)
lavaan::fitMeasures(semResult,"srmr")
semResult@ParTable


# semPaths(ch9modML,layout="circle",#what="est",
#          rotation=3,residuals=FALSE,
#          nCharNodes=0,sizeMan=12,sizeMan2=6,whatLabels="stand",edge.label.cex = 1.2)

semPaths(semResult,layout=cbind(x=c(3,1,2,0),y=c(0,0,1,1)),#what="est",
         rotation=3,residuals=FALSE,
         nCharNodes=0,sizeMan=12,sizeMan2=6,whatLabels="stand",edge.label.cex = 1.2)


############################


add<-list()
thisAdd<-list(c("homophily","cesd"))
add<-c(add,thisAdd)
thisAdd<-list(c("density","cesd"))
add<-c(add,thisAdd)
remove<-list()
remove<-c(remove,list(c("density","pass")))
sem1<-makeSEM("lennox.dat",makeSEMPath(stages=list(c("homophily"),c("density"),c("pass"),c("cesd")),
                                       add=add,remove=remove))
semResult1<-fit_sem_model(sem1$path,sem1$data)
g1<-plotSEMModel(semResult1)
print(g1)
semResult1$stats
