

sem1<-makeSEM("lennox.dat",makeSEMPath(stages=list(c("size","density","homophily"),c("pass"),c("cesd"))))
semResult<-fit_sem_model(sem1$path,sem1$data)
print(plotSEMModel(semResult))

g1<-reportSEMModel(semResult,"CF","AIC",TRUE)
print(g1)

############################

ch9data <- read.table("lennox.dat", header=T)

ch9mod <- 'cesd ~ pass
           pass ~ size + density + homophily
          '

ch9modML <- sem(ch9mod, data=ch9data)
summary(ch9modML, fit.measures=T)

############################
