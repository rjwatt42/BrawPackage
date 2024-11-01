
data1<-readSample("class data 2018.xlsx")
data1$data<-data1$data[rowSums(is.na(data1$data))==0,]

pathmodel<-list(path=list(
                          stages=list(c("Age","Sex"),c("HAMD total","CESD-total"),c("PHQ-9 total")),
                          depth="1",
                          only_ivs=c(),
                          only_dvs=c(),
                          within_stage=0,
                          add=c()
            )
)
pathmodel$path$add<-list(c("Age","PHQ-9 total"),c("Sex","PHQ-9 total"))
pathmodel$path$remove<-list(c("Age","CESD-total"),c("Sex","HAMD total"))
# pathmodel$path$stages<-list("CESD-total","PHQ-9 total","Sex")
# pathmodel$path$depth<-"2"
# pathmodel$path$stages<-list("Sex","PHQ-9 total")

model_data<-list(pid=data1$data$participant,
                 data=data1$data[2:ncol(data1$data)],
                 varnames=data1$variables$name,
                 varcat=data1$variables$type=="Categorical"
                 )

sem<-fit_sem_model(pathmodel,model_data)

plotPathModel(sem)



