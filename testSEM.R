
data1<-readSample("testData.xlsx")
data1$data<-data1$data[rowSums(is.na(data1$data))==0,]

pathmodel<-list(path=list(
                          stages=list(c("PsychDistress"),c("Autism?")),
                          depth="d1",
                          only_ivs=list(),
                          only_dvs=list(),
                          within_stage=0,
                          add=list(),
                          remove=list()
            )
)
# pathmodel$path$add<-list(c("Age","PHQ-9 total"),c("Sex","PHQ-9 total"))
# pathmodel$path$add<-list(c("GAD-7 total","PHQ-9 total"))
# pathmodel$path$only_ivs<-list(c("CESD-total"))
# pathmodel$path$only_dvs<-list(c("CESD-total"))

# pathmodel$path$stages<-list("CESD-total","PHQ-9 total","Sex")
# pathmodel$path$depth<-"2"
# pathmodel$path$stages<-list("Sex","PHQ-9 total")

model_data<-list(pid=data1$data$participant,
                 data=data1$data[2:ncol(data1$data)],
                 varnames=data1$variables$name,
                 varcat=data1$variables$type=="Categorical"
                 )
sem<-path2sem(pathmodel,model_data)
sem<-fit_sem_model(pathmodel,model_data)

print(plotPathModel(sem))



