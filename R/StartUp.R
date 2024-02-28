# files<-setdiff(dir("R","*.R"),"StartUp.R")
# fAvoid<-dir(".","example[0-9]*.R")
# files<-setdiff(files,fAvoid)
# # 
# for (fi in files) source(paste0("R/",fi))
# StartUp<-function() getGlobals()
# 
# 
