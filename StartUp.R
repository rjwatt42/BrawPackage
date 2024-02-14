files<-setdiff(dir(".","*.R"),"StartUp.R")
for (fi in files) source(fi)

getGlobals()


