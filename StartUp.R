files<-setdiff(dir(".","*.R"),"StartUp.R")
fAvoid<-dir(".","example[0-9]*.R")
files<-setdiff(files,fAvoid)

for (fi in files) source(fi)

getGlobals()


