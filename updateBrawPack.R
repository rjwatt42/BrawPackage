
here<-pwd()

dir1<-"/Users/rogerwatt/Documents/GitHub/BrawPackage/R"
dir2<-"/Users/rogerwatt/Documents/GitHub/BrawPackDevelopment/R"
# files1<-list.files(dir1)
# files2<-list.files(dir2)
cd(dir1)
files1<-list.files(".")
f1info<-file.info(files1)

cd(dir2)
files2<-list.files(".")
f2info<-file.info(files2)

cd(here)

ignore<-c(
  "packages.R"
)

fint<-setdiff(union(files1,files2),ignore)
for (fi in 1:length(fint)) {
  use1<-which(files1==fint[fi])
  use2<-which(files2==fint[fi])
  if (isempty(use2)) {
    print(paste0("Copy ",fint[fi]," from BrawPack "))
  }
  if (isempty(use1)) {
    print(paste0("Copy ",fint[fi]," from Jamovi "))
  }
  if (!isempty(use1) && !isempty(use2)) {
    output<-""
    t1<-readLines(paste0(dir1,"/",fint[fi]),warn=FALSE)
    t2<-readLines(paste0(dir2,"/",fint[fi]),warn=FALSE)
    mismatch<-TRUE
    if (length(t1)==length(t2)) {
      if (all(t1==t2)) mismatch<-FALSE
      else output<-paste("lines ",paste(format(which(t1!=t2)),collapse=","))
    } else output<-paste("lengths ",paste(format(c(length(t1),length(t2))),collapse=","))
    if (mismatch) {
      fdate1<-f1info$mtime[use1]
      fdate2<-f2info$mtime[use2]
      if (fdate1>fdate2) {
        print(paste0("Update ",fint[fi]," from BrawPackage (",format(fdate1-fdate2,digits=1),") ",output))
      }
      if (fdate2>fdate1) {
        print(paste0("Update ",fint[fi]," from BrawPackDevelopment (",format(fdate2-fdate1,digits=1),") ",output))
      }
    }
  }
}

