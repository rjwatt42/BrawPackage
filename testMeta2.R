##########################################

setDefaults()
setEffect(0.3)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,100,"fixed")

d<-doMetaAnalysis(NULL,ma)
reportMetaSingle(d)
showMetaSingle(d)

##########################################

setDefaults()
setEffect(rIV=0.3,rSD=0.1)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,100,"random")

d<-doMetaAnalysis(NULL,ma)
reportMetaSingle(d)
showMetaSingle(d)

##########################################

setDefaults()
setWorld(TRUE,"GenExp","z",0.3,populationPDFs=0.8)
# rp<-rRandomValue(braw.def$hypothesis$effect$world,100000000)
# hist(rp$use,seq(-1,1,0.0001))

setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,1000,"world",modelPDF="GenExp")
for (i in 1:10) {
d<-doMetaAnalysis(NULL,ma)
print(c(d$best$param1,d$best$param4))
}
# reportMetaSingle(d)
# showMetaSingle(d)

##########################################

setDefaults()
setWorld(TRUE,"Exp","z",0.3,populationNullp = 0.6)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,100,"world",modelNulls=TRUE,modelPDF="Exp")

d<-doMetaAnalysis(NULL,ma)
reportMetaSingle(d)
showMetaSingle(d)

##########################################

setDefaults()
setWorld(TRUE,"Exp","z",0.3,populationNullp = 0.6)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,1000,"world",modelNulls=TRUE,modelPDF="All")

d<-doMetaAnalysis(NULL,ma)
reportMetaSingle(d)
showMetaSingle(d)

##########################################

setDefaults()
setWorld(TRUE,"Exp","z",0.3)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,100,"world",modelPDF="All")

d<-doMetaAnalysis(NULL,ma)
reportMetaSingle(d)
showMetaSingle(d)

##########################################

setDefaults()
setEffect(0.3)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,100,"fixed")

d<-doMetaMultiple(100,NULL,ma)
reportMetaMultiple(d)
showMetaMultiple(d)

##########################################

setDefaults()
setEffect(0.3)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,100,"random")

d<-doMetaMultiple(100,NULL,ma)
reportMetaMultiple(d)
showMetaMultiple(d)

##########################################

setDefaults()
setWorld(TRUE,"Exp","z",0.3,populationNullp=0.3)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,1000,"world",modelNulls=TRUE,modelPDF="Exp")

d<-doMetaMultiple(10,NULL,ma)
reportMetaMultiple(d)
showMetaMultiple(d)

##########################################

setDefaults()
setWorld(TRUE,"Exp","z",0.3,populationNullp=0.3)
setDesign(getDesign("Psych"))

ma<-makeMetaAnalysis(TRUE,1000,"world",modelNulls=TRUE,modelPDF="All")

d<-doMetaMultiple(100,NULL,ma)
reportMetaMultiple(d)
showMetaMultiple(d)

