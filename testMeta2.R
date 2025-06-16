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

ma<-makeMetaAnalysis(TRUE,100,"world",modelNulls=TRUE,modelPDF="All")

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

ma<-makeMetaAnalysis(TRUE,10000,"world",modelNulls=TRUE,modelPDF="All")

d<-doMetaMultiple(10,NULL,ma)
reportMetaMultiple(d)
showMetaMultiple(d)

