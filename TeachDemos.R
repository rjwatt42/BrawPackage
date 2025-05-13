
h1<-makeHypothesis(effect=makeEffect(0.3))
d1<-makeDesign(sN=42)

print(showMultiple(doMultiple(100,hypothesis = h1, design = d1),showType="Basic"))

##########################

h2<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Single","r",0.3,0,0.5)))
d2<-makeDesign(sN=42)
d2$sN<-d2$sN*5

print(showMultiple(doMultiple(200,NULL,hypothesis = h2, design = d2),showType="NHST"))
print(showMultiple(showType="Inference"))
# print(showMultiple(doMultiple(100,NULL,hypothesis = h2, design = d2),showType="Basic"))

##########################

h3<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Exp","z",0.3,0,0.75)))
d3<-makeDesign(sN=42)
# d3$sN<-d3$sN*15

print(showMultiple(doMultiple(200,NULL,hypothesis = h3, design = d3),showType="NHST"))
print(showMultiple(showType="Inference"))

##########################

h4<-h3
d4a<-makeDesign(sN=210)
d4b<-makeDesign(sN=42,sCheating="Retry",sCheatingAttempts=10)

print(showMultiple(doMultiple(200,NULL,hypothesis = h4, design = d4a),showType="NHST"))
print(showMultiple(doMultiple(200,NULL,hypothesis = h4, design = d4b),showType="NHST"))

##########################

h5<-h3
h5a<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Exp","z",0.3,0,1)))
h5b<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Exp","z",0.3,0,0)))
d5<-makeDesign(sN=42,Replication=makeReplication(TRUE,Keep="Cautious"))

print(showMultiple(doMultiple(200,NULL,hypothesis = h5, design = d5),showType="NHST"))
print(showMultiple(doMultiple(200,NULL,hypothesis = h5a, design = d5),showType="NHST"))
print(showMultiple(doMultiple(200,NULL,hypothesis = h5b, design = d5),showType="NHST"))

##########################

h6<-h3
h6a<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Exp","z",0.3,0,1)))
h6b<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Exp","z",0.3,0,0)))
d6<-makeDesign(sN=42,Replication=makeReplication(TRUE,Keep="MetaAnalysis"))

print(showMultiple(doMultiple(200,NULL,hypothesis = h6, design = d6),showType="NHST"))
print(showMultiple(doMultiple(200,NULL,hypothesis = h6a, design = d6),showType="NHST"))
print(showMultiple(doMultiple(200,NULL,hypothesis = h6b, design = d6),showType="NHST"))

##########################

h6<-makeHypothesis(effect=makeEffect(rIV=0.3,rSD=0.1))
d6<-getDesign("Psych")
d6$sN<-d6$sN*15

e6<-makeEvidence(metaAnalysis = makeMetaAnalysis(TRUE,nstudies=10,analysisType="random"))

print(showMetaMultiple(doMultiple(200,NULL,hypothesis = h6, design = d6, evidence=e6)))

