###########################
# defaults

orientation<-"horz"

world<-makeWorld(TRUE,populationPDF="Exp",populationRZ="z",populationPDFk=0.3,populationNullp=0.75)
n<-42
nrep<-5

##########################
# 1a

h1<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,populationPDF="Single",populationRZ="r",populationPDFk=0.3,populationNullp=0.5)))
d1<-makeDesign(sN=n)
d1a<-makeDesign(sN=n*nrep)

print(showMultiple(doMultiple(200,NULL,hypothesis = h1, design = d1),showType="rse",orientation=orientation))

##########################
# 1b, 1c

print(showMultiple(doMultiple(200,NULL,hypothesis = h1, design = d1a),showType="sig",orientation=orientation))
print(showMultiple(doMultiple(200,NULL,hypothesis = h1, design = d1a),showType="ns",orientation=orientation))

##########################
# 2a

h2<-makeHypothesis(effect=makeEffect(world = world))
d2<-makeDesign(sN=n)

print(showMultiple(doMultiple(200,NULL,hypothesis = h2, design = d2),showType="rse",orientation=orientation))

##########################
# 2b, 2c

d2a<-makeDesign(sN=n*nrep)
d2b<-makeDesign(sN=n,sCheating="Retry",sCheatingAttempts=nrep)

print(showMultiple(doMultiple(200,NULL,hypothesis = h2, design = d2a),showType="rse",orientation=orientation))
print(showMultiple(doMultiple(200,NULL,hypothesis = h2, design = d2b),showType="rse",orientation=orientation))

##########################
# 3a, 3b, 3c

h3<-makeHypothesis(effect=makeEffect(world = world))
d3<-makeDesign(sN=n,Replication=makeReplication(Keep="Cautious"))

print(showMultiple(doMultiple(200,NULL,hypothesis = h3, design = d3),showType="rse",orientation=orientation))
print(showMultiple(showType="nonnulls",orientation=orientation))
print(showMultiple(showType="nulls",orientation=orientation))

##########################
# 4a, 4b, 4c

h4<-makeHypothesis(effect=makeEffect(world = world))
d4<-makeDesign(sN=n,Replication=makeReplication(Keep="MetaAnalysis"))

print(showMultiple(doMultiple(200,NULL,hypothesis = h4, design = d4),showType="rse",orientation=orientation))
print(showMultiple(showType="nonnulls",orientation=orientation))
print(showMultiple(showType="nulls",orientation=orientation))

##########################
