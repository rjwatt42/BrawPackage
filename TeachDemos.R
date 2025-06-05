###########################
# defaults
print("OK")
orientation<-"horz"
setBrawEnv("fontSize",1.5)

world<-makeWorld(TRUE,populationPDF="Exp",populationRZ="z",populationPDFk=0.3,populationNullp=0.75)
n<-42
nrep<-5

##########################
# 1a, 1b, 1c

h1<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,populationPDF="Single",populationRZ="r",populationPDFk=0.3,populationNullp=0.5)))
d1<-makeDesign(sN=n)
d1a<-makeDesign(sN=n*nrep)

print(showMultiple(doMultiple(200,NULL,hypothesis = h1, design = d1),showType="rse",orientation=orientation))
print(showMultiple(showType="sig",orientation=orientation))
print(showMultiple(showType="ns",orientation=orientation))

##########################
# 1d

d1a<-makeDesign(sN=n*nrep)

print(showMultiple(doMultiple(200,NULL,hypothesis = h1, design = d1),showType="rse",orientation=orientation))

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
h3a<-h3; h3a$effect$world$populationNullp<-0
h3b<-h3; h3b$effect$world$populationNullp<-1
d3<-makeDesign(sN=n,Replication=makeReplication(Keep="Cautious",forceSigOriginal = TRUE))

print(showMultiple(doMultiple(200,NULL,hypothesis = h3, design = d3),showType="rse",orientation=orientation))
print(showMultiple(doMultiple(200,NULL,hypothesis = h3a, design = d3),showType="nonnulls",orientation=orientation))
print(showMultiple(doMultiple(200,NULL,hypothesis = h3b, design = d3),showType="nulls",orientation=orientation))

##########################
# 4a, 4b, 4c

h4<-makeHypothesis(effect=makeEffect(world = world))
h4a<-h4; h4a$effect$world$populationNullp<-0
h4b<-h4; h4b$effect$world$populationNullp<-1
d4<-makeDesign(sN=n,Replication=makeReplication(Keep="MetaAnalysis",forceSigOriginal = TRUE))

print(showMultiple(doMultiple(200,NULL,hypothesis = h4, design = d4),showType="rse",orientation=orientation))
print(showMultiple(doMultiple(200,NULL,hypothesis = h4a, design = d4),showType="nonnulls",orientation=orientation))
print(showMultiple(doMultiple(200,NULL,hypothesis = h4b, design = d4),showType="nulls",orientation=orientation))

##########################

h5<-makeHypothesis(DV=getVariable("ExamGrade"),IV=makeVariable("Perfectionism"),IV2=makeVariable("Anxiety"),
                   effect=makeEffect(0.25,-0.5,0.5))
h5a<-makeHypothesis(DV=getVariable("ExamGrade"),IV=makeVariable("Perfectionism"),IV2=makeVariable("Anxiety"),
                   effect=makeEffect(0.25,-0.5,0))
d5<-makeDesign(150)

print(showMultiple(doMultiple(200,NULL,hypothesis=h5,design=d5),whichEffect="Main 1",effectType="total",showType="rs",orientation=orientation,showTheory=FALSE))
print(showMultiple(doMultiple(200,NULL,hypothesis=h5a,design=d5),whichEffect="Main 1",effectType="total",showType="rs",orientation=orientation,showTheory=FALSE))
print(showHypothesis(h5))

print(showMultiple(doMultiple(200,NULL,hypothesis=h5,design=d5),whichEffect="Main 1",effectType="direct",showType="rs",orientation=orientation,showTheory=FALSE))
print(showMultiple(doMultiple(200,NULL,hypothesis=h5a,design=d5),whichEffect="Main 1",effectType="direct",showType="rs",orientation=orientation,showTheory=FALSE))

print(showMultiple(doMultiple(200,NULL,hypothesis=h5,design=d5),whichEffect="Main 2",effectType="total",showType="rs",orientation=orientation,showTheory=FALSE))
print(showMultiple(doMultiple(200,NULL,hypothesis=h5a,design=d5),whichEffect="Main 2",effectType="total",showType="rs",orientation=orientation,showTheory=FALSE))
