
h1<-makeHypothesis(effect=makeEffect(0.3))
d1<-makeDesign(sN=42)

print(showMultiple(doMultiple(100,hypothesis = h1, design = d1)))

##########################

h2<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Single","r",0.3,0,0.5)))
d2<-makeDesign(sN=42)

print(showMultiple(doMultiple(100,NULL,hypothesis = h2, design = d2),showType="NHST"))

##########################

h3<-makeHypothesis(effect=makeEffect(world = makeWorld(TRUE,"Exp","z",0.3,0,0.5)))
d3<-getDesign("Psych")

print(showMultiple(doMultiple(100,NULL,hypothesis = h3, design = d3),showType="NHST"))
