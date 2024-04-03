
showDescription()

showPossible(makePossible("Populations"),view="flat")

effect=makeEffect(rIV=0.2)
hypothesis=makeHypothesis(effect=effect)
showPossible(makePossible("Populations",hypothesis=hypothesis),walls=FALSE)

effect=makeEffect(rIV=0.2)
hypothesis=makeHypothesis(effect=effect)
showPossible(makePossible("Samples",hypothesis=hypothesis),walls=FALSE)

world<-makeWorld(worldOn=TRUE,populationPDF = "Single",populationPDFk = 0.2,populationNullp = 0.5)
hypothesis=makeHypothesis(effect=makeEffect(world=world))
showPossible(makePossible("Samples",hypothesis=hypothesis),walls=FALSE)

world<-makeWorld(worldOn=TRUE,populationPDF = "Single",populationPDFk = 0.2,populationNullp = 0.85)
hypothesis=makeHypothesis(effect=makeEffect(world=world))
showPossible(makePossible("Samples",hypothesis=hypothesis),walls=FALSE)


world<-makeWorld(worldOn=TRUE,populationPDF = "Uniform",populationRZ="r",populationPDFk = 0.2,populationNullp = 0)
hypothesis=makeHypothesis(effect=makeEffect(world=world))
showPossible(makePossible("Samples",hypothesis=hypothesis),walls=FALSE,cutaway=FALSE)

hypothesis=makeHypothesis(effect=makeEffect(rIV=0))
showPossible(makePossible("Samples",targetSample=NA,hypothesis=hypothesis),walls=FALSE)

hypothesis=makeHypothesis(effect=makeEffect(rIV=0))
showPossible(makePossible("Samples",hypothesis=hypothesis),walls=FALSE)

hypothesis=makeHypothesis(effect=makeEffect(rIV=0))
showPossible(makePossible("Samples",hypothesis=hypothesis),walls=FALSE,showP=2)

showPossible(makePossible("Samples"),walls=FALSE,showP=2)

####################################
##

showWorld(hypothesis=getHypothesis("PsychF"))
showWorld(hypothesis=getHypothesis("Psych"))

showExpected(makeExpected(0,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),showType="r",orientation = "horz")

showExpected(makeExpected(0,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),showType="p")

showExpected(makeExpected(0,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),showType="NHST")

showExpected(makeExpected(0,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),showType="tDR")
showExpected(makeExpected(0,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),showType="fMR")

####################################
##

newBrawDev(height=500,aspect=1.2)
  
showExplore(makeExplore(0,max_n=1000,xlog=TRUE,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),
            showType="p(sig)")
showExplore(makeExplore(0,max_n=1000,xlog=TRUE,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),
            showType="tDR")


showExplore(makeExplore(0,exploreType="Alpha",xlog=TRUE,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),
            showType="p(sig)")
showExplore(makeExplore(0,exploreType="Alpha",xlog=TRUE,hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),
            showType="tDR")



showExplore(makeExplore(0,exploreType="pNull",hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),
            showType="p(sig)")
showExplore(makeExplore(0,exploreType="pNull",hypothesis=getHypothesis("Psych"),design=getDesign("Psych")),
            showType="tDR")




