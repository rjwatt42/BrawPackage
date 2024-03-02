###########################
#

design<-makeDesign(sN=200)

showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("II"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("IO"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("IC"),design=design)))

###########################
#
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("OI"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("OO"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("OC"),design=design)))

###########################
#
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CI"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CO"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CC"),design=design)))

###########################
#

showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("III"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("IIC"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("ICI"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("ICC"),design=design)))

###########################
#
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CII"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CIC"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CCI"),design=design)))
showDescription(makeAnalysis(makeSample(hypothesis=getHypothesis("CCC"),design=design)))
