###########################
#

design<-makeDesign(sN=200)

showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("II"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("IO"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("IC"),design=design)))

###########################
#
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("OI"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("OO"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("OC"),design=design)))

###########################
#
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CI"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CO"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CC"),design=design)))

###########################
#

showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("III"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("IIC"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("ICI"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("ICC"),design=design)))

###########################
#
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CII"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CIC"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CCI"),design=design)))
showDescription(doAnalysis(makeSample(hypothesis=getHypothesis("CCC"),design=design)))
