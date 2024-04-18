###########################
#

design<-makeDesign(sN=200)

showDescription(doAnalysis(doSample(hypothesis=getHypothesis("II"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("IO"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("IC"),design=design)))

###########################
#
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("OI"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("OO"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("OC"),design=design)))

###########################
#
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CI"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CO"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CC"),design=design)))

###########################
#

showDescription(doAnalysis(doSample(hypothesis=getHypothesis("III"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("IIC"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("ICI"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("ICC"),design=design)))

###########################
#
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CII"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CIC"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CCI"),design=design)))
showDescription(doAnalysis(doSample(hypothesis=getHypothesis("CCC"),design=design)))
