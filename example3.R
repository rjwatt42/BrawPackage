
hypothesis<-getHypothesis("3")
hypothesis$effect$rIV<-0.3
hypothesis$effect$rIV2<-0.5


showDescription(doAnalysis(doSample(hypothesis=hypothesis)))

