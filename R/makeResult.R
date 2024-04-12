
#' makes a sample and analyses it
#' 
#' @returns analysis object
#' @examples
#' analysis<-makeResult(hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),autoShow=braw.env$autoShow)#' make a multiple samples
#' @export
makeResult<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,autoShow=braw.env$autoShow){
  # sample<-makeSample(hypothesis=hypothesis,design=design,autoShow=FALSE)
  # result<-makeAnalysis(sample,evidence=evidence,autoShow=autoShow)
  result<-runSimulation(hypothesis=hypothesis,design=design,evidence=evidence,autoShow=autoShow)
}
