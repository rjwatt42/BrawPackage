
#' set up for a meta-analysis
#' 
#' @return metaAnalysis object 
#' @examples
#' makeMetaAnalysis<-function(On=TRUE,nstudies=100,
#' analysisType="random",analysisVar="sd",
#' method="MLE",analysisPrior="none",
#' modelNulls=FALSE,modelPDF="All",
#' sourceBias=FALSE,
#' analyseBias=FALSE)
#' @export
makeMetaAnalysis<-function(On=FALSE, nstudies=10,
                           analysisType="random",analysisVar="sd",
                           method="MLE",analysisPrior="none",
                           modelNulls=FALSE,modelPDF="All",
                           sourceBias=FALSE,
                           analyseBias=FALSE) {
  metaAnalysis<-list(
    On=On,
    nstudies=nstudies,
    analysisType=analysisType,
    analysisVar=analysisVar,
    method=method,
    analysisPrior=analysisPrior,
    modelPDF=modelPDF,
    sourceBias=sourceBias,
    modelNulls=modelNulls,
    analyseBias=analyseBias
  )
  
}


