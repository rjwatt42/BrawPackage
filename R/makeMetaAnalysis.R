
#' set up for a meta-analysis
#' 
#' @return metaAnalysis object 
#' @examples
#' makeMetaAnalysis<-function(nstudies=100,
#' analysisType="random",
#' modelPDF="All",
#' sigOnlySource=FALSE,
#' includeNulls=TRUE)
#' @export
makeMetaAnalysis<-function(On=FALSE, nstudies=10,
                           analysisType="random",analysisVar="sd",
                           method="MLE",
                           analysisPrior="none",modelPDF="All",
                           sigOnlySource=FALSE,
                           includeNulls=FALSE,includeBias=FALSE) {
  metaAnalysis<-list(
    On=On,
    nstudies=nstudies,
    analysisType=analysisType,
    analysisVar=analysisVar,
    method=method,
    analysisPrior=analysisPrior,
    modelPDF=modelPDF,
    sigOnlySource=sigOnlySource,
    includeNulls=includeNulls,
    includeBias=includeBias
  )
  
}


