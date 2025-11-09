
#' set up for a meta-analysis
#' 
#' @return metaAnalysis object 
#' @examples
#' makeMetaAnalysis<-function(On=TRUE,nstudies=100,
#' analysisType="random",analysisVar="sd",
#' method="MLE",analysisPrior="none",
#' analyseNulls=FALSE,modelPDF="All",
#' sourceBias=FALSE,
#' analyseBias=FALSE)
#' @export
makeMetaAnalysis<-function(On=FALSE, nstudies=10,
                           analysisType="random",analysisVar="sd",modelPDF="All",
                           method="MLE",analysisPrior="none",
                           sourceNulls=0,analyseNulls=FALSE,
                           sourceBias=0,analyseBias=FALSE,
                           sourceAbs=FALSE) {
  metaAnalysis<-list(
    On=On,
    nstudies=nstudies,
    analysisType=analysisType,
    analysisVar=analysisVar,
    method=method,
    analysisPrior=analysisPrior,
    modelPDF=modelPDF,
    sourceBias=sourceBias,
    sourceNulls=sourceNulls,
    analyseNulls=analyseNulls,
    analyseBias=analyseBias,
    sourceAbs=sourceAbs
  )
  
}


