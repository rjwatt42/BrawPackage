
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
makeMetaAnalysis<-function(nstudies=10,
                           analysisType="random",
                           modelPDF="All",
                           sigOnlySource=FALSE,
                           includeNulls=TRUE,includeBias=FALSE) {
  
  metaAnalysis<-list(
    nstudies=nstudies,
    analysisType=analysisType,
    modelPDF=modelPDF,
    sigOnlySource=sigOnlySource,
    includeNulls=includeNulls,
    includeBias=includeBias
  )
  
}


