
#' set up for a meta-analysis
#' 
#' @return metaAnalysis object 
#' @examples
#' makeMetaAnalysis<-function(nstudies=100,
#' analysisType="random",
#' modelPDF="All",
#' sig_only=FALSE,
#' includeNulls=TRUE)
#' @export
makeMetaAnalysis<-function(nstudies=10,
                           analysisType="random",
                           modelPDF="All",
                           sig_only=FALSE,
                           includeNulls=TRUE) {
  
  metaAnalysis<-list(
    nstudies=nstudies,
    analysisType=analysisType,
    modelPDF=modelPDF,
    sig_only=sig_only,
    includeNulls=includeNulls
  )
  
}


