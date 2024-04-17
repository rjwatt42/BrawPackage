
#' show the analysis of a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDescription(analysis=doAnalysis())
#' @export
showResult<-function(result=doResult(autoShow=FALSE),show="describe",showType="Basic",dimension="1D") {
  
  switch(tolower(show),
         "data"=showSample(result),
         "describe"=showDescription(result),
         "infer"=showInference(result,showType=showType,dimension=dimension)
         )
}
