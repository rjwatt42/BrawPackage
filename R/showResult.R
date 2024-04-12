
#' show the analysis of a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showDescription(analysis=makeAnalysis())
#' @export
showResult<-function(result=makeResult(autoShow=FALSE),show="describe",showType="Basic",dimension="1D") {
  
  switch(tolower(show),
         "data"=showSample(result),
         "describe"=showDescription(result),
         "infer"=showInference(result,showType=showType,dimension=dimension)
         )
}
