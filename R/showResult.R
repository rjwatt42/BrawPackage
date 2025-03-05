
#' show the analysis of a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' showSingle(analysis=doAnalysis(),show="describe",showType="Basic",dimension="1D")
#' @export
showSingle<-function(result=braw.res$result,show="describe",showType="Basic",dimension="1D") {
  
  if (is.null(result)) result<-doSingle()
  
  switch(tolower(show),
         "data"=showSample(result),
         "describe"=showDescription(result),
         "infer"=showInference(result,showType=showType,dimension=dimension)
         )
}
