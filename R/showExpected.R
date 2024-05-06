
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "CILimits", "NHST", "Hits" \cr
#'        \emph{ or one or two of:} \cr
#'         "rs","p","ci1","ci2", "rp","n" \cr
#'          "ws","wp","nw", ro","po"
#' @param dimension "1D", "2D"
#' @param orientation "vert", "horz"
#' @return ggplot2 object - and printed
#' @examples
#' showExpected(expectedResult=doExpected(),
#'                        showType="Basic",
#'                        dimension="1D",
#'                        orientation="vert",
#'                        effectType="direct",showTheory=TRUE)
#' @export
showExpected<-function(expectedResult=braw.res$expected,showType="Basic",
                       dimension="1D",orientation="vert",
                       effectType="direct",showTheory=braw.env$showTheory
) {
  if (is.null(expectedResult)) expectedResult=doExpected(autoShow=FALSE)
  if (is.numeric(expectedResult)) expectedResult=doExpected(expectedResult,autoShow=FALSE)

    if (!expectedResult$hypothesis$effect$world$worldOn && is.element(showType,c("NHST","Hits","Misses"))) {
      if (expectedResult$nullcount<expectedResult$count) {
        expectedResult<-doExpected(0,expectedResult,doingNull=TRUE)
      }
    }
    
    if (!expectedResult$hypothesis$effect$world$worldOn && !all(is.na(expectedResult$nullresult$rIV))) {
      if (all(expectedResult$result$rpIV==0)) expectedResult$result$rpIV<-expectedResult$result$rpIV+0.0000000001
      fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)
    } else fullResult<-expectedResult$result

  fullResult<-c(fullResult,list(hypothesis=expectedResult$hypothesis,
                                design=expectedResult$design,
                                evidence=expectedResult$evidence)
  )
  g<-showInference(fullResult,showType=showType,dimension=dimension,orientation=orientation,
                effectType=effectType,showTheory=showTheory
  ) 
  g<-g+plotTitle(paste0("Expected: ",brawFormat(expectedResult$count)),"right")
  g
}

