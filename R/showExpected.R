
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "CILimits", "NHSTErrors", "FDR", "FDR:FMR" \cr
#'        \emph{ or one or two of:} \cr
#'         "r","p","ci1","ci2", "rp","n" \cr
#'          "w","wp","nw", ro","po", 
#' @param dimension "1D", "2D"
#' @param orientation "vert", "horz"
#' @return ggplot2 object - and printed
#' @examples
#' showExpected(expectedResult=makeExpected(),
#'                        showType="Basic",
#'                        dimension="1D",
#'                        orientation="vert",
#'                        effectType="direct",showTheory=TRUE)
#' @export
showExpected<-function(expectedResult=makeExpected(),showType="Basic",
                       dimension="1D",orientation="vert",
                       effectType="direct",showTheory=TRUE
) {
  if (is.numeric(expectedResult)) expectedResult=makeExpected(expectedResult)
  
  if (is.element(showType,c("NHSTErrors","FDR"))) {
    if (expectedResult$nullcount<expectedResult$count) {
      expectedResult<-makeExpected(0,expectedResult,doingNull=TRUE)
    }
  }

  if (expectedResult$hypothesis$effect$world$worldOn) {
    fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)
  } else {
    switch (showType,
            "NHSTErrors"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
            "FDR"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
            "e1"={fullResult<-expectedResult$nullresult},
            "e2"={fullResult<-expectedResult$result},
            {fullResult<-expectedResult$result}
    )
  }
  fullResult<-c(fullResult,list(hypothesis=expectedResult$hypothesis,
                                design=expectedResult$design,
                                evidence=expectedResult$evidence)
  )
  g<-showInference(fullResult,showType=showType,dimension=dimension,orientation=orientation,
                effectType=effectType,showTheory=showTheory
  ) 
  g
}

