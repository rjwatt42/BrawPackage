
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "CILimits", "NHST", "tDR", "fDR" \cr
#'        \emph{ or one or two of:} \cr
#'         "r","p","ci1","ci2", "rp","n" \cr
#'          "w","wp","wn", ro","po"
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
showExpected<-function(expectedResult=makeExpected(autoShow=FALSE),showType="Basic",
                       dimension="1D",orientation="vert",
                       effectType="direct",showTheory=braw.env$showTheory
) {
  if (is.numeric(expectedResult)) expectedResult=makeExpected(expectedResult,autoShow=FALSE)

  if (showType=="tDR") showType<-"fDR"
  if (!expectedResult$hypothesis$effect$world$worldOn && is.element(showType,c("NHST","fDR","fMR"))) {
    if (expectedResult$nullcount<expectedResult$count) {
      expectedResult<-makeExpected(0,expectedResult,doingNull=TRUE)
    }
  }

  if (expectedResult$hypothesis$effect$world$worldOn) {
    fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)
  } else {
    switch (showType,
            "NHST"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
            "fDR"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
            "fMR"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
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
  g<-g+plotTitle(paste0("Expected: ",brawFormat(expectedResult$count)),"right")
  g
}

