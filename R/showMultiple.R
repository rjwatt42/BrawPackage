
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "p(sig)", "CILimits", "NHST", "Hits", "Misses" \cr
#'        \emph{ or one or two of:} \cr
#'         "rs","p","ci1","ci2", "rp","n" \cr
#'          "ws","wp","nw", ro","po"
#' @param dimension "1D", "2D"
#' @param orientation "vert", "horz"
#' @return ggplot2 object - and printed
#' @examples
#' showMultiple(multipleResult=doMultiple(),
#'                        showType="Basic",
#'                        dimension="1D",
#'                        orientation="vert",
#'                        effectType="direct",showTheory=TRUE)
#' @export
showMultiple<-function(multipleResult=braw.res$multiple,showType="Basic",
                       dimension="1D",orientation="vert",
                       whichEffect="All",effectType="all",showTheory=braw.env$showTheory
) {
  if (is.null(multipleResult)) multipleResult=doMultiple(autoShow=FALSE)
  if (is.numeric(multipleResult)) multipleResult=doMultiple(multipleResult,autoShow=FALSE)

    if (!multipleResult$hypothesis$effect$world$worldOn && is.element(showType[1],c("NHST","Hits","Misses"))) {
      if (multipleResult$nullcount<multipleResult$count) {
        multipleResult<-doMultiple(0,multipleResult,doingNull=TRUE)
      }
    }
    
  if (is.element(showType[1],c("NHST","Hits","Misses","p(sig)")) &&
      !multipleResult$hypothesis$effect$world$worldOn && 
      !all(is.na(multipleResult$nullresult$rIV))) {
      if (all(multipleResult$result$rpIV==0)) multipleResult$result$rpIV<-multipleResult$result$rpIV+0.0000000001
      fullResult<-mergeMultiple(multipleResult$result,multipleResult$nullresult)
    } else fullResult<-multipleResult$result

  fullResult<-c(fullResult,list(hypothesis=multipleResult$hypothesis,
                                design=multipleResult$design,
                                evidence=multipleResult$evidence)
  )
  
  g<-showInference(fullResult,showType=showType,dimension=dimension,orientation=orientation,
                   whichEffect=whichEffect,effectType=effectType,showTheory=showTheory
  ) 
  # if (is.null(multipleResult$hypothesis$IV2) || !is.element(whichEffect,c("All","Mains")))
    # g<-addG(g,plotTitle(paste0("Multiple: ",brawFormat(multipleResult$count)),"right",size=0.85))
  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}

