
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
                       whichEffect="All",effectType="all",
                       showTheory=braw.env$showTheory,showData=TRUE,showLegend=TRUE,
                       showYaxis=TRUE
) {
  if (is.null(multipleResult)) multipleResult=doMultiple(autoShow=FALSE)
  if (is.numeric(multipleResult)) multipleResult=doMultiple(multipleResult,autoShow=FALSE)

    if (!multipleResult$hypothesis$effect$world$On && multipleResult$hypothesis$effect$rIV!=0 && is.element(showType[1],c("NHST","Hits","Misses"))) {
      if (multipleResult$nullcount<multipleResult$count) {
        multipleResult<-doMultiple(multipleResult$count-multipleResult$nullcount,multipleResult,doingNull=TRUE)
      }
    }
    
  fullResult<-multipleResult$result
  sequence<-FALSE
  if (is.null(fullResult)) {
    fullResult<-multipleResult$ResultHistory
    showType<-"rs;p"
    dimension<-"2D"
    sequence<-TRUE
  }
  if (is.element(showType[1],c("NHST","Hits","Misses","Source","Inference","p(sig)","SEM")) &&
      !multipleResult$hypothesis$effect$world$On && 
      !all(is.na(multipleResult$nullresult$rIV))) {
      if (all(multipleResult$result$rpIV==0)) multipleResult$result$rpIV<-multipleResult$result$rpIV+0.0000000001
      fullResult<-mergeMultiple(multipleResult$result,multipleResult$nullresult)
    }
  if (substr(showType[1],1,4)=="meta")
    fullResult<-multipleResult
      
  fullResult<-c(fullResult,list(hypothesis=multipleResult$hypothesis,
                                design=multipleResult$design,
                                evidence=multipleResult$evidence)
  )
  
  g<-showInference(fullResult,showType=showType,dimension=dimension,orientation=orientation,
                   whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,showData=showData,showLegend=showLegend,sequence=sequence,
                   showYaxis=showYaxis
  ) 
  # if (is.null(multipleResult$hypothesis$IV2) || !is.element(whichEffect,c("All","Mains")))
    # g<-addG(g,plotTitle(paste0("Multiple: ",brawFormat(multipleResult$count)),"right",size=0.85))
  if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
    print(g)
    return(invisible(g))
  }
  return(g)  
}

