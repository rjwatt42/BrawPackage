
getNulls<-function(analysis) {
    nonnulls<-which(analysis$rpIV!=0)
    nulls<-which(analysis$rpIV==0)
    
    nullanalysis<-analysis
    nullanalysis$rIV<-analysis$rIV[nulls]
    nullanalysis$pIV<-analysis$pIV[nulls]
    nullanalysis$rpIV<-analysis$rpIV[nulls]
    nullanalysis$raIV<-analysis$raIV[nulls]
    nullanalysis$roIV<-analysis$roIV[nulls]
    nullanalysis$nval<-analysis$nval[nulls]
    nullanalysis$df1<-analysis$df1[nulls]
    
    analysis$rIV<-analysis$rIV[nonnulls]
    analysis$pIV<-analysis$pIV[nonnulls]
    analysis$rpIV<-analysis$rpIV[nonnulls]
    analysis$raIV<-analysis$raIV[nonnulls]
    analysis$roIV<-analysis$roIV[nonnulls]
    analysis$nval<-analysis$nval[nonnulls]
    analysis$df1<-analysis$df1[nonnulls]
    
    analysis$count<-sum(!is.na(analysis$rIV))
    analysis$hypothesis$effect$world$populationNullp<-0
    
    nullanalysis$count<-sum(!is.na(nullanalysis$rIV))
    nullanalysis$hypothesis$effect$world$populationNullp<-1
    
    list(analysis=analysis,nullanalysis=nullanalysis)
  }

#' show the estimated population characteristics from a simulated sample
#' 
#' @param showType "Basic", "CILimits" \cr
#'        \emph{ or one or two of:} \cr
#' "r","p","ci1","ci2", "rp","n"
#' @param dimension "1D", "2D"
#' @param orientation "vert", "horz"
#' @return ggplot2 object - and printed
#' @examples
#' showInference(analysis=makeAnalysis(),
#'               showType="Basic",
#'               dimension="1D",
#'               orientation="vert",
#'               effectType="direct",
#'               showTheory=TRUE)
#' @export
showInference<-function(analysis=makeAnalysis(),showType="Basic",dimension="1D",orientation="vert",
                        effectType="direct",showTheory=TRUE
) {
  if (showType[1]=="2D") {
    showType<-"Basic"
    dimension<-"2D"
  }
  analysis1<-analysis
  analysis2<-NA
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {showType<-c("r","p")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           "NHSTErrors"={
             showType<-c("e2","e1")
             r<-getNulls(analysis)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             },
           "FDR"=       {
             showType<-c("e1","e2")
             r<-getNulls(analysis)
             analysis2<-r$nullanalysis
             analysis1<-r$analysis
           },
           {showType<-c(showType,NA)}
    )
  } 
  if (dimension=="2D") {
    g1<-plot2Inference(analysis,showType[1],showType[2])
  } else {
    if (!is.null(analysis$hypothesis$IV2) && effectType=="all") {
      effectType<-c("direct","unique","total")
      area.y<-0.25
    } else area.y<-1
    g1<-ggplot()+coord_cartesian(xlim=c(0,1),ylim=c(0,1))
    for (fi in 1:length(effectType)) {
      braw.env$plotArea<-c(0.0,0.33*(fi-1),0.5,area.y)
      if (showType[1]=="e1")
        g1<-plotInference(analysis2,showType[1],effectType=effectType[fi],orientation=orientation,showTheory=showTheory,g=g1)
      else
        g1<-plotInference(analysis1,showType[1],effectType=effectType[fi],orientation=orientation,showTheory=showTheory,g=g1)
      braw.env$plotArea<-c(0.5,0.33*(fi-1),0.5,area.y)
      if (!is.na(showType[2])) {
        if (showType[2]=="e1")
          g1<-plotInference(analysis2,showType[2],effectType=effectType[fi],orientation=orientation,showTheory=showTheory,g=g1)
        else
          g1<-plotInference(analysis1,showType[2],effectType=effectType[fi],orientation=orientation,showTheory=showTheory,g=g1)
      } 
    }
  }
 # g1<-g1+ggtitle(paste0("Expected: ",format(length(analysis$rIV)),"  "))+
 #   theme(plot.title=element_text(face='plain', size=8, hjust=0.9))

    g<-joinPlots(g1)
  return(g)
}
