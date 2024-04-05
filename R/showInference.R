
getNulls<-function(analysis,useSig=FALSE,useNSig=FALSE) {
  nonnulls<-which(analysis$rpIV!=0)
  nulls<-which(analysis$rpIV==0)
  if (useSig) {
    sigs<-isSignificant(braw.env$STMethod,
                        analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
    
    nonnulls<-which(analysis$rpIV!=0 & sigs)
    nulls<-which(analysis$rpIV==0 & sigs)
  }
  if (useNSig) {
    sigs<-isSignificant(braw.env$STMethod,
                        analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
    
    nonnulls<-which(analysis$rpIV!=0 & !sigs)
    nulls<-which(analysis$rpIV==0 & !sigs)
  }
  
    nullanalysis<-analysis
    nullanalysis$rIV<-analysis$rIV[nulls]
    nullanalysis$pIV<-analysis$pIV[nulls]
    nullanalysis$rpIV<-analysis$rpIV[nulls]
    nullanalysis$roIV<-analysis$roIV[nulls]
    nullanalysis$poIV<-analysis$poIV[nulls]
    nullanalysis$nval<-analysis$nval[nulls]
    nullanalysis$df1<-analysis$df1[nulls]
    
    analysis$rIV<-analysis$rIV[nonnulls]
    analysis$pIV<-analysis$pIV[nonnulls]
    analysis$rpIV<-analysis$rpIV[nonnulls]
    analysis$roIV<-analysis$roIV[nonnulls]
    analysis$poIV<-analysis$poIV[nonnulls]
    analysis$nval<-analysis$nval[nonnulls]
    analysis$df1<-analysis$df1[nonnulls]
    
    analysis$count<-sum(!is.na(analysis$rIV))
    # analysis$hypothesis$effect$world$populationNullp<-0
    
    nullanalysis$count<-sum(!is.na(nullanalysis$rIV))
    # nullanalysis$hypothesis$effect$world$populationNullp<-1
    
    list(analysis=analysis,nullanalysis=nullanalysis)
  }

#' show the estimated population characteristics from a simulated sample
#' 
#' @param showType "Basic", "CILimits", \cr
#' "NHST","fDR","fMR",
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
  analysis2<-analysis
  other1<-NULL
  other2<-NULL
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {showType<-c("r","p")},
           "Power"=     {showType<-c("w","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           "NHST"={
             showType<-c("e2","e1")
             r<-getNulls(analysis)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             other1<-analysis2
             other2<-analysis1
             },
           "fDR"=       {
             showType<-c("e2a","e1a")
             r<-getNulls(analysis,useSig=TRUE)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             other1<-analysis2
             other2<-analysis1
           },
           "fMR"=       {
             showType<-c("e2b","e1b")
             r<-getNulls(analysis,useNSig=TRUE)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             other1<-analysis2
             other2<-analysis1
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
    
    nplots<-sum(!is.na(showType))
    if (orientation=="vert") nplots<-2
    for (fi in 1:length(effectType)) {
      braw.env$plotArea<-c(0.0,0.33*(fi-1),1/nplots,area.y)
        g1<-plotInference(analysis1,otheranalysis=other1,
                          disp=showType[1],effectType=effectType[fi],orientation=orientation,showTheory=showTheory,g=g1)
      braw.env$plotArea<-c(0.5,0.33*(fi-1),1/nplots,area.y,area.y)
      if (!is.na(showType[2])) {
          g1<-plotInference(analysis2,otheranalysis=other2,
                            disp=showType[2],effectType=effectType[fi],orientation=orientation,showTheory=showTheory,g=g1)
      } 
    }
    braw.env$plotLimits<-NULL
  }

  return(g1)
}
