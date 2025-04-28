
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
#' "NHST","Hits","Misses",
#'        \emph{ or one or two of:} \cr
#' "rs","p","ci1","ci2", "rp","n"
#' @param dimension "1D", "2D"
#' @param orientation "vert", "horz"
#' @return ggplot2 object - and printed
#' @examples
#' showInference(analysis=doAnalysis(),
#'               showType="Basic",
#'               dimension="1D",
#'               orientation="vert",
#'               whichEffect="Main 1",
#'               showTheory=TRUE)
#' @export
showInference<-function(analysis=braw.res$result,showType="Basic",dimension="1D",orientation="vert",
                        whichEffect="All",effectType="all",showTheory=braw.env$showTheory
) {
  if (is.null(analysis)) analysis<-doSingle(autoShow=FALSE)
  
  if (showType[1]=="2D") {
    showType<-"Basic"
    dimension<-"2D"
  }
  if (is.numeric(dimension)) dimension<-paste0(dimension,"D")
  
  analysis1<-analysis
  analysis2<-analysis
  other1<-NULL
  other2<-NULL
  if (length(showType)==1) {
    switch(showType,
           "Single"=     {showType<-c("rs");dimension<-"1D"},
           "Basic"=     {showType<-c("rs","p");dimension<-"1D"},
           "p(sig)"=    {showType<-"ps";dimension<-"1D"},
           "Power"=     {showType<-c("ws","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           "NHST"={
             showType<-c("rse","ps1");dimension<-"1D"
             # r<-getNulls(analysis)
             # analysis1<-r$analysis
             # analysis2<-r$nullanalysis
             # other1<-analysis2
             # other2<-analysis1
             },
           "Source"={
             showType<-c("rs1","rs2");dimension<-"1D"
             
             use<-analysis1$rpIV!=0
             analysis1$rIV<-analysis1$rIV[use]
             analysis1$pIV<-analysis1$pIV[use]
             analysis1$rpIV<-analysis1$rpIV[use]
             analysis1$nval<-analysis1$nval[use]
             
             use<-!use
             analysis2$rIV<-analysis2$rIV[use]
             analysis2$pIV<-analysis2$pIV[use]
             analysis2$rpIV<-analysis2$rpIV[use]
             analysis2$nval<-analysis2$nval[use]
           },
           "Inference"={
             showType<-c("rse1","rse2");dimension<-"1D"
             
             use<-analysis1$pIV<0.05
             analysis1$rIV<-analysis1$rIV[use]
             analysis1$pIV<-analysis1$pIV[use]
             analysis1$rpIV<-analysis1$rpIV[use]
             analysis1$nval<-analysis1$nval[use]

             use<-!use
             analysis2$rIV<-analysis2$rIV[use]
             analysis2$pIV<-analysis2$pIV[use]
             analysis2$rpIV<-analysis2$rpIV[use]
             analysis2$nval<-analysis2$nval[use]
           },
           "Hits"=       {
             showType<-c("e2+","e1+");dimension<-"1D"
             r<-getNulls(analysis,useSig=TRUE)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             other1<-analysis2
             other2<-analysis1
           },
           "Misses"=       {
             showType<-c("e2-","e1-");dimension<-"1D"
             r<-getNulls(analysis,useNSig=TRUE)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             other1<-analysis2
             other2<-analysis1
           },
           "SEM"= {
             showType<-c("rss","SEM");dimension<-"1D"
           },
           "DV"= {
             showType=c("dv.mn","dv.sd","dv.sk","dv.kt");dimension<-"1D"
           },
           "Residuals"= {
             showType=c("er.mn","er.sd","er.sk","er.kt");dimension<-"1D"
           },
           { showType<-strsplit(showType,";")[[1]]
             # if (length(showType)==1) showType<-c(showType,NA)
             }
    )
  } 
  
  if (length(showType)==2 && dimension=="2D") {
    g1<-plot2Inference(analysis,showType[1],showType[2])
  } else {
    area.x<-0
    area.y<-1
    if (!is.null(analysis$hypothesis$IV2)) {
      if (whichEffect=="All" && !analysis$evidence$rInteractionOn) whichEffect<-"Mains"
      if (whichEffect=="All") {
        whichEffect<-c("Main 1","Main 2","Interaction")
        area.y<-c(1,1,1)*0.32
        area.x<-c(0,0.333,0.666)
      } else
      if (whichEffect=="Mains") {
        whichEffect<-c("Main 1","Main 2")
        area.y<-c(1,1,1)*0.47
        area.x<-c(0,0.5)
      } 
    } else whichEffect<-"Main 1"
  
    g1<-nullPlot()
    
    nplots<-sum(!is.na(showType))
    if (nplots==4) {
      for (fi in 1:length(whichEffect)) {
        braw.env$plotArea<-c(0.0,0.5,0.45,0.5)
        g1<-plotInference(analysis1,otheranalysis=other1,disp=showType[1],
                          whichEffect=whichEffect[fi],effectType=effectType,
                          orientation=orientation,showTheory=showTheory,g=g1)
          braw.env$plotArea<-c(0.0,0,0.45,0.5)
          g1<-plotInference(analysis2,otheranalysis=other2,disp=showType[2],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,g=g1)
          if (showType[3]=="SEM") braw.env$plotArea<-c(0.55,0,0.45,1)
          else                    braw.env$plotArea<-c(0.55,0.5,0.45,0.5)
        g1<-plotInference(analysis1,otheranalysis=other1,disp=showType[3],
                          whichEffect=whichEffect[fi],effectType=effectType,
                          orientation=orientation,showTheory=showTheory,g=g1)
        if (showType[4]!="SEM") {
          braw.env$plotArea<-c(0.55,0,0.45,0.5)
          g1<-plotInference(analysis2,otheranalysis=other2,disp=showType[4],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,g=g1)
        }
      }
    } 
    if (nplots<=2) {
      if (orientation=="horz") minWidth<-1 else minWidth<-0.6
      for (fi in 1:length(whichEffect)) {
          braw.env$plotArea<-c((1-1)/nplots+0.05,area.x[fi],min(minWidth,0.9/nplots),area.y[fi])
          g1<-plotInference(analysis1,otheranalysis=other1,disp=showType[1],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,g=g1)
          braw.env$plotArea<-c((2-1)/nplots+0.05,area.x[fi],min(minWidth,0.9/nplots),area.y[fi])
          g1<-plotInference(analysis2,otheranalysis=other2,disp=showType[2],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,g=g1)
      }
    }
  }

  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g1)
    return(invisible(g1))
  }
  else return(g1)  
}
