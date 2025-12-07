useData<-function(analysis,use) {
  analysis$rIV<-analysis$rIV[use]
  analysis$pIV<-analysis$pIV[use]
  analysis$rpIV<-analysis$rpIV[use]
  analysis$roIV<-analysis$roIV[use]
  analysis$poIV<-analysis$poIV[use]
  analysis$nval<-analysis$nval[use]
  analysis$noval<-analysis$noval[use]
  analysis$df1<-analysis$df1[use]
  analysis$rFull<-analysis$rFull[use]
  analysis$pFull<-analysis$pFull[use]
  analysis$AIC<-analysis$AIC[use]
  analysis$AICnull<-analysis$AICnull[use]
  analysis$sem<-analysis$sem[use]
  return(analysis)
}

getNulls<-function(analysisOld,evidence,useSig=FALSE,useNSig=FALSE) {
  nonnulls<-which(abs(analysisOld$rpIV)>evidence$minRp)
  nulls<-which(abs(analysisOld$rpIV)<=evidence$minRp)
  if (useSig) {
    sigs<-isSignificant(braw.env$STMethod,
                        analysisOld$pIV,analysisOld$rIV,analysisOld$nval,analysisOld$df1,evidence)
    
    nonnulls<-which(abs(analysisOld$rpIV)>evidence$minRp & sigs)
    nulls<-which(abs(analysisOld$rpIV)<=evidence$minRp & sigs)
  }
  if (useNSig) {
    sigs<-isSignificant(braw.env$STMethod,
                        analysisOld$pIV,analysisOld$rIV,analysisOld$nval,analysisOld$df1,evidence)
    
    nonnulls<-which(abs(analysisOld$rpIV)>evidence$minRp & !sigs)
    nulls<-which(abs(analysisOld$rpIV)<=evidence$minRp & !sigs)
  }
  
    nullanalysis<-useData(analysisOld,nulls)
    nullanalysis$count<-sum(!is.na(nullanalysis$rIV))
    
    analysis<-useData(analysisOld,nonnulls)
    analysis$count<-sum(!is.na(analysis$rIV))

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
                        whichEffect="All",effectType="all",showTheory=braw.env$showTheory,showData=TRUE,showLegend=FALSE,sequence=FALSE,
                        showYaxis=TRUE
) {
  if (is.null(analysis)) analysis<-doSingle(autoShow=FALSE)
  if (length(analysis$rIV)==1 && length(analysis$ResultHistory$rIV)>1) {
    analysis$rIV<-analysis$ResultHistory$rIV
    analysis$pIV<-analysis$ResultHistory$pIV
    analysis$nval<-analysis$ResultHistory$nval
    analysis$df1<-analysis$ResultHistory$df1
    analysis$rpIV<-analysis$ResultHistory$rpIV
    analysis$sequence<-analysis$ResultHistory$sequence
  } else analysis$sequence<-FALSE
  
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
           "Basic"=     {showType<-c("rs","p")},
           "p(sig)"=    {showType<-"ps";dimension<-"1D"},
           "Power"=     {showType<-c("ws","wp")},
           "CILimits"=  {showType<-c("ci1","ci2")},
           "NHST"={
             showType<-c("rse","ps1");dimension<-"1D"
             },
           "Source"={
             showType<-c("nonnulls","nulls");dimension<-"1D"
             
             use<-abs(analysis1$rpIV)>analysis$evidence$minRp
             analysis1<-useData(analysis1,use)
             analysis2<-useData(analysis2,!use)
             
           },
           "nonnulls"={
             use<-abs(analysis1$rpIV)>analysis$evidence$minRp
             analysis1<-useData(analysis1,use)
           },
           "nulls"={
             use<-abs(analysis1$rpIV)>analysis$evidence$minRp
             analysis1<-useData(analysis1,!use)
           },
           "Inference"={
             showType<-c("sig","ns");dimension<-"1D"
             
             use<-isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
             analysis1<-useData(analysis1,use!=0)
             analysis2<-useData(analysis2,use==0)
           },
           "sig"={
             use<-isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
             analysis1<-useData(analysis1,use!=0)
           },
           "ns"={
             use<-isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
             analysis1<-useData(analysis1,use==0)
           },
           "Hits"=       {
             showType<-c("e2+","e1+");dimension<-"1D"
             r<-getNulls(analysis,analysis$evidence,useSig=TRUE)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             other1<-analysis2
             other2<-analysis1
           },
           "Misses"=       {
             showType<-c("e2-","e1-");dimension<-"1D"
             r<-getNulls(analysis,analysis$evidence,useNSig=TRUE)
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
    area.y<-0
    area.w<-1
    area.h<-1
    if (!is.null(analysis$hypothesis$IV2)) {
      if (sum(analysis$evidence$AnalysisTerms)<2) effectType<-"direct"
      if (whichEffect=="All" && sum(analysis$evidence$AnalysisTerms)<2) whichEffect<-"Main 1"
      if (whichEffect=="All" && sum(analysis$evidence$AnalysisTerms)<3) whichEffect<-"Mains"
      if (whichEffect=="All") {
        if (analysis$evidence$AnalysisTerms[4])
          whichEffect<-c("Main 1","Main 2","Covariation")
          else whichEffect<-c("Main 1","Main 2","Interaction")
        area.y<-c(0,0,0)
        area.x<-c(0,0.485,0.745)
        area.w<-c(0.48,0.255,0.255)
        area.h<-c(1,1,1)
      } else
      if (whichEffect=="Mains") {
        whichEffect<-c("Main 1","Main 2")
        area.y<-c(0,0)
        area.x<-c(0,0.61)
        area.w<-c(0.6,0.38)
        area.h<-c(1,1)
      } 
    } else whichEffect<-"Main 1"
  
    g1<-nullPlot()
    
    nplots<-sum(!is.na(showType))
    if (nplots==4) {
      for (fi in 1:length(whichEffect)) {
        braw.env$plotArea<-c(0.0,0.5,0.45,0.5)
        g1<-plotInference(analysis1,otheranalysis=other1,disp=showType[1],
                          whichEffect=whichEffect[fi],effectType=effectType,
                          orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,
                          showYaxis=showYaxis,
                          g=g1)
          braw.env$plotArea<-c(0.0,0,0.45,0.5)
          g1<-plotInference(analysis2,otheranalysis=other2,disp=showType[2],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,
                            showYaxis=showYaxis,
                            g=g1)
          if (showType[3]=="SEM") braw.env$plotArea<-c(0.55,0,0.45,1)
          else                    braw.env$plotArea<-c(0.55,0.5,0.45,0.5)
        g1<-plotInference(analysis1,otheranalysis=other1,disp=showType[3],
                          whichEffect=whichEffect[fi],effectType=effectType,
                          orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,
                          showYaxis=showYaxis,
                          g=g1)
        if (showType[4]!="SEM") {
          braw.env$plotArea<-c(0.55,0,0.45,0.5)
          g1<-plotInference(analysis2,otheranalysis=other2,disp=showType[4],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,
                            showYaxis=showYaxis,
                            g=g1)
        }
      }
    } 
    if (nplots<=2) {
      pA<-braw.env$plotArea
      if (orientation=="horz") minWidth<-1 else minWidth<-0.6
      for (fi in 1:length(whichEffect)) {
        plotArea<-c(area.x[fi]/nplots,area.y[fi],area.w[fi]/nplots,area.h[fi])
        braw.env$plotArea<-plotArea*pA[c(3,4,3,4)]+c(pA[c(1,2)],0,0)
        # braw.env$plotArea<-c(area.x[fi]/nplots,area.y[fi],area.w[fi]/nplots,area.h[fi])
          g1<-plotInference(analysis1,otheranalysis=other1,disp=showType[1],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,
                            showYaxis=(fi==1),
                            g=g1)
          if (nplots==2) {
            plotArea<-c(area.x[fi]/nplots+0.5,area.y[fi],area.w[fi]/nplots,area.h[fi])
            braw.env$plotArea<-plotArea*pA[c(3,4,3,4)]+c(pA[c(1,2)],0,0)
            # braw.env$plotArea<-c(area.x[fi]/nplots+0.5,area.y[fi],area.w[fi]/nplots,area.h[fi])
          g1<-plotInference(analysis2,otheranalysis=other2,disp=showType[2],
                            whichEffect=whichEffect[fi],effectType=effectType,
                            orientation=orientation,showTheory=showTheory,showData=showData,showLegend=showLegend,
                            showYaxis=(fi==1),
                            g=g1)
          }
      }
      braw.env$plotArea<-pA
    }
  }

  if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
    showHTML(g1)
    return(invisible(g1))
  }
  if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
    print(g1)
    return(invisible(g1))
  }
  return(g1)  
}
