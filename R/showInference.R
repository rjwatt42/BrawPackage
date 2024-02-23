
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
    g2<-NULL
  } else {
    if (showType[1]=="e1")
      g1<-plotInference(analysis2,showType[1],effectType=effectType,orientation=orientation,showTheory=showTheory)
    else
      g1<-plotInference(analysis1,showType[1],effectType=effectType,orientation=orientation,showTheory=showTheory)
    if (!is.na(showType[2])) {
      if (showType[2]=="e1")
        g2<-plotInference(analysis2,showType[2],effectType=effectType,orientation=orientation,showTheory=showTheory)
      else
        g2<-plotInference(analysis1,showType[2],effectType=effectType,orientation=orientation,showTheory=showTheory)
    } else {
      g2<-NULL
    }
  }

  if (!is.null(g2)) {
    g<-joinPlots(g1,g2)
  } else {
    g<-joinPlots(g1)
  }
  
  return(g)
}
