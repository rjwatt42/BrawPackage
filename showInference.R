
getNulls<-function(analysis) {
    nulls<-analysis$rpIV==0
    
    nullanalysis<-analysis
    nullanalysis$rpIV<-analysis$rpIV[nulls]
    nullanalysis$roIV<-analysis$roIV[nulls]
    nullanalysis$rIV<-analysis$rIV[nulls]
    nullanalysis$pIV<-analysis$pIV[nulls]
    nullanalysis$nval<-analysis$nval[nulls]
    nullanalysis$df1<-analysis$df1[nulls]
    
    analysis$rpIV<-analysis$rpIV[!nulls]
    analysis$roIV<-analysis$roIV[!nulls]
    analysis$rIV<-analysis$rIV[!nulls]
    analysis$pIV<-analysis$pIV[!nulls]
    analysis$nval<-analysis$nval[!nulls]
    analysis$df1<-analysis$df1[!nulls]
    
    nullanalysis$count<-sum(!is.na(nullanalysis$rIV))
    analysis$count<-sum(!is.na(analysis$rIV))
    list(analysis=analysis,nullanalysis=nullanalysis)
  }

showInference<-function(analysis=makeAnalysis(),type="Basic",dimension="1D",orientation="vert",
                        showType="direct",Theory=showTheory
) {
  oldshowTheory<-showTheory
  showTheory<<-Theory
  if (type=="2D") {
    type<-"Basic"
    dimension<-"2D"
  }
  analysis1<-analysis
  analysis2<-NA
  if (length(type)==1) {
    switch(type,
           "Basic"=     {type<-c("r","p")},
           "CILimits"=  {type<-c("ci1","ci2")},
           "NHSTErrors"={
             type<-c("e2","e1")
             r<-getNulls(analysis)
             analysis1<-r$analysis
             analysis2<-r$nullanalysis
             },
           "FDR"=       {
             type<-c("e1","e2")
             r<-getNulls(analysis)
             analysis2<-r$nullanalysis
             analysis1<-r$analysis
           },
           {type<-c(type,NA)}
    )
  } 
  if (dimension=="2D") {
    g1<-plot2Inference(analysis,type[1],type[2])
    g2<-NULL
  } else {
    if (type[1]=="e1")
      g1<-plotInference(analysis2,type[1],showType=showType,orientation=orientation)
    else
      g1<-plotInference(analysis1,type[1],showType=showType,orientation=orientation)
    if (!is.na(type[2])) {
      if (type[2]=="e1")
        g2<-plotInference(analysis2,type[2],showType=showType,orientation=orientation)
      else
        g2<-plotInference(analysis1,type[2],showType=showType,orientation=orientation)
    } else {
      g2<-NULL
    }
  }
  showTheory<<-oldshowTheory
  
  if (!is.null(g2)) {
    g<-joinPlots(g1,g2)
  } else {
    g<-joinPlots(g1)
  }
  
  return(g)
}
