
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

showInference<-function(analysis=makeAnalysis(),type="Basic",dimension="1D",orientation="vert",
                        showType="direct",showTheory=TRUE
) {
  if (type[1]=="2D") {
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
      g1<-plotInference(analysis2,type[1],showType=showType,orientation=orientation,showTheory=showTheory)
    else
      g1<-plotInference(analysis1,type[1],showType=showType,orientation=orientation,showTheory=showTheory)
    if (!is.na(type[2])) {
      if (type[2]=="e1")
        g2<-plotInference(analysis2,type[2],showType=showType,orientation=orientation,showTheory=showTheory)
      else
        g2<-plotInference(analysis1,type[2],showType=showType,orientation=orientation,showTheory=showTheory)
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
