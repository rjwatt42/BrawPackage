

showExpected<-function(expectedResult,type="Basic",dimension="1D",orientation="vert",
                       showType="direct",Theory=showTheory
) {
  oldshowTheory<-showTheory
  showTheory<<-Theory
  g<-showInference(expectedResult$result,type=type,dimension=dimension,orientation=orientation,
                showType=showType
  ) 
  showTheory<<-oldshowTheory
  g
}

showInference<-function(result,type="Basic",dimension="1D",orientation="vert",
                        showType="direct",Theory=showTheory
) {
  oldshowTheory<-showTheory
  showTheory<<-Theory
  if (type=="2D") {
    type<-"Basic"
    dimension<-"2D"
  }
  if (length(type)==1) {
    switch(type,
           "Basic"=     {type<-c("r","p")},
           "NHSTErrors"={type<-c("e2","e1")},
           "FDR"=       {type<-c("e1","e2")},
           "CILimits"=  {type<-c("ci1","ci2")},
                        {type<-c(type,NA)}
    )
  } 
  if (dimension=="2D") {
    g1<-plot2Inference(expectedResult$result,type[1],type[2])
    g2<-NULL
  } else {
    g1<-plotInference(expectedResult$result,type[1],showType=showType)
    if (!is.na(type[2])) {
      g2<-plotInference(expectedResult$result,type[2],showType=showType)
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