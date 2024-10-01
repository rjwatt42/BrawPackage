showMarginals<-function(result=braw.res$result) {
  
  if (!is.null(result$hypothesis$IV2)) {
    g<-inspectMainGraph(result$hypothesis$IV$name,result,plotArea=c(0.1,0.5,0.4,0.5))
    g<-inspectMainGraph(result$hypothesis$IV2$name,result,plotArea=c(0.5,0.5,0.4,0.5),g=g)
    g<-inspectMainGraph(result$hypothesis$DV$name,result,plotArea=c(0.3,0,0.4,0.5),g=g)
  } else {
    g<-inspectMainGraph(result$hypothesis$IV$name,result,plotArea=c(0,0.25,0.5,0.6))
    g<-inspectMainGraph(result$hypothesis$DV$name,result,plotArea=c(0.5,0.25,0.5,0.6),g=g)
  }
  
  return(g)
}


makeInspect<-function(inspectOrder="piled",
                      showFitDistribution=TRUE,
                      showMean=FALSE,
                      showSd=FALSE) {
  list(inspectOrder=inspectOrder,
       showFitDistribution=showFitDistribution,
       showMean=showMean,
       showSd=showSd
       )
}

inspectMainGraph<-function(varName,result=braw.res$result,inspect=makeInspect(),plotArea=NULL,g=NULL) {
  aspect=0.5
  
  vars<-c(result$hypothesis$DV$name,result$hypothesis$IV$name,result$hypothesis$IV2$name)
  useVar<-which(varName==vars)
  switch(useVar,
         var<-result$hypothesis$DV,
         var<-result$hypothesis$IV,
         var<-result$hypothesis$IV2)
  inspect$var<-var
  
  switch(useVar,
         data<-result$dv,
         data<-result$iv,
         data<-result$iv2)
  data<-as.numeric(data[!is.na(data)])
  inspect$data<-data
  
  n<-length(data)
  inspect$n<-n
  
  # start
  if (is.null(g)) 
    g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  if (!is.null(plotArea)) braw.env$plotArea<-plotArea
  
  switch (var$type,
          "Categorical"={
            xlim<-c(1,var$ncats)+c(-1,1)*(var$ncats-1)/2
            ticks<-data.frame(breaks=1:var$ncats,labels=var$cases)
            },
          "Ordinal"={
            xlim<-c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/4
            ticks<-data.frame(breaks=1:var$nlevs,labels=1:var$nlevs)
          },
          "Interval"={
            xlim<-c(-1,1)*4*var$sd+var$mu 
            bt<-(-4:4)*var$sd+var$mu
            lt<-bt
            ticks<-data.frame(breaks=bt,labels=lt)
          }
  )
  g<-startPlot(xlim=xlim,ylim=c(0,diff(xlim)*aspect),box="X",g=g,fontScale=1)
  g<-g+xAxisTicks(ticks$breaks,ticks$labels)+xAxisLabel(bquote(bold(.(var$name))))
  
  if (!is.null(data)) {
  # prepare data points
    ptSize<-diff(xlim)/6/sqrt(n)
    
    switch(inspect$inspectOrder,
           "unsorted"={y<-1:n},
           "sorted"={y<-rank(data,ties.method="first")},
           "piled"={
             if (var$type!="Interval") {data<-data+runif(n,-1,1)*0.2}
             y<-pile(data,ptSize)
           }
    )
    y<-y*aspect
    y<-y+ptSize
    # xc<-ptSize*cos(seq(0,2*pi,length.out=20))
    # yc<-ptSize*sin(seq(0,2*pi,length.out=20))
    # y<-y/max(y)*diff(xlim)*aspect*0.9
    
    
  # show mean
    if (inspect$showMean) {
      # vertical line
      switch (var$type,
              "Categorical"= g<-g+geom_vline(xintercept=Mode(data), colour = "red", lwd=2),
              "Ordinal"= g<-g+geom_vline(xintercept=median(data), colour = "red", lwd=2),
              "Interval"= g<-g+geom_vline(xintercept=mean(data), colour = "red", lwd=2)
      )
    }
  # show sd
    if (inspect$showSd) {
      # vertical lines
      switch (var$type,
              "Categorical"={},
              "Ordinal"={
                g<-g+geom_vline(xintercept=quantile(data,0.25), colour = "red", lwd=1)
                g<-g+geom_vline(xintercept=quantile(data,0.75), colour = "red", lwd=1)
              },
              "Interval"={
                g<-g+geom_vline(xintercept=mean(data)+std(data,1), colour = "red", lwd=1)
                g<-g+geom_vline(xintercept=mean(data)-std(data,1), colour = "red", lwd=1)
              }
      )
    }
    
    # show fitted distribution
      if (inspect$showFitDistribution) {
        switch(var$type,
               "Interval"={
                 dT<-makedrawInterval(var)
                 var$mu<-mean(inspect$data)
                 var$sd<-std(inspect$data,1)
                 var$skew<-skewness(inspect$data)
                 var$kurtosis<-kurtosis(inspect$data)
                 d<-makedrawInterval(var)
               },
               "Ordinal"={
                 dT<-makedrawOrdinal(var)
                 counts<-""
                 for (i in 1:var$nlevs){
                   counts<-paste0(counts,sum(inspect$data==i),",")
                 }
                 counts<-substr(counts,1,nchar(counts)-1)
                 var$ordProportions<-counts
                 d<-makedrawOrdinal(var)
               },
               "Categorical"={
                 dT<-makedrawCategorical(var)
                 counts<-""
                 for (i in 1:var$ncats){
                   counts<-paste0(counts,sum(inspect$data==i),",")
                 }
                 counts<-substr(counts,1,nchar(counts)-1)
                 var$proportions<-counts
                 d<-makedrawCategorical(var)
               }
        )
        pts<-dT$pts
        g<-g+dataPolygon(data=data.frame(x=pts$r,y=pts$dens*(max(y)+ptSize)),alpha=0.5, fill="white")
        pts<-d$pts
        g<-g+dataPolygon(data=data.frame(x=pts$r,y=pts$dens*(max(y)+ptSize)),alpha=0.6, fill=braw.env$plotColours$sampleC)
      }
    
    # show data points
    g<-g+dataPoint(data=data.frame(x=data,y=y),colour="black", 
                   fill=braw.env$plotColours$sampleC,size=20/sqrt(n))
    # for (i in 1:n) {
    #   g<-g+dataPolygon(data=data.frame(x=data[i]+xc,y=y[i]+yc),colour="black", fill=braw.env$plotColours$sampleC)
    # }
    
    
  # wind up
  }
  return(g)
}

pile<-function(data,ptSize) {

  space<-ptSize*2
  x<-c()
  y<-c()
  for (i in 1:length(data)){
    for (iy in seq(0,100,by=0.001)) {
      distances=sqrt((x-data[i])^2+(y-iy)^2)
      found<-any(distances<space)
      if (!found) {
        x<-c(x,data[i])
        y<-c(y,iy)
        break
      }
    }
  }
  return(y)
}

