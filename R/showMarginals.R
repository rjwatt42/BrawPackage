showMarginals<-function(result=braw.res$result,style="piled") {
  if (is.null(result)) result<-doAnalysis()

  if (!is.null(result$hypothesis$IV2)) {
    g<-inspectMainGraph(result$hypothesis$IV$name,result,plotArea=c(0.1,0.5,0.4,0.5))
    g<-inspectMainGraph(result$hypothesis$IV2$name,result,plotArea=c(0.5,0.5,0.4,0.5),g=g)
    g<-inspectMainGraph(result$hypothesis$DV$name,result,plotArea=c(0.3,0,0.4,0.5),g=g)
  } else {
    if (style=="all") {
      g<-inspectMainGraph(result$hypothesis$IV$name,result,inspect=makeInspect("piled"),plotArea=c(0,0,0.5,0.45))
      g<-inspectMainGraph(result$hypothesis$DV$name,result,inspect=makeInspect("piled"),plotArea=c(0.5,0,0.5,0.45),g=g)
      g<-inspectMainGraph(result$hypothesis$IV$name,result,inspect=makeInspect("sorted"),plotArea=c(0,0.5,0.5,0.45),g=g)
      g<-inspectMainGraph(result$hypothesis$DV$name,result,inspect=makeInspect("sorted"),plotArea=c(0.5,0.5,0.5,0.45),g=g)
    } else {
      g<-inspectMainGraph(result$hypothesis$IV$name,result,inspect=makeInspect(style),plotArea=c(0,0.25,0.5,0.6))
      g<-inspectMainGraph(result$hypothesis$DV$name,result,inspect=makeInspect(style),plotArea=c(0.5,0.25,0.5,0.6),g=g)
    }
  }
  
  if (braw.env$graphHTML && braw.env$autoShow) {
    showHTML(g)
    return(invisible(g))
  }
  else return(g)  
}


makeInspect<-function(inspectOrder="piled",
                      showFitDistribution=TRUE,
                      showMean=TRUE,
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
  ylim<-c(0,diff(xlim)*aspect)
  g<-startPlot(xlim=xlim,ylim=ylim,
               xticks=makeTicks(ticks$breaks,ticks$labels),xlabel=makeLabel(var$name),
               # yticks=makeTicks(0,0),ylabel=makeLabel("a"),
               box="x",g=g)

  if (!is.null(data)) {
  # prepare data points
    ptSize<-diff(xlim)/6/sqrt(n)
    
    switch(inspect$inspectOrder,
           "unsorted"={
             y<-1:n
           },
           "sorted"={
             y<-rank(data,ties.method="first")
             },
           "piled"={
             if (var$type!="Interval") {data<-data+runif(n,-1,1)*0.2}
             y<-pile(data,ptSize)*aspect
           }
    )
    y<-y/max(y)*diff(ylim)*0.8
    y<-y+ptSize
    # xc<-ptSize*cos(seq(0,2*pi,length.out=20))
    # yc<-ptSize*sin(seq(0,2*pi,length.out=20))
    # y<-y/max(y)*diff(xlim)*aspect*0.9
    
    if (inspect$showFitDistribution && inspect$inspectOrder=="sorted") {
      switch(var$type,
             "Interval"={
               xs<-seq(var$mu-3*var$sd,var$mu+3*var$sd,length.out=51)
               ys<-pnorm(xs,var$mu,var$sd)*(max(y)-min(y))+min(y)
               pts<-data.frame(x=xs,y=ys)
               g<-addG(g,dataPath(data=pts))
             },
             "Ordinal"={
             },
             "Categorical"={
             }
      )
    }
    # show fitted distribution
      if (inspect$showFitDistribution && inspect$inspectOrder=="piled") {
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
        g<-addG(g,dataPolygon(data=data.frame(x=pts$r,y=pts$dens*(max(y)+ptSize)),alpha=0.5, fill="white"))
        pts<-d$pts
        g<-addG(g,dataPolygon(data=data.frame(x=pts$r,y=pts$dens*(max(y)+ptSize)),alpha=0.6, fill=braw.env$plotColours$sampleC))
      }
    
    # show data points
    g<-addG(g,dataPoint(data=data.frame(x=data,y=y),colour="black", 
                   fill=braw.env$plotColours$sampleC,size=50/sqrt(n))
    )
    # for (i in 1:n) {
    #   g<-addG(g,dataPolygon(data=data.frame(x=data[i]+xc,y=y[i]+yc),colour="black", fill=braw.env$plotColours$sampleC))
    # }
    
    
    # show mean
    if (inspect$showMean) {
      # vertical line
      switch (var$type,
              "Categorical"= xintercept<-Mode(inspect$data),
              "Ordinal"=     xintercept<-median(inspect$data),
              "Interval"=    xintercept<-mean(inspect$data)
      )
      g<-addG(g,vertLine(xintercept,colour="red",linewidth=1))
    }
    
    # show sd
    if (inspect$showSd) {
      # vertical lines
      switch (var$type,
              "Categorical"={},
              "Ordinal"={
                g<-addG(g,geom_vline(xintercept=quantile(data,0.25), colour = "red", lwd=1))
                g<-addG(g,geom_vline(xintercept=quantile(data,0.75), colour = "red", lwd=1))
              },
              "Interval"={
                g<-addG(g,geom_vline(xintercept=mean(data)+std(data,1), colour = "red", lwd=1))
                g<-addG(g,geom_vline(xintercept=mean(data)-std(data,1), colour = "red", lwd=1))
              }
      )
    }
    
  # wind up
  }
  return(g)
}

pile<-function(data,ptSize) {

  space<-ptSize*2
  x<-c()
  y<-c()
  for (i in 1:length(data)){
    for (iy in seq(0,500,by=0.01)) {
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

