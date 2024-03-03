reRangeY<-function(y) {
  if (is.null(braw.env$plotLimits)) return(y)
  y<-(y-braw.env$plotLimits$ysc[1])/diff(braw.env$plotLimits$ysc)
  y<-y*(braw.env$plotArea[4]-braw.env$plotLimits$gap[2])+braw.env$plotArea[2]+braw.env$plotLimits$gap[2]
  return(y)
}
reRangeX<-function(x) {
  if (is.null(braw.env$plotLimits)) return(x)
  x<-(x-braw.env$plotLimits$xsc[1])/diff(braw.env$plotLimits$xsc)
  x<-x*(braw.env$plotArea[3]-braw.env$plotLimits$gap[1])+braw.env$plotArea[1]+braw.env$plotLimits$gap[1]
  return(x)
}
reRangeXY<-function(data) {
  data$x[data$x<braw.env$plotLimits$xlim[1]]<-braw.env$plotLimits$xlim[1]
  data$x[data$x>braw.env$plotLimits$xlim[2]]<-braw.env$plotLimits$xlim[2]
  data$y[data$y<braw.env$plotLimits$ylim[1]]<-braw.env$plotLimits$ylim[1]
  data$y[data$y>braw.env$plotLimits$ylim[2]]<-braw.env$plotLimits$ylim[2]
  data<-reOrientXY(data)
  data$x<-reRangeX(data$x)
  data$y<-reRangeY(data$y)
  return(data)
}
reRangeYX<-function(data) {
  data$x<-reRangeY(data$x)
  data$y<-reRangeX(data$y)
  return(data)
}
reOrientXY<-function(data,orientation=braw.env$plotLimits$orientation) {
  switch(orientation,
         "horz"={data},
         "vert"={data.frame(x=data$y,y=data$x)})
}

plotLimits<-function(xlim,ylim,orientation,gaps=c(1,1)) {
  switch(orientation,
         "horz"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=xlim,ysc=ylim,
                                           orientation=orientation,gap=gaps)},
         "vert"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=ylim,ysc=xlim,
                                           orientation=orientation,gap=gaps[c(2,1)])}
         )
}

startPlot<-function(xlim,ylim,box="both",backC=braw.env$plotColours$graphBack,orientation="horz",g=NULL) {
  switch(box,
         "x"=gaps<-c(0,1),
         "y"=gaps<-c(1,0),
         "both"=gaps<-c(1,1),
         {gaps<-c(0,0)}
  )
  gaps<-gaps*braw.env$plotArea[3:4]*0.14
  plotLimits(xlim = xlim, ylim = ylim,orientation=orientation,gaps)

  if (is.null(g)) g<-ggplot()
  back<-data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)])
  xaxis<-data.frame(x=xlim,y=ylim[1])
  yaxis<-data.frame(x=xlim[1],y=ylim)
  
  # x<-c(0,braw.env$plotArea[3])+braw.env$plotArea[1]
  # y<-c(0,braw.env$plotArea[4])+braw.env$plotArea[2]
  # g<-g+geom_polygon(data=data.frame(x=x[c(1,2,2,1)],y=y[c(1,1,2,2)]),aes(x=x,y=y),fill="red")
  if (!(is.character(backC) && backC=="transparent"))
    g<-g+dataPolygon(data=back, fill=backC, colour=backC)
  switch(box,
         "x"={
           g<-g+dataLine(data=xaxis,colour="black",linewidth=0.25)
         },
         "y"={
           g<-g+dataLine(data=yaxis,colour="black",linewidth=0.25)
         },
         "both"={
           g<-g+dataLine(data=xaxis,colour="black",linewidth=0.25)
           g<-g+dataLine(data=yaxis,colour="black",linewidth=0.25)
         }
         )
  
  g<-g+theme(legend.position = "none")+braw.env$blankTheme
}

plotTitle<-function(label) {
  ggtitle(label)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
}

xAxisLabel<-function(label) {
  axis<-data.frame(x=mean(braw.env$plotLimits$xlim),y=braw.env$plotLimits$ylim[1])
  axis<-reRangeXY(axis)
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=axis,aes(x=x-0.18*braw.env$plotArea[3],y=y),label=deparse(label), parse = TRUE,
                     hjust=0.5,vjust=0,
                     size=braw.env$labelSize*1.25,angle=90,fontface="bold")      
         },
         "horz"={
           geom_text(data=axis,aes(x=x,y=y),label=deparse(label), parse = TRUE,
                     hjust=0.5,vjust=2,
                     size=braw.env$labelSize*1.25,angle=0,fontface="bold")      
         }
  )
}
yAxisLabel<-function(label){
  axis<-data.frame(x=braw.env$plotLimits$xlim[1],y=mean(braw.env$plotLimits$ylim))
  axis<-reRangeXY(axis)
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=axis,aes(x=x,y=y-0.18*braw.env$plotArea[4]),label=deparse(label), parse = TRUE,
                     hjust=0,vjust=0.5,
                     size=braw.env$labelSize*1.25,angle=0,fontface="bold")      
         },
         "horz"={
           geom_text(data=axis,aes(x=x,y=y),label=deparse(label), parse = TRUE,
                     hjust=0.5,vjust=-2,
                     size=braw.env$labelSize*1.25,angle=90,fontface="bold")      
         }
  )
}
yAxisTicks<-function(breaks,labels=NULL){
  ticks<-reRangeXY(data.frame(x=braw.env$plotLimits$xlim[1],y=breaks))
  if (is.null(labels)) labels<-breaks
  mn<-max(4,max(nchar(labels)))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=0.5,vjust=1.1,
                     size=braw.env$labelSize*4/mn)
         },
         "horz"={
           geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
                     size=braw.env$labelSize*4/mn)
         }
  )
}
xAxisTicks<-function(breaks,labels=NULL){
  ticks<-reRangeXY(data.frame(x=breaks,y=braw.env$plotLimits$ylim[1]))
  if (is.null(labels)) labels<-breaks
  mn<-max(4,max(nchar(labels)))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
                     size=braw.env$labelSize*4/mn)
         },
         "horz"={
           geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=0.5,vjust=1.1,
                     size=braw.env$labelSize*4/mn)
         }
  )
}

vertLine<-function(intercept=NULL,linetype="solid",colour="black",alpha=1,linewidth=0.25){
  data<-data.frame(x=intercept,y=braw.env$plotLimits$ylim)
  geom_line(data=reRangeXY(data),aes(x=x,y=y),linetype=linetype, color=colour, alpha=alpha, linewidth=linewidth)
}
horzLine<-function(intercept=NULL,linetype="solid",colour="black",alpha=1,linewidth=0.25){
  data<-data.frame(x=braw.env$plotLimits$xlim,y=intercept)
  geom_line(data=reRangeXY(data),aes(x=x,y=y),linetype=linetype, color=colour, alpha=alpha, linewidth=linewidth)
}
dataLabel<-function(data,label, hjust=0, vjust=0) {
  data<-reRangeXY(data)
  switch(braw.env$plotLimits$orientation,
         "horz"=
           geom_label(data=data,aes(x = x, y = y), label=label, hjust=hjust, vjust=vjust, fill="white",size=braw.env$labelSize),
         "vert"=   
           geom_label(data=data,aes(x = x, y = y), label=label, hjust=vjust, vjust=hjust, fill="white",size=braw.env$labelSize)
         )
  
}
dataLine<-function(data,arrow=NULL,colour="black",linewidth=0.25) {
  data<-reRangeXY(data)
  geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linewidth=linewidth)
}
dataPath<-function(data,arrow=NULL,colour,linewidth) {
  data<-reRangeXY(data)
  geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linewidth=linewidth)
}
dataPoint<-function(data,shape=21,colour="black",fill="white",alpha=1,size=3) {
  data<-reRangeXY(data)
  if (is.null(data$fill)) {
    geom_point(data=data,aes(x=x,y=y),shape=shape,colour=colour,fill=fill,alpha=alpha,size=size)
  } else {
    geom_point(data=data,aes(x=x,y=y,fill=fill),shape=shape,colour=colour,alpha=alpha,size=size)
  }
}
dataBar<-function(data,colour="black",fill="white",alpha=1,barwidth) {
  bar<-data.frame(x=c(-1,1,1,-1)*barwidth/length(data$x),
                  y=c(0,0,1,1)
  )
  output<-c()
  for (i in 1:length(data$x)) {
    databar<-data.frame(x=bar$x+data$x[i],y=bar$y*data$y[i])
    output<-c(output,dataPolygon(databar,colour=colour,fill=fill[i],alpha=alpha))
  }
  return(output)
}
dataPolygon<-function(data,colour="black",fill="white",alpha=1) {
  data<-reRangeXY(data)
  if (!is.null(data$ids)) {
    geom_polygon(data=data,aes(x=x,y=y,group=ids,alpha=alpha*value),colour = colour, fill = fill)
  } else {
    if (!is.null(data$fill)) {
      geom_polygon(data=data,aes(x=x,y=y, fill = fill),colour = colour,alpha=alpha)
    } else {
      geom_polygon(data=data,aes(x=x,y=y),colour = colour, fill = fill,alpha=alpha)
    }
  }
}
dataErrorBar<-function(data,colour,linewidth) {
  data1<-reRangeXY(data.frame(x=data$x,y=data$ymin))
  data2<-reRangeXY(data.frame(x=data$x,y=data$ymax))
  if (braw.env$plotLimits$orientation=="horz"){
    data<-data.frame(x=data1$x,ymin=data1$y,ymax=data2$y)
    geom_errorbar(data=data,aes(x=x, ymin=ymin, ymax=ymax),width=0.1)
  } else {
    data<-data.frame(y=data1$y,xmin=data1$x,xmax=data2$x)
    geom_errorbarh(data=data,aes(y=y, xmin=xmin, xmax=xmax),width=0.1)
  }
}
dataLegend<-function(data,title="title",fontsize=3.5) {
  names<-data$names
  width<-max(nchar(names)+4)*0.0045*fontsize
  height<-(length(names)+1)*0.018*fontsize
  colours<-data$colours
  
  x<-braw.env$plotArea[c(1,3)]
  y<-braw.env$plotArea[c(2,4)]
  top<-1.01
  legX<-x[1]+x[2]*c(top-width,top)
  legY<-y[1]+y[2]*c(top-height,top)
  ptsX<-rep(legX[1]+legX[2]*0.025,length(colours))
  ptsY<-legY[1]+seq(1,length(colours))*0.25*(legY[2]-legY[1])
  
  titleY<-legY[1]+0.8*(legY[2]-legY[1])
  list(
    geom_polygon(data=data.frame(x=legX[c(1,2,2,1)],y=legY[c(1,1,2,2)]),
                 aes(x=x,y=y),color="black",fill="white",alpha=1),
    geom_point(data=data.frame(x=ptsX,y=ptsY),
               aes(x=x,y=y),shape=21,size=fontsize,colour="black",fill=colours),
    geom_text(data=data.frame(x=ptsX+legX[2]*0.025,y=ptsY,label=names),
              aes(x=x,y=y,label=label),hjust=0,size=fontsize*0.8),
    geom_text(data=data.frame(x=ptsX[1],y=titleY,label=title),
              aes(x=x,y=y,label=label),hjust=0,size=fontsize*0.8,fontface="bold")
  )
}