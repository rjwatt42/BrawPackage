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
dataLabel<-function(data,label) {
  data<-reRangeXY(data)
  geom_label(data=data,aes(x = x, y = y), label=label, hjust=0, vjust=0, fill="white",size=braw.env$labelSize)
}
dataLine<-function(data,arrow=NULL,colour="black",linewidth=0.25) {
  data<-reRangeXY(data)
  geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linewidth=linewidth)
}
dataPath<-function(data,arrow=NULL,colour,linewidth,orientation) {
  data<-reRangeXY(data)
  geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linewidth=linewidth)
}
dataPoint<-function(data,shape=21,colour="black",fill="white",alpha=1,size=3) {
  data<-reRangeXY(data)
  geom_point(data=data,aes(x=x,y=y),shape=shape,colour=colour,fill=fill,alpha=alpha,size=size)
}
dataPolygon<-function(data,colour="black",fill="white",alpha=1) {
  if (!is.null(data$ids)) {
    data<-reRangeXY(data)
    geom_polygon(data=data,aes(x=x,y=y,group=ids,alpha=alpha*value),colour = colour, fill = fill)
  } else {
    data<-reRangeXY(data)
    geom_polygon(data=data,aes(x=x,y=y),colour = colour, fill = fill,alpha=alpha)
  }
}