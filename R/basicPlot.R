reRangeY<-function(y) {
  if (is.null(braw.env$plotLimits)) return(y)
  y<-(y-braw.env$plotLimits$ysc[1])/diff(braw.env$plotLimits$ysc)
  y<-y*(braw.env$plotArea[4]-braw.env$plotLimits$gap[2]-braw.env$plotLimits$gap[3])+braw.env$plotArea[2]+braw.env$plotLimits$gap[2]
  return(y)
}
reRangeX<-function(x) {
  if (is.null(braw.env$plotLimits)) return(x)
  x<-(x-braw.env$plotLimits$xsc[1])/diff(braw.env$plotLimits$xsc)
  x<-x*(braw.env$plotArea[3]-braw.env$plotLimits$gap[1]-braw.env$plotLimits$gap[4])+braw.env$plotArea[1]+braw.env$plotLimits$gap[1]
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

plotLimits<-function(xlim,ylim,orientation,gaps=c(1,1,0,0),fontScale=1) {
  switch(orientation,
         "horz"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=xlim,ysc=ylim,
                                           orientation=orientation,gap=gaps,fontScale=fontScale,
                                           xAxisTickSize=5,yAxisTickSize=5)},
         "vert"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=ylim,ysc=xlim,
                                           orientation=orientation,gap=gaps[c(2,1,4,3)],fontScale=fontScale,
                                           xAxisTickSize=5,yAxisTickSize=5)}
         )
}

startPlot<-function(xlim=c(0,1),ylim=c(0,1),box="both",top=FALSE,tight=FALSE,backC=braw.env$plotColours$graphBack,orientation="horz",fontScale=1,g=NULL) {
  minGap<-0.05
  maxGap<-0.125
  if (tight) maxGap<-0.075
  switch(box,
         "X"=gaps<-c(0,maxGap),
         "x"=gaps<-c(minGap,maxGap),
         "Y"=gaps<-c(maxGap,0),
         "y"=gaps<-c(maxGap,minGap*1.2),
         "both"=gaps<-c(maxGap*1.2,maxGap),
         "Both"=gaps<-c(maxGap,maxGap),
         "none"=gaps<-c(0,0),
         {gaps<-c(minGap,minGap)}
  )
  if (top) gaps<-c(gaps,maxGap,minGap)
  else gaps<-c(gaps,minGap,minGap)
  plotLimits(xlim = xlim, ylim = ylim,orientation=orientation,gaps,fontScale=fontScale)

  if (is.null(g)) g<-ggplot()+braw.env$plotRect
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
  
  g<-g+theme(legend.position = "none")+braw.env$blankTheme()
}

plotTitle<-function(label,position="centre",size=1) {
  switch(position,
         "left"={
           dataText(data.frame(x=braw.env$plotLimits$xlim[1],y=braw.env$plotLimits$ylim[2]),label,
                    hjust=0,vjust=-1,size=size)
         },
         "centre"={
           dataText(data.frame(x=mean(braw.env$plotLimits$xlim),y=braw.env$plotLimits$ylim[2]),label,
                    hjust=0.5,vjust=-1,size=size)
         },
         "right"={
           dataText(data.frame(x=braw.env$plotLimits$xlim[2],y=braw.env$plotLimits$ylim[2]),label,
                     hjust=1,vjust=-0.1,size=size)
         },
  )
  
}

xAxisLabel<-function(label) {
  mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  if (any(mathlabel)) {
    label<-deparse(label)
  } else {
    label<-deparse(bquote(bold(.(label))))
  }
  voff<-braw.env$plotLimits$xAxisTickSize
  
  axis<-data.frame(x=mean(braw.env$plotLimits$xlim),y=braw.env$plotLimits$ylim[1])
  axis<-reRangeXY(axis)
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
                     hjust=0.5,vjust=-voff/1.5,
                     size=braw.env$labelSize*1.25*braw.env$plotLimits$fontScale,angle=90,fontface="bold")      
         },
         "horz"={
           geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
                     hjust=0.5,vjust=4/1.5,
                     size=braw.env$labelSize*1.25*braw.env$plotLimits$fontScale,angle=0,fontface="bold")      
         }
  )
}
yAxisLabel<-function(label){
  mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  if (any(mathlabel)) {
    label<-deparse(label)
  } else {
    label<-deparse(bquote(bold(.(label))))
  }
  voff<-braw.env$plotLimits$yAxisTickSize
  
  axis<-data.frame(x=braw.env$plotLimits$xlim[1],y=mean(braw.env$plotLimits$ylim))
  axis<-reRangeXY(axis)
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
                     hjust=0.5,vjust=2/1.5,
                     size=braw.env$labelSize*1.25*braw.env$plotLimits$fontScale,angle=0,fontface="bold")      
         },
         "horz"={
           geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
                     hjust=0.5,vjust=-voff,
                     size=braw.env$labelSize*1.25*braw.env$plotLimits$fontScale,angle=90,fontface="bold")      
         }
  )
}
yAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE){
  if (is.null(breaks)) {
      breaks<-axisTicks(usr=braw.env$plotLimits$ylim, log=logScale, axp = NULL, nint = 7)
  }
  
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  
  ticks<-reRangeXY(data.frame(x=braw.env$plotLimits$xlim[1],y=breaks))
  mn<-5/max(5,max(nchar(labels)))
  braw.env$plotLimits$yAxisTickSize<-min(5,max(nchar(labels)))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=0.5,vjust=1.1,
                     size=braw.env$labelSize*mn*braw.env$plotLimits$fontScale)
         },
         "horz"={
           geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
                     size=braw.env$labelSize*mn*braw.env$plotLimits$fontScale)
         }
  )
}
xAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE){
  if (is.null(breaks)) {
      breaks<-axisTicks(usr=braw.env$plotLimits$xlim, log=logScale, axp = NULL, nint = 7)
  }
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  
  yoff<-diff(braw.env$plotLimits$ylim)/20
  ticksTop<-reRangeXY(data.frame(x=breaks,y=braw.env$plotLimits$ylim[1]))
  ticksBottom<-reRangeXY(data.frame(x=breaks,y=braw.env$plotLimits$ylim[1]-yoff))
  
  sz<-100/length(breaks)
  mn<-sz*0.9/max(sz,max(nchar(labels)))
  braw.env$plotLimits$xAxisTickSize<-min(5,max(nchar(labels)))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           geom_text(data=ticksTop,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
                     size=braw.env$labelSize*mn*braw.env$plotLimits$fontScale)
         },
         "horz"={
           geom_text(data=ticksTop,aes(x=x,y=y),label=labels,hjust=0.5,vjust=1.1,
                     size=braw.env$labelSize*mn*braw.env$plotLimits$fontScale)
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
dataLabel<-function(data,label, hjust=0, vjust=0, fill="white",colour="black",parser=TRUE,fontface="plain",size=1,label.size=0.25) {
  mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  if (any(mathlabel)) {
    label<-deparse(label)
    parser<-TRUE
    voff<-1
  } else {
    if (parser) label<-deparse(bquote(.(label)))
    voff<-0
  }
  data<-reRangeXY(data)
  switch(braw.env$plotLimits$orientation,
         "horz"=
           geom_label(data=data,aes(x = x, y = y), label=label, 
                      hjust=hjust, vjust=vjust, nudge_y=voff,
               fill=fill,color=colour,fontface=fontface,
               label.size=label.size,
               size=size*braw.env$labelSize*braw.env$plotLimits$fontScale,parse=parser),
         "vert"=   
           geom_label(data=data,aes(x = x, y = y), label=label, 
                      hjust=vjust,vjust=hjust,  nudge_y=voff,
               fill=fill,color=colour,fontface=fontface,
               size=size*braw.env$labelSize),parse=TRUE
  )
  
}
dataText<-function(data,label, hjust=0, vjust=0, colour="black",size=1,fontface="plain") {
  mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  if (any(mathlabel)) {
    label<-deparse(label)
  } else {
    if (fontface=="bold")    label<-deparse(bquote(bold(.(label))))
      else     label<-deparse(bquote(.(label)))
  }
  data<-reRangeXY(data)
  
  switch(braw.env$plotLimits$orientation,
         "horz"=
           geom_text(data=data,aes(x = x, y = y), label=label, hjust=hjust, vjust=vjust, 
               color=colour,fontface=fontface,
               size=size*braw.env$labelSize,parse=TRUE),
         "vert"=   
           geom_text(data=data,aes(x = x, y = y), label=label, hjust=vjust, 
               vjust=hjust, 
               color=colour,fontface=fontface,
               size=size*braw.env$labelSize),parse=TRUE
  )
  
}
dataLine<-function(data,arrow=NULL,colour="black",linetype="solid",linewidth=0.25) {
  data<-reRangeXY(data)
  geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linetype=linetype,linewidth=linewidth)
}
dataPath<-function(data,arrow=NULL,colour="black",linetype="solid",linewidth=0.25) {
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
dataBar<-function(data,colour="black",fill="white",alpha=1,barwidth=0.85) {
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
dataPolygon<-function(data,colour="black",fill="white",alpha=1,linewidth=0.25) {
  data<-reRangeXY(data)
  if (!is.null(data$ids)) {
    geom_polygon(data=data,aes(x=x,y=y,group=ids,alpha=alpha*value),colour = colour, fill = fill,linewidth=linewidth)
  } else {
    if (!is.null(data$fill)) {
      geom_polygon(data=data,aes(x=x,y=y, fill = fill),colour = colour,alpha=alpha,linewidth=linewidth)
    } else {
      geom_polygon(data=data,aes(x=x,y=y),colour = colour, fill = fill,alpha=alpha,linewidth=linewidth)
    }
  }
}
dataErrorBar<-function(data,colour="black",linewidth=0.25) {
  data1<-reRangeXY(data.frame(x=data$x,y=data$ymin))
  data2<-reRangeXY(data.frame(x=data$x,y=data$ymax))
  width<-diff(braw.env$plotLimits$xlim)/100
  if (braw.env$plotLimits$orientation=="horz"){
    data<-data.frame(x=data1$x,ymin=data1$y,ymax=data2$y)
    geom_errorbar(data=data,aes(x=x, ymin=ymin, ymax=ymax),width=width,linewidth=linewidth)
  } else {
    data<-data.frame(y=data1$y,xmin=data1$x,xmax=data2$x)
    geom_errorbarh(data=data,aes(y=y, xmin=xmin, xmax=xmax),width=width,linewidth=linewidth)
  }
}
dataLegend<-function(data,title="title",fontsize=1.2) {
  mathlabel<-grepl("['^']{1}",title) | grepl("['[']{1}",title)
  if (any(mathlabel)) {
    title<-deparse(title)
  } else {
    title<-deparse(bquote(bold(.(title))))
  }
  
  fontsize<-fontsize*braw.env$labelSize
  names<-data$names
  width<-(max(nchar(names))*fontsize + 4*3.2)*braw.env$graphicsSize[1]/3200
  height<-(length(names)*fontsize + 1*3.2)*braw.env$graphicsSize[1]/800
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
              aes(x=x,y=y,label=label),hjust=0,size=fontsize*0.8,parse=TRUE)
  )
}

dataContour<-function(data,colour="black",breaks=c(0.1,0.3,0.5,0.7,0.9),linewidth=0.25,linetype="solid") {
  data<-reRangeXY(data)
  geom_contour(data=data,aes(x=x,y=y,z=z),colour=colour,breaks=breaks,lwd=linewidth,lty=linetype)
}

desat <- function(col,gain=1) {
  col<-(col2rgb(col)/255-0.5)*gain+0.5
  col[col<0]<-0
  col[col>1]<-1
  rgb(col[1],col[2],col[3])
}

darken <- function(col,gain=1,off=0) {
  col<-col2rgb(col)/255*gain+off
  col[col<0]<-0
  col[col>1]<-1
  rgb(col[1],col[2],col[3])
}

addTransparency <- function(col,alpha) {
  col<-col2rgb(col)/255
  rgb(col[1],col[2],col[3],alpha)
}
