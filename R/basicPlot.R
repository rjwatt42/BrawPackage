# 
#' @export
initHTML<-function(gsize=400,aspect=1.3,fontScale=1.5,xlim=c(0,1),ylim=c(0,1)) {
  braw.env <- new.env(parent = emptyenv())
  braw.env <<- list(graphicsType='HTML',
                   addHistory=FALSE,
                   plotSize=c(aspect,1)*gsize,
                   labelSize=gsize/100*fontScale,
                   dotSize=gsize/100*fontScale*1.25,
                   plotArea=c(0,0,1,1)
  )
  braw.env$plotLimits<<-plotLimits(xlim,ylim,returnValue=TRUE)
}

addGraphElement<-function(element) {
  if (braw.env$addHistory) {
    if (is.null(braw.env$history)) 
      braw.env$history<-list(list(list(type="Null",args=list())))
    if (is.null(element)) {
        braw.env$history[[length(braw.env$history)+1]]<-list(list(type="Null",args=list()))
      } else {
      b<-braw.env$history[[length(braw.env$history)]]
      b[[length(b)+1]]<-element
      braw.env$history[[length(braw.env$history)]]<-b
    }
  }
  # setBrawEnv("graph",b)
}

doGraphElement<-function(element,g) {
  args<-element$args
  switch(element$type,
         "Null"={
           g<-drawNullPlot()
         },
         "Start"={
           braw.env$plotLimits<-args
         },
         "Text"={ #   drawText(data,label, hjust, vjust, colour,fill,size,angle,dx,dy,fontface,background)
           g<-addG(g,
                   drawText(data=args[[1]],label=args[[2]], hjust=args[[3]], vjust=args[[4]], 
                            colour=args[[5]],fill=args[[6]],size=args[[7]],angle=args[[8]],dx=args[[9]],dy=args[[10]],
                            fontface=args[[11]],background=args[[12]]) 
           )
         },
         "Label"={
           g<-addG(g,
                   drawLabel(data=args[[1]],label=args[[2]], hjust=args[[3]], vjust=args[[4]], 
                             fill=args[[5]],colour=args[[6]],parser=args[[7]],
                             fontface=args[[8]],size=args[[9]],label.size=args[[10]])
           )
         },
         "Point"={
           g<-addG(g,
                   drawPoint(data=args[[1]],shape=args[[2]],colour=args[[3]],fill=args[[4]],alpha=args[[5]],size=args[[6]])
           )
         },
         "Path"={
           g<-addG(g,
                   drawPath(data=args[[1]],arrow=args[[2]],colour=args[[3]],linetype=args[[4]],linewidth=args[[5]],alpha=args[[6]])
           )
         },
         "Polygon"={
           g<-addG(g,
                   drawPolygon(data=args[[1]],colour=args[[2]],fill=args[[3]],alpha=args[[4]],linewidth=args[[5]])
           )
         }
  )
  return(g)
}

buildGraph<-function(g1=NULL) {
  if (is.null(g1)) g1<-braw.env$history[[length(braw.env$history)]]
  g<-c()
  for (i in 1:length(g1)) {
    g<-doGraphElement(g1[[i]],g)
  }
  return(g)
}

#' @export
showHistory<-function(back=-1) {
  back<-min(back,0)
  use<-max(1,length(braw.env$history)+back)
  buildGraph(braw.env$history[[use]])
}

#' set up for a meta-analysis
#' 
#' @return changes plotting environment 
#' @examples 
#' svgB0x(height=200)
#' @export
svgBox<-function(height=NULL,aspect=1.3,fontScale=braw.env$fontSize) {
  if (is.null(height) && is.null(aspect)) return(braw.env$plotSize)
  if (is.null(height)) height<-braw.env$plotSize[2]
  setBrawEnv("plotSize",c(aspect,1)*height)
  dotSize<-braw.env$dotSize/braw.env$labelSize
  setBrawEnv("labelSize",height/100*fontScale)
  setBrawEnv("dotSize",dotSize*braw.env$labelSize)
}
svgBoxX<-function() {return(braw.env$plotSize[1])}
svgBoxY<-function() {return(braw.env$plotSize[2])}
svgX<-function(x) {
  margin<-10
  return(x*(svgBoxX()-2*margin)+margin)
  }
svgY<-function(y) {
  margin<-10
  return((1-y)*(svgBoxY()-2*margin)+margin)
  }
# svgX<-function(x) {return(x*(svgBoxX()))}
# svgY<-function(y) {return((1-y)*(svgBoxY()))}

#' @export
joinHTML<-function(p1,p2) {
  paste0('<div style="display: inline-block; float left;vertical-align: text-top;"> ',
         p1,
         '</div><div style="display: inline-block;vertical-align: text-top;">',
         p2,
         '</div>')
}

addG<-function(g,...) {
  if (braw.env$graphicsType=="HTML") {
    for (i in list(...)) 
      for (j in 1:length(i))
        g<-paste0(g,i[j])
  } else {
    for (i in list(...)) g<-g+i
  }
  return(g)
}

reRangeY<-function(y) {
  if (!is.null(braw.env$plotLimits)) {
    y<-(y-braw.env$plotLimits$ysc[1])/diff(braw.env$plotLimits$ysc)
    gap0<-braw.env$plotLimits$gap[2]
    gap1<-braw.env$plotLimits$gap[4]
  } else {
    gap0<-0
    gap1<-0
  }
  y<-(gap0+y*(braw.env$plotArea[4]-gap0-gap1))+braw.env$plotArea[2]
  return(y)
}
rangeY<-function(y) {
  y<-y*braw.env$plotArea[4]+braw.env$plotArea[2]
  return(y)
}
reRangeX<-function(x) {
  if (!is.null(braw.env$plotLimits)){
    x<-(x-braw.env$plotLimits$xsc[1])/diff(braw.env$plotLimits$xsc)
    gap0<-braw.env$plotLimits$gap[1]
    gap1<-braw.env$plotLimits$gap[3]
  } else {
    gap0<-0
    gap1<-0
  }
  x<-(gap0+x*(braw.env$plotArea[3]-gap0-gap1))+braw.env$plotArea[1]
  return(x)
}
re2RangeX<-function(x) {
  if (!is.null(braw.env$plotLimits)){
    gap0<-braw.env$plotLimits$gap[1]
    gap1<-braw.env$plotLimits$gap[3]
  } else {
    gap0<-0
    gap1<-0
  }
  x<-(x-braw.env$plotArea[1]-gap0)/(braw.env$plotArea[3]-gap0-gap1)
  if (!is.null(braw.env$plotLimits)){
    x<-x*diff(braw.env$plotLimits$xsc)+braw.env$plotLimits$xsc[1]
  }
  return(x)
}
rangeX<-function(x) {
  x<-x*braw.env$plotArea[3]+braw.env$plotArea[1]
  return(x)
}
reRangeXY<-function(data) {
  if (!is.null(braw.env$plotLimits)){
    data$x[data$x<braw.env$plotLimits$xlim[1]]<-braw.env$plotLimits$xlim[1]
    data$x[data$x>braw.env$plotLimits$xlim[2]]<-braw.env$plotLimits$xlim[2]
    data$y[data$y<braw.env$plotLimits$ylim[1]]<-braw.env$plotLimits$ylim[1]
    data$y[data$y>braw.env$plotLimits$ylim[2]]<-braw.env$plotLimits$ylim[2]
  }
  data<-reOrientXY(data)
  data$x<-reRangeX(data$x)
  data$y<-reRangeY(data$y)
  return(data)
}
rangeXY<-function(data) {
  data$x<-rangeX(data$x)
  data$y<-rangeY(data$y)
  return(data)
}
reRangeYX<-function(data) {
  data$x<-reRangeY(data$x)
  data$y<-reRangeX(data$y)
  return(data)
}
reOrientXY<-function(data,orientation=braw.env$plotLimits$orientation) {
  if (!is.null(braw.env$plotLimits)) 
    switch(orientation,
           "horz"={data<-data},
           "vert"={data<-data.frame(x=data$y,y=data$x)}
    )
  return(data)
}
reSizeFont<-function(size) {
  if (braw.env$graphicsType=="HTML") return(size*braw.env$plotLimits$fontScale*3.5)
  else return(size*braw.env$plotLimits$fontScale*1.5)
}

plotLimits<-function(xlim,ylim,orientation="horz",gaps=c(1,1,0,0),fontScale=1,returnValue=FALSE) {
  gain<-4/braw.env$plotSize[c(1,2,1,2)]
  gaps<-gaps*fontScale*gain
  
  switch(orientation,
         "horz"={plotLimits<-list(xlim=xlim,ylim=ylim,xsc=xlim,ysc=ylim,
                                           orientation=orientation,gap=gaps,fontScale=fontScale,
                                           xAxisTickSize=5,yAxisTickSize=5)},
         "vert"={plotLimits<-list(xlim=xlim,ylim=ylim,xsc=ylim,ysc=xlim,
                                           orientation=orientation,gap=gaps[c(2,1,4,3)],fontScale=fontScale,
                                           xAxisTickSize=5,yAxisTickSize=5)}
  )
  if (returnValue) return(plotLimits)
  else braw.env$plotLimits<-plotLimits
}

#' this is the start of any figure - which may then contain several graphs
#' 
#' @returns a plot object
#' @examples
#' g<-nullPlot()
#' @export
nullPlot<-function(background=braw.env$plotColours$graphC) {
  addGraphElement(NULL)
  drawNullPlot(background=background)
}

drawNullPlot<-function(background=braw.env$plotColours$graphC) {  
  switch(braw.env$graphicsType,
         "ggplot"={
           g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
         },
         "HTML"={
           if (background=="#FFFFFF") col<-"rgba(0,0,0,0)"
           else col<-background
           g<-paste0(
             '<svg width=',format(svgBoxX()),' height=',format(svgBoxY()),
             ' padding:0;',
             ' style="','background-color: ',col,';','margin: auto;','" ',
             ' xmlns="http://www.w3.org/2000/svg">'
           )
         },
         "base"={
           g<-NULL
         }
         )
  return(g)
}

containsSubscript<-function(label) any(grepl('\\[([^ ]*)\\]',label))
containsSuperscript<-function(label) any(grepl('[\\^].',label))

makeTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE,angle=0) {
  return(list(breaks=breaks,labels=labels,logScale=logScale,angle=angle))
}
makeLabel<-function(label=NULL) {
  return(list(label=label))
}
startPlot<-function(xlim=c(0,1),ylim=c(0,1),gaps=NULL,box="both",top=0,
                    xticks=NULL,xlabel=NULL,xmax=FALSE,yticks=NULL,ylabel=NULL,ymax=FALSE,
                    backC=braw.env$plotColours$graphBack,orientation="horz",fontScale=1,
                    unitGap=0.4,
                    g=NULL) {
  sz<-braw.env$fullGraphSize
  # if (all(braw.env$plotArea==c(0,0,1,1))) {
  #   braw.env$plotSize<-c(1.5,1)*300*sz
  #   braw.env$labelSize<-300*sz/100*1.5
  #   braw.env$dotSize<-braw.env$labelSize*1.25
  # } else {
  #   braw.env$plotSize<-c(1.5,1)*300
  #   braw.env$labelSize<-300/100*1.5
  #   braw.env$dotSize<-braw.env$labelSize*1.25
  # }
  if (is.character(xlabel)) xlabel<-makeLabel(xlabel)
  if (is.character(ylabel)) ylabel<-makeLabel(ylabel)
  if (is.numeric(xticks)) xticks<-makeTicks(xticks)
  if (is.numeric(yticks)) yticks<-makeTicks(yticks)
  
  fontDimensions<-c(1) # this overrides plotArea in effect
  if (!is.null(xticks) || !is.null(xlabel))
    fontDimensions<-c(fontDimensions,braw.env$plotArea[4])
  if (!is.null(yticks) || !is.null(ylabel))
    fontDimensions<-c(fontDimensions,braw.env$plotArea[3])
  ez<-1
  fontShrink<-max(0.7,max(fontDimensions))^(1/ez)
  fontScale<-fontScale*fontShrink

  minGap<-0.1
  unitGap<-unitGap*braw.env$fontSize*fontScale
  # if (braw.env$graphicsType!="HTML") 
    unitGap<-unitGap*1.7
  labelGapx<-labelGapy<-unitGap
  if (containsSubscript(xlabel$label) || containsSuperscript(xlabel$label)) labelGapx<-labelGapx*1.6
  if (containsSubscript(ylabel$label) || containsSuperscript(ylabel$label)) labelGapy<-labelGapy*1.6
  labelGapx<-labelGapx*braw.env$plotArea[4]
  labelGapy<-labelGapy*braw.env$plotArea[3]
  
  if (!is.null(xticks)) {
    if (is.null(xticks$breaks))
      xticks$breaks<-axisTicks(usr=xlim, log=xticks$logScale, axp = NULL, nint = 5)
    if (is.null(xticks$labels))
      xticks$labels<-as.character(xticks$breaks)
    if (!is.character(xticks$labels)) xticks$labels<-brawFormat(xticks$labels,digits=-2)
  }
  
  if (!is.null(yticks)) {
    if (is.null(yticks$breaks))
      yticks$breaks<-axisTicks(usr=ylim, log=yticks$logScale, axp = NULL, nint = 5)
    if (is.null(yticks$labels))
      yticks$labels<-as.character(yticks$breaks)
    if (!is.character(yticks$labels)) yticks$labels<-brawFormat(yticks$labels,digits=-2)
  }
  
  if (!is.null(xticks) && !is.null(xticks$labels))
    maxtickx<-max(strNChar(xticks$labels))
  else maxtickx<-0
  if (!is.null(yticks) && !is.null(yticks$labels))
    maxticky<-max(c(3,strNChar(yticks$labels)))
  else 
    maxticky<-0
  maxtick<-max(maxtickx,maxticky)
  
  tickSize<-5/max(7,maxtick)

  bottomGap<-labelGapx+(3+0.25)*unitGap
  if (top>0) topGap<-top*unitGap*3.125 else topGap<-minGap
  leftGap<-labelGapy+(maxticky+0.25)*unitGap
  rightGap<-minGap
  
  if (!is.null(xticks)) {
    # if (!ymax)
    #   bottomGap<-labelGapx+max(strNChar(xticks$labels))*unitGap
  } else {
    bottomGap<-bottomGap-1.5*unitGap
  }
  if (!is.null(yticks)) {
    # if (!xmax)
    #   leftGap<-labelGapy+max(strNChar(yticks$labels))*unitGap
  } else {
    leftGap<-minGap
  }
  
  if (is.null(gaps)) gaps<-c(leftGap,bottomGap,rightGap,topGap)
  plotLimits(xlim = xlim, ylim = ylim,orientation=orientation,gaps,fontScale=braw.env$labelSize*fontScale)
  
  if (is.null(g)) g<-nullPlot()
  addGraphElement(list(type="Start",args=braw.env$plotLimits))
  
  if (!(is.character(backC) && backC=="transparent")) {
    back<-data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)])
    # g<-addG(g,axisPath(data=data.frame(x=c(0,1,1,0,0),y=c(0,0,1,1,0)),colour="red"))
    g<-addG(g,dataPolygon(data=back, fill=backC, colour=backC))
  }
  
  xaxis<-data.frame(x=xlim,y=ylim[1])
  yaxis<-data.frame(x=xlim[1],y=ylim)
  
  switch(box,
         "x"={
           g<-addG(g,dataLine(data=xaxis,colour="#000000",linewidth=0.25))
         },
         "y"={
           g<-addG(g,dataLine(data=yaxis,colour="#000000",linewidth=0.25))
         },
         "both"={
           g<-addG(g,dataLine(data=xaxis,colour="#000000",linewidth=0.25))
           g<-addG(g,dataLine(data=yaxis,colour="#000000",linewidth=0.25))
         }
  )
  if (!is.null(xticks)) 
    g<-addG(g,xAxisTicks(breaks=xticks$breaks,labels=xticks$labels,logScale=xticks$logScale,angle=xticks$angle,size=tickSize*braw.env$fontSize))
  if (!is.null(yticks)) 
    g<-addG(g,yAxisTicks(breaks=yticks$breaks,labels=yticks$labels,logScale=yticks$logScale,angle=yticks$angle,size=tickSize*braw.env$fontSize))
  # braw.env$plotLimits$fontScale<-braw.env$plotLimits$fontScale/fontShrink
  if (!is.null(xlabel))
    g<-addG(g,xAxisLabel(label=xlabel$label,size=tickSize*1.5*braw.env$fontSize))
  if (!is.null(ylabel))
    g<-addG(g,yAxisLabel(label=ylabel$label,size=tickSize*1.5*braw.env$fontSize))
  # braw.env$plotLimits$fontScale<-braw.env$plotLimits$fontScale*fontShrink
  return(g)  
}

plotTitle<-function(label,position="left",size=0.75,fontface="bold") {
  ypos<-1-braw.env$plotLimits$gap[4]*0.8
  switch(position,
         "left"={
           data<-data.frame(x=(braw.env$plotLimits$gap[1]), y=ypos)
           hjust<-0
         },
         "centre"={
           data<-data.frame(x=(braw.env$plotLimits$gap[1]+(1-braw.env$plotLimits$gap[1]-braw.env$plotLimits$gap[3])/2),y=ypos)
           hjust<-0.5
         },
         "right"={
           data<-data.frame(x=(1-braw.env$plotLimits$gap[3]),y=ypos)
           hjust<-1
         },
  )
  axisText(rangeXY(data),label,hjust=hjust,vjust=0.0,size=size,fontface=fontface)
}

xAxisLabel<-function(label,size=0.75) {
  if (braw.env$plotArea[3]<0.5) size<-size*0.7
  voff<-0+braw.env$plotLimits$gap[2]*0.25
  
  axis<-data.frame(x=reRangeX(mean(braw.env$plotLimits$xlim)),y=rangeY(voff))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(axis,label=label, hjust=0.5, vjust=-voff/1.5, colour="#000000",size=size,angle=90,fontface="bold")
         },
         "horz"={
           axisText(axis,label=label, hjust=0.5, vjust=0, colour="#000000",size=size,fontface="bold")
         }
  )
}
xAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE,angle=0,size=NULL){
  if (is.null(breaks)) {
    breaks<-axisTicks(usr=braw.env$plotLimits$xlim, log=logScale, axp = NULL, nint = 5)
  }
  if (is.null(labels)) labels<-breaks
  if (!is.character(labels)) labels<-brawFormat(labels,digits=2)
  if (logScale) breaks<-log10(breaks)
  # labels<-as.character(labels)
  
  if (is.null(size)) size<-7/max(7,max(strNChar(labels)))
  
  voff<-braw.env$plotLimits$ylim[1]-diff(braw.env$plotLimits$ylim)*0.01
  ticksBottom<-data.frame(x=reRangeX(breaks),y=reRangeY(voff))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(ticksBottom,label=labels, hjust=1.1, vjust=0.5, colour="#000000",size=size,fontface="plain")
         },
         "horz"={
           if (angle==0) {
             hjust=0.5
           } else {
             hjust=1.1
           }
           axisText(ticksBottom,label=labels, hjust=hjust, vjust=1, colour="#000000",size=size,fontface="plain")
         }
  )
}
yAxisLabel<-function(label,size=0.75){
  
  axis<-data.frame(x=rangeX(0.0),y=reRangeY(mean(braw.env$plotLimits$ylim)))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(axis,label=label, hjust=0.5, vjust=2/1.5, colour="#000000",size=size,fontface="bold")
         },
         "horz"={
           axisText(axis,label=label, hjust=0.5, vjust=1, colour="#000000",size=size,angle=90,fontface="bold")
         }
  )
}
yAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE,angle=0,size=NULL){
  if (is.null(breaks)) {
    breaks<-axisTicks(usr=braw.env$plotLimits$ylim, log=logScale, axp = NULL, nint = 5)
  }
  
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  # labels<-as.character(labels)
  
  hoff<-braw.env$plotLimits$xlim[1]-diff(braw.env$plotLimits$xlim)*0.0075
  ticks<-data.frame(x=reRangeX(hoff),y=reRangeY(breaks))
  if (is.null(size)) size<-4/max(4,max(strNChar(labels)))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(ticks,label=labels, hjust=0.5, vjust=1.1, colour="#000000",size=size,fontface="plain")
         },
         "horz"={
           if (angle==0) {
             hjust=1.1
           } else {
             hjust=1.1
           }
           axisText(ticks,label=labels, hjust=hjust, vjust=0.5, colour="#000000",size=size,dx=-2,fontface="plain")
         }
  )
}


vertLine<-function(intercept=NULL,linetype="solid",colour="#000000",alpha=1,linewidth=0.25){
  data<-data.frame(x=intercept,y=braw.env$plotLimits$ylim)
  return(dataPath(data=data,arrow=NULL,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha))
}
horzLine<-function(intercept=NULL,linetype="solid",colour="#000000",alpha=1,linewidth=0.25){
  data<-data.frame(x=braw.env$plotLimits$xlim,y=intercept)
  return(dataPath(data=data,arrow=NULL,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha))
}
dataLine<-function(data,arrow=NULL,colour="#000000",linetype="solid",linewidth=0.25,alpha=1) {
  return(dataPath(data,arrow=arrow,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha))
}
dataBar<-function(data,colour="#000000",fill="white",alpha=1,barwidth=0.85) {
  bar<-data.frame(x=c(-1,1,1,-1,-1)*barwidth/length(data$x),
                  y=c(0,0,1,1,0)
  )
  output<-c()
  for (i in 1:length(data$x)) {
    if (length(fill)==length(data$x)) fi<-i else fi<-1
    databar<-data.frame(x=bar$x+data$x[i],y=bar$y*data$y[i])
    output<-c(output,dataPolygon(databar,colour=colour,fill=fill[fi],alpha=alpha))
  }
  return(output)
}

# primitives from here down:
# axisLabel
# axisText
# axisPath
# axisPoint
# axisPolygon
# dataContour
is.mathLabel<-function(label) {
  return(grepl("['^']{1}",label) | grepl("['[']{1}",label))
}
mathPrepText<-function(label,bold=FALSE) {
  label<-gsub("\\[([^ ]*)\\]","\\['\\1'\\]",label)
  # label<-gsub("\\(","\\(\\(",label)
  # label<-gsub("\\)","\\)\\)",label)
  label<-gsub("=","==",label)
  label<-gsub(" ","~",label)
  label<-gsub("\u00B1([0-9.]*)","~'\u00B1 \\1'",label)
  if (bold) label<-paste0("bold(",label,")")
  return(label)
}
dataLabel<-function(data,label, hjust=0, vjust=0, fill="white",colour="#000000",size=0.8,angle=0,fontface="plain",background=FALSE,parser=TRUE,label.size=0.25) {
  if (is.character(data)) 
    switch(data,
           "topright"={
             data<-data.frame(x=braw.env$plotLimits$xlim[2],
                              y=braw.env$plotLimits$ylim[2]
             )
             hjust<-1
             vjust<-1
           },
           "centreright"={
             data<-data.frame(x=braw.env$plotLimits$xlim[2],
                              y=mean(braw.env$plotLimits$ylim)
             )
             hjust<-1
             vjust<-0.5
           },
           "bottomright"={
             data<-data.frame(x=braw.env$plotLimits$xlim[2],
                              y=braw.env$plotLimits$ylim[1]
             )
             hjust<-1
             vjust<-0
           }
    )
  data<-reRangeXY(data)
  g<-axisLabel(data,label, hjust=hjust, vjust=vjust, angle=angle, 
               fill=fill,colour=colour,parser=parser,fontface=fontface,
               size=size,label.size=label.size)
  return(g)  
}
axisLabel<-function(data,label, hjust=0, vjust=0, angle=0, fill="white",colour="#000000",parser=TRUE,fontface="plain",size=0.8,label.size=0.25) {
  addGraphElement(list(type="Label",args=list(data,label,hjust,vjust,colour,fill,parser,fontface,size,label.size)))
  drawLabel(data,label, hjust, vjust, angle, fill,colour,parser,fontface,size,label.size) 
}
drawLabel<-function(data,label, hjust=0, vjust=0, angle=0, fill="white",colour="#000000",parser=TRUE,fontface="plain",size=0.8,label.size=0.25) {
  g<-drawText(data,label, hjust=hjust, vjust=vjust, angle=angle, colour=colour,fontface=fontface,size=size,background=TRUE,fill=fill)
  return(g)
}
dataText<-function(data,label, hjust=0, vjust=0, colour="#000000",size=1,angle=0,fontface="plain",background=FALSE,fill="white") {
  data<-reRangeXY(data)
  g<-axisText(data,label, hjust=hjust, vjust=vjust, colour=colour,size=size,angle=angle,fontface=fontface,background=background,fill=fill)
  return(g)  
}
axisText<-function(data,label, hjust=0, vjust=0, colour="#000000",size=1,angle=0,dx=0,dy=0,fontface="plain",background=FALSE,fill="white") {
  addGraphElement(list(type="Text",args=list(data,label,hjust,vjust,colour,fill,size,angle,dx,dy,fontface,background)))
  drawText(data,label, hjust, vjust, colour,fill,size,angle,dx,dy,fontface,background)
}
drawText<-function(data,label, hjust=0, vjust=0, colour="#000000",fill="white",size=1,angle=0,dx=0,dy=0,fontface="plain",background=FALSE) {
  switch(braw.env$graphicsType,
         "ggplot"={
           parser<-FALSE
           if (fontface=="normal") fontface="plain"
           if (any(is.mathLabel(label))) {
             label<-mathPrepText(label,fontface=="bold")
             parser=TRUE
           } 
           
           if (braw.env$plotLimits$orientation=="vert") {
             a<-hjust; hjust<-vjust; vjust<-a
           }
           if (vjust<0.5) vjust<-0
           if (vjust>0.5) vjust<-1
           if (hjust<0.5) hjust<-0
           if (hjust>0.5) hjust<-1
           if (background) {
             if (parser && !any(is.mathLabel(label))) label<-deparse(bquote(.(label)))
             g<-geom_label(data=data,aes(x = x, y = y), label=label, angle=angle,
                           hjust=hjust, vjust=vjust,
                           fill=fill,color=colour,fontface=fontface,
                           label.padding=unit(0.1, "lines"),label.size=0.25,
                           size=reSizeFont(size)*0.8,parse=parser)
           } else {
             g<-geom_text(data=data,aes(x = x, y = y), label=label, angle=angle,
                          hjust=hjust, vjust=vjust, 
                          color=colour,fontface=fontface,
                          size=reSizeFont(size)*0.8,parse=parser)
           }
         },
         "HTML"={
           halign<-' text-anchor="start"' 
           if (hjust==0.5) halign<-' text-anchor="middle"' 
           if (hjust>0.5) halign<-' text-anchor="end"' 
           
           if (fontface=="plain") fontface="normal"
           valign<-' dominant-baseline="auto"' 
           if (vjust==0.5) valign<-' dominant-baseline="middle"'
           if (vjust>0.5) valign<-' dominant-baseline="text-before-edge"'
           
           if (fontface=="plain") fontface="normal"
           
           size<-size*1.2
           
           x<-svgX(data$x)
           y<-svgY(data$y)
           # if (containsSubscript(label)) y<-y-0.025*braw.env$plotArea[4]*svgBoxY()
           labels<-""
           
           if (!background) filter<-'' else {
             labels<-paste0(
               '  <filter x="0" y="0" width="1" height="1" id="bg-',fill,'">',
               '  <feFlood flood-color="',fill,'"/>',
               '  <feComposite in="SourceGraphic" />',
               '  </filter>'
             )
             filter<-paste0(' filter="url(#bg-',fill,')"')
           }
           sub_super_offset<-format(6*size)
           if (angle!=0) sub_super_offset<-format(6*size*braw.env$plotSize[1]/braw.env$plotSize[2])

           for (i in 1:length(x)) {
             thisLabel<-label[i]
             thisLabel<-gsub("'","",thisLabel)
               thisLabel<-gsub('\\[([^ ]*?)\\]',
                               paste0('</tspan><tspan ',valign,' font-size="',reSizeFont(size)*0.8,
                                      '" dx="0" dy="',sub_super_offset,'">',
                                      '\\1</tspan><tspan',valign,' dx="0" dy="-',sub_super_offset,'">'),
                               thisLabel)
               thisLabel<-gsub('\\^([^ ]*?) ',
                               paste0('</tspan><tspan ',valign,' font-size="',reSizeFont(size)*0.8,
                                      '" dx="0" dy="-',sub_super_offset,'">',
                                      '\\1</tspan><tspan',valign,' dx="0" dy="',sub_super_offset,'">'),
                               thisLabel)
             thisLabel<-paste0(
               '<tspan',halign,valign,
               ' dx=',dx,'px',
               ' dy=',-dy,'px',
               '>',
               thisLabel,
               '</tspan>'
             )
             
             labels<-paste0(labels,
                            '<text ',
                            filter,
                            ' x="',x[i],'"',
                            ' y="',y[i],'"',
                            ' fill="',colour,'"',
                            ' text-anchor="middle"', valign,
                            ' transform="rotate(',-angle,',',x[1],',',y[1],')"',
                            ' font-size="',reSizeFont(size),'"',
                            ' font-weight="',fontface,'"',
                            ' font-family="Arial, Helvetica, sans-serif"',
                            '>',
                            thisLabel,
                            '</text>'
             )
           }
           g<-labels
         },
         "base"={
           g<-NULL
         })
  return(g)
}

dataPath<-function(data,arrow=NULL,colour="#000000",linetype="solid",linewidth=0.25,alpha=1) {
  data<-reRangeXY(data)
  axisPath(data,arrow=arrow,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha)
}
axisPath<-function(data,arrow=NULL,colour="#000000",linetype="solid",linewidth=0.25,alpha=1) {
  addGraphElement(list(type="Path",args=list(data,arrow,colour,linetype,linewidth,alpha)))
  drawPath(data,arrow,colour,linetype,linewidth,alpha)
}
drawPath<-function(data,arrow=NULL,colour="#000000",linetype="solid",linewidth=0.25,alpha=1) {
  switch(braw.env$graphicsType,
         "ggplot"={
           if (!is.null(arrow)) arrow<-grid::arrow(angle=20,length=unit(0.4,"cm"))
           g<-geom_path(data=data,aes(x=x,y=y),arrow=arrow,color=colour,alpha=alpha,
                        linetype=linetype,linewidth=linewidth)
         },
         "HTML"={
           ls<-''
           if (linetype=="dotted") ls<-' stroke-dasharray="2,2"'
           if (linetype=="dashed") ls<-' stroke-dasharray="4,1"'
           linestyle<-paste0(' fill="none" stroke="',colour,'"',
                             ls,
                             ' stroke-width="',linewidth*2.5,'"',
                             ' stroke-opacity="',alpha,'"')
           x<-svgX(data$x)
           y<-svgY(data$y)
           points<-' points="'
           for (i in 1:length(x))  {
             points<-paste0(points,' ',format(x[i],digits=braw.env$htmlDigits),',',format(y[i],digits=braw.env$htmlDigits))
           }
           points<-paste0(points,'"')
           g<-paste0(
             '<polyline',
             points,
             linestyle,
             ' />'
           )
           
           if (!is.null(arrow)) {
             for (i in 2:length(x)) {
             direction<-atan2(diff(y[(i-1):i]),diff(x[(i-1):i]))
             lineLength<-sqrt(diff(y[(i-1):i])^2+diff(x[(i-1):i])^2)
             finLength<-8
             finAngle<-20/(180/pi)
             ax1<-x[i]-cos(direction+finAngle)*finLength
             ay1<-y[i]-sin(direction+finAngle)*finLength
             ax2<-x[i]-cos(direction-finAngle)*finLength
             ay2<-y[i]-sin(direction-finAngle)*finLength
             g1<-paste0(
               '<polyline',
               paste0(' points=" ',format(ax1,digits=braw.env$htmlDigits),',',format(ay1,digits=braw.env$htmlDigits),
                               ' ',format(x[i],digits=braw.env$htmlDigits),',',format(y[i],digits=braw.env$htmlDigits),
                               ' ',format(ax2,digits=braw.env$htmlDigits),',',format(ay2,digits=braw.env$htmlDigits),
                      '"'),
               linestyle,
               ' />'
             )
             g<-paste0(g,g1)
             }
           }
         },
         "base"={
           
         })
  return(g)
}
dataPoint<-function(data,shape=21,colour="#000000",fill="white",alpha=1,size=3,strokewidth=1) {
  data<-reRangeXY(data)
  axisPoint(data=data,shape=shape,colour=colour,fill=fill,alpha=alpha,size=size,strokewidth=strokewidth)
}
axisPoint<-function(data,shape=21,colour="#000000",fill="white",alpha=1,size=3,strokewidth=1) {
  addGraphElement(list(type="Point",args=list(data,shape,colour,fill,alpha,size,strokewidth=strokewidth)))
  drawPoint(data,shape,colour,fill,alpha,size,strokewidth=strokewidth)
}
drawPoint<-function(data,shape=21,colour="#000000",fill="white",alpha=1,size=3,strokewidth=1) {
  size<-0.75*size*(braw.env$plotArea[4])^0.5
  # if (alpha<1) colour=fill
  switch(braw.env$graphicsType,
         "ggplot"={
           sz<-size*1.35
           if (is.null(data$fill)) {
             g<-geom_point(data=data,aes(x=x,y=y),shape=shape,size=sz,
                           stroke=strokewidth*size/6.7,colour=colour,fill=fill,alpha=alpha)
           } else {
             g<-geom_point(data=data,aes(x=x,y=y,fill=fill),shape=shape,sz=size,
                           stroke=strokewidth*size/6.7,colour=colour,alpha=alpha)
           }
         },
         "HTML"={
           x<-svgX(data$x)
           y<-svgY(data$y)
           if (length(x)==0) return("")
           if (length(size)<length(x)) size<-rep(size,ceiling(length(x)/length(size)))
           if (length(colour)<length(x)) colour<-rep(colour,ceiling(length(x)/length(colour)))
           if (length(fill)<length(x)) fill<-rep(fill,ceiling(length(x)/length(fill)))
           if (length(alpha)<length(x)) alpha<-rep(alpha,ceiling(length(x)/length(alpha)))
           alpha<-format(alpha,digits=braw.env$htmlDigits)
           sw<-format(strokewidth*size/5,digits=braw.env$htmlDigits)
           if (shape==21) {
             sz<-format(size*7/pi,digits=braw.env$htmlDigits)
             g<-""
             for (i in 1:length(x)) {
               g<-paste0(g,
                         '<circle cx="',format(x[i],digits=braw.env$htmlDigits),'" cy="',format(y[i],digits=braw.env$htmlDigits),'"',
                         ' r="',sz[i],'"',
                         ' fill="',fill[i],'"',
                         ' fill-opacity="',alpha[i],'"',
                         ' stroke="',colour[i],'" stroke-width="',sw[i],'"',
                         ' stroke-opacity="',alpha[i],'"',
                         ' />'
               )
             }
           } else {
             sz<-format(size*4,digits=braw.env$htmlDigits)
             g<-""
             for (i in 1:length(x)) {
               if (shape==22) tr="" 
               else           tr=paste0(' transform=rotate(45,',format(x[i],digits=braw.env$htmlDigits),',',format(y[i],digits=braw.env$htmlDigits),')')
               g<-paste0(g,
                         '<rect x="',format(x[i]-size[i]*2,digits=braw.env$htmlDigits),'" y="',format(y[i]-size[i]*2,digits=braw.env$htmlDigits),'"',
                         ' width="',sz[i],'"',' height="',sz[i],'"',
                         ' rx="0" ry="0"',
                         ' fill="',fill[i],'"',
                         ' fill-opacity="',alpha[i],'"',
                         ' stroke="',colour[i],'" stroke-width="',sw[i],'"',
                         ' stroke-opacity="',alpha[i],'"',
                         tr,
                         ' />'
               )
             }
           }
         },
         "base"={
           
         })
  return(g)
}
dataPolygon<-function(data,colour="#000000",fill="white",alpha=1,linewidth=0.25,linetype="solid") {
  data<-reRangeXY(data)
  g<-axisPolygon(data,colour=colour,fill=fill,alpha=alpha,linewidth=linewidth,linetype=linetype)
  return(g)
}
axisPolygon<-function(data,colour="#000000",fill="white",alpha=1,linewidth=0.25,linetype="solid") {
  addGraphElement(list(type="Polygon",args=list(data,colour,fill,alpha,linewidth,linetype)))
  drawPolygon(data,colour,fill,alpha,linewidth,linetype)
}
drawPolygon<-function(data,colour="#000000",fill="white",alpha=1,linewidth=0.25,linetype="solid") {
  switch(braw.env$graphicsType,
         "ggplot"={
           if (!is.na(colour) && colour=="none") colour=NA
           if (!is.null(data$ids)) {
             g<-geom_polygon(data=data,aes(x=x,y=y,group=ids,alpha=alpha*value),colour = colour, fill = fill,linewidth=linewidth,linetype=linetype)
           } else {
             if (!is.null(data$fill)) {
               g<-geom_polygon(data=data,aes(x=x,y=y, fill = fill),colour = colour,alpha=alpha,linewidth=linewidth,linetype=linetype)
             } else {
               g<-geom_polygon(data=data,aes(x=x,y=y),colour = colour, fill = fill,alpha=alpha,linewidth=linewidth,linetype=linetype)
             }
           }
         },
         "HTML"={
           opacity<-1
           if (!is.na(colour) && colour=="none") colour=NA
           if (is.na(colour)) colour<-fill
           x<-svgX(data$x)
           y<-svgY(data$y)
           ls<-''
           if (linetype=="dotted") ls<-' stroke-dasharray="2,2"'
           if (linetype=="dashed") ls<-' stroke-dasharray="4,1"'
           if (!is.null(data$ids)) {
             g<-""
             for (i in seq(1,length(x),4)) {
               linestyle<-paste0(' fill="',fill,'"',
                                 ' fill-opacity="',format(alpha*data$value[i],digits=braw.env$htmlDigits),'"',
                                 ' stroke="',colour,'"',ls,
                                 ' stroke-width="',linewidth*2,'"',
                                 ' stroke-opacity="',format(alpha*data$value[i],digits=braw.env$htmlDigits),'"')
               points<-' points="'
               for (j in 1:4)  points<-paste0(points,' ',format(x[i+j-1],digits=braw.env$htmlDigits),',',format(y[i+j-1],digits=braw.env$htmlDigits))
               points<-paste0(points,'"')
               g<-paste0(g,
                         '<polyline',
                         points,
                         linestyle,
                         ' />'
               )
             }
           } else {
             linestyle<-paste0(' fill="',fill,'" stroke="',colour,'"',
                               ' fill-opacity="',alpha,'"',
                               ' stroke-width="',linewidth*2,'"',
                               ' stroke-opacity="',opacity,'"')
             points<-' points="'
             for (i in 1:length(x)) points<-paste0(points,' ',format(x[i],digits=braw.env$htmlDigits),',',format(y[i],digits=braw.env$htmlDigits))
             # points<-paste0(points,' ',format(x[1]),',',format(y[1]))
             points<-paste0(points,'"')
             
             g<-paste0(
               '<polyline',
               points,
               linestyle,
               ' />'
             )
           }
         },
         "base"={
           
         })
  return(g)
}
dataErrorBar<-function(data,colour="#000000",linewidth=0.25) {
  data1<-data.frame(x=data$x,y=data$ymin)
  data2<-data.frame(x=data$x,y=data$ymax)
  width<-diff(braw.env$plotLimits$xlim)/100
  if (braw.env$plotLimits$orientation=="horz"){
    data<-data.frame(x=data1$x,ymin=data1$y,ymax=data2$y)
  } else {
    data<-data.frame(y=data1$y,xmin=data1$x,xmax=data2$x)
  }
  i<-1
  g<-c()
  for (i in 1:length(data$x)) {
    thisError<-data.frame(x=rep(data$x[i],2),y=c(data$ymin[i],data$ymax[i]))
    g<-c(g,dataPath(thisError,colour=colour,linewidth=linewidth))
  }
  return(g)
}
strNChar<-function(str) {
  str1<-gsub("'","",str)
  str2<-gsub("\\[[[:punct:][:alnum:]]*\\]","",str1)
  nsub<-nchar(str1)-nchar(str2)
  nsub[nsub>0]<-nsub[nsub>0]-2
  nsub<-nsub+(is.mathLabel(str) & grepl("=",str))*2
  nother<-nchar(str2)
  nother<-nother-(is.mathLabel(str) & grepl("=",str))*1
  return(nother+nsub*0.6)
}
dataLegend<-function(data,title="",titleCol="black",fontsize=1,shape=21,location="right") {
  fontsize=0.6*fontsize*braw.env$fontSize
  dy=0.06*fontsize/braw.env$plotArea[4]
  dx=0.03*fontsize/braw.env$plotArea[3] # because rangeX() below
  if (nchar(title)>0) tn<-1.2 else tn<-0
  nrows<-tn+length(data$names)+1
  ncols<-max(c(strNChar(title)*1.5,strNChar(data$names)+1))+2
  # ncols<-max(strwidth(parse(text=mathPrepText(c(title,data$names)))))/strwidth("e")+2
  switch(location,
         "right"={xL<-1-ncols*dx},
         "left"={xL<-0}
  )
  
  g<-list(axisPolygon(data=data.frame(x=rangeX(c(xL,xL+ncols*dx,xL+ncols*dx,xL,xL)),
                                      y=rangeY(c(1-nrows*dy,1-nrows*dy,1,1,1-nrows*dy))),
                      fill="white",colour="white",linewidth=0.5)
  )
  g<-c(g,list(axisPath(data=data.frame(x=rangeX(c(xL,xL+ncols*dx,xL+ncols*dx,xL,xL)),
                                       y=rangeY(c(1-nrows*dy,1-nrows*dy,1,1,1-nrows*dy))),
                       colour="#000000",linewidth=0.5))
  )
  if (tn>0)
    g<-c(g,list(axisText(data=data.frame(x=rangeX(xL+2*dx),y=rangeY(1-dy*tn)),label=title,colour=titleCol,size=fontsize,fontface="bold"))
    )
  
  if (length(shape)<length(data$names)) shape<-rep(shape,length(data$names))
  for (i in 1:length(data$names)) {
    if (!is.na(data$colours[i]))
      g<-c(g,
           list(axisPoint(data=data.frame(x=rangeX(xL+dx*1.5),y=rangeY(1-dy*(i+tn-0.05))),
                          fill=data$colours[i],shape=shape[i]))
      )
    g<-c(g,
         list(axisText(data=data.frame(x=rangeX(xL+3.5*dx),y=rangeY(1-dy*(i+tn))),
                       label=data$names[i],vjust=0.5,size=fontsize))
    )
  }
  return(g)
}

dataContour<-function(data,colour="#000000",fill=NA,breaks=seq(0.1,0.9,0.2),linewidth=0.25,linetype="solid") {
  data<-reRangeXY(data)
  # because the x & y values may have been truncated to ylim
  usex<-c(diff(data$x)>0,TRUE)
  usey<-c(diff(data$y)>0,TRUE)
  data$x<-data$x[usex]
  data$y<-data$y[usey]
  data$z<-data$z[usey,usex]
  # now we need to worry about corners
  ny<-length(data$y)
  nx<-length(data$x)
  data$x<-c(data$x[1]-diff(data$x[1:2]),data$x,data$x[nx]+diff(data$x[c(nx-1,nx)]))
  data$y<-c(data$y[1]-diff(data$y[1:2]),data$y,data$y[ny]+diff(data$y[c(ny-1,ny)]))
  data$z<-rbind(data$z[1,]*0,data$z,data$z[ny,]*0)
  data$z<-cbind(data$z[,1]*0,data$z,data$z[,nx]*0)
  # then proceed
  c<-contourLines(data$y,data$x,data$z/max(data$z),levels=breaks)
  ny<-length(data$y)
  nx<-length(data$x)
  c1<-contourLines(c(data$y[1]-diff(data$y[1:2]),data$y,data$y[ny]+diff(data$y[1:2])),
                   c(data$x[1]-diff(data$x[1:2]),data$x,data$x[nx]+diff(data$x[1:2])),
                   cbind(0,rbind(0,data$z,0),0),levels=breaks)
  ct<-list()
  if (is.na(fill)) {
    for (i in 1:length(c)) {
      if (length(c[[i]]$x)>4) {
      cdata<-data.frame(x=c[[i]]$y,y=c[[i]]$x)
      ct<-c(ct,list(axisPath(cdata,colour=colour,linewidth=linewidth,linetype=linetype)))
      }
    }
  } else {
    for (i in 1:length(c)) {
      if (length(c[[i]]$x)>4) {
        cdata<-data.frame(x=c[[i]]$y,y=c[[i]]$x)
        fill1<-darken(desat(fill,i/length(c)),off=i/length(c))
      ct<-c(ct,list(axisPolygon(cdata,colour=colour,fill=fill1,alpha=0.4,linewidth=linewidth)))
      }
    }
  }
  return(ct)
  # x1<-as.vector(matrix(rep(data$x,101),length(data$y),byrow=TRUE))
  # y1<-as.vector(matrix(rep(data$y,101),length(data$x),byrow=FALSE))
  # z1<-as.vector(data$z)
  # geom_contour(data=data,aes(x=x1,y=y1,z=z1),colour=colour,breaks=breaks,lwd=linewidth,lty=linetype)
}

desat <- function(col,gain=1) {
  col<-(col2rgb(col)/255-0.5)*gain+0.5
  col[col<0]<-0
  col[col>1]<-1
  rgb(col[1],col[2],col[3])
}

darken <- function(col,gain=1,off=0) {
  if (is.character(col) && nchar(col)==4)
    col<-paste0(substr(col,1,1),substr(col,2,2),substr(col,2,2),substr(col,3,3),substr(col,3,3),substr(col,4,4),substr(col,4,4))
  col<-col2rgb(col)/255*gain+off
  col[col<0]<-0
  col[col>1]<-1
  rgb(col[1],col[2],col[3])
}

addTransparency <- function(col,alpha) {
  col<-col2rgb(col)/255
  rgb(col[1],col[2],col[3],alpha)
}

blend <- function(col1,col2,mix) {
  col1<-col2rgb(col1)/255
  col2<-col2rgb(col2)/255
  col<-col1*mix+col2*(1-mix)
  rgb(col[1],col[2],col[3])
}

complementary <- function(col1) {
  col1<-col2rgb(col1)/255
  col<-1-col1
  rgb(col[1],col[2],col[3])
}
