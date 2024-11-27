svgBox<-function(height=NULL,aspect=1.3,fontScale=1.5) {
  if (is.null(height) && is.null(aspect)) return(braw.env$plotSize)
  if (is.null(height)) height<-braw.env$plotSize[2]
    setBrawEnv("plotSize",c(aspect,1)*height)
    setBrawEnv("labelSize",height/100*fontScale)
    setBrawEnv("dotSize",height/100*fontScale*1.25)
}
svgBoxX<-function() {return(braw.env$plotSize[1])}
svgBoxY<-function() {return(braw.env$plotSize[2])}
svgX<-function(x) {return(x*(svgBoxX()-20)+10)}
svgY<-function(y) {return((1-y)*(svgBoxY()-20)+10)}

addG<-function(g,...) {
  if (braw.env$graphHTML) {
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
  if (braw.env$graphHTML) return(size*braw.env$plotLimits$fontScale*3.5)
  else return(size*braw.env$plotLimits$fontScale*1.5)
}

plotLimits<-function(xlim,ylim,orientation="horz",gaps=c(1,1,0,0),fontScale=1) {
    ez<-2
    fS<-braw.env$labelSize*fontScale*max(0.3,min(braw.env$plotArea[3:4]))^(1/ez)
    gaps<-gaps*fS/120/c(braw.env$plotSize[1]/braw.env$plotSize[2],1,braw.env$plotSize[1]/braw.env$plotSize[2],1)

  switch(orientation,
         "horz"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=xlim,ysc=ylim,
                                           orientation=orientation,gap=gaps,fontScale=fS,
                                           xAxisTickSize=5,yAxisTickSize=5)},
         "vert"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=ylim,ysc=xlim,
                                           orientation=orientation,gap=gaps[c(2,1,4,3)],fontScale=fS,
                                           xAxisTickSize=5,yAxisTickSize=5)}
         )
}

nullPlot<-function() {
  if (braw.env$graphHTML) {
    g<-paste0(
      '<svg width=',format(svgBoxX()),' height=',format(svgBoxY()),
      ' margin:0; padding:0;',
      ' style=background-color:','"black"','',
      ' xmlns="http://www.w3.org/2000/svg">'
    )
  } else {
    g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  }
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
startPlot<-function(xlim=c(0,1),ylim=c(0,1),gaps=NULL,box="both",top=FALSE,
                    xticks=NULL,xlabel=NULL,xmax=FALSE,yticks=NULL,ylabel=NULL,ymax=FALSE,
                    backC=braw.env$plotColours$graphBack,orientation="horz",fontScale=1,
                    fullSize="full",g=NULL) {
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
  
  minGap<-0.1
  unitGap<-0.75
  labelGapx<-labelGapy<-unitGap*1.5
  if (containsSubscript(xlabel$label) || containsSuperscript(xlabel$label)) labelGapx<-labelGapx*1.85
  if (containsSubscript(ylabel$label) || containsSuperscript(ylabel$label)) labelGapy<-labelGapy*1.85
  
  if (!is.null(xticks) && !is.null(xticks$labels))
    maxtick<-max(nchar(xticks$labels))
  else maxtick<-0
  if (!is.null(yticks) && !is.null(yticks$labels))
    maxtick<-max(c(maxtick,nchar(yticks$labels)))
  tickSize<-5/max(7,maxtick)

  tickGap<-unitGap*tickSize
  
  bottomGap<-labelGapx+2*unitGap
  if (top) topGap<-unitGap*3.125 else topGap<-minGap
  leftGap<-labelGapy+4*tickGap
  rightGap<-minGap
  
  if (!is.null(xticks)) {
    if (is.null(xticks$breaks))
      xticks$breaks<-axisTicks(usr=xlim, log=xticks$logScale, axp = NULL, nint = 5)
    if (is.null(xticks$labels))
      xticks$labels<-as.character(xticks$breaks)
    if (!is.character(xticks$labels)) xticks$labels<-brawFormat(xticks$labels,digits=-2)
  } else {
    bottomGap<-minGap
  }
  if (!is.null(yticks)) {
    if (is.null(yticks$breaks))
      yticks$breaks<-axisTicks(usr=ylim, log=yticks$logScale, axp = NULL, nint = 5)
    if (is.null(yticks$labels))
      yticks$labels<-as.character(yticks$breaks)
    if (!xmax)
      leftGap<-labelGapy+max(nchar(yticks$labels))*tickGap
    if (!is.character(yticks$labels)) yticks$labels<-brawFormat(yticks$labels,digits=-2)
  } else {
    leftGap<-minGap
  }

  gaps<-c(leftGap,bottomGap,rightGap,topGap)
  plotLimits(xlim = xlim, ylim = ylim,orientation=orientation,gaps,fontScale=fontScale)
  
  if (is.null(g)) g<-nullPlot()
    # yside<-data.frame(x=c(0,braw.env$plotLimits$gap[1],braw.env$plotLimits$gap[1],0),y=c(braw.env$plotLimits$gap[2],braw.env$plotLimits$gap[2],1-braw.env$plotLimits$gap[4],1-braw.env$plotLimits$gap[4]))
    # g<-addG(g,axisPolygon(data=rangeXY(yside), fill="red"))
    # xside<-data.frame(x=c(braw.env$plotLimits$gap[1],1-braw.env$plotLimits$gap[3],1-braw.env$plotLimits$gap[3],gaps[1]),y=c(0,0,braw.env$plotLimits$gap[2],braw.env$plotLimits$gap[2]))
    # g<-addG(g,axisPolygon(data=rangeXY(xside), fill="green"))
    # topside<-data.frame(x=c(braw.env$plotLimits$gap[1],1-braw.env$plotLimits$gap[3],1-braw.env$plotLimits$gap[3],braw.env$plotLimits$gap[1]),y=c(1,1,1-braw.env$plotLimits$gap[4],1-braw.env$plotLimits$gap[4]))
    # g<-addG(g,axisPolygon(data=rangeXY(topside), fill="yellow"))
    # farside<-data.frame(x=c(1,1-braw.env$plotLimits$gap[3],1-braw.env$plotLimits$gap[3],1),y=c(braw.env$plotLimits$gap[2],braw.env$plotLimits$gap[2],1-braw.env$plotLimits$gap[4],1-braw.env$plotLimits$gap[4]))
    # g<-addG(g,axisPolygon(data=rangeXY(farside), fill="yellow"))
    
  
  if (!(is.character(backC) && backC=="transparent")) {
    back<-data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)])
    # g<-addG(g,axisPath(data=data.frame(x=c(0,1,1,0,0),y=c(0,0,1,1,0)),colour="black"))
    g<-addG(g,dataPolygon(data=back, fill=backC, colour=backC))
  }

  xaxis<-data.frame(x=xlim,y=ylim[1])
  yaxis<-data.frame(x=xlim[1],y=ylim)
  
  switch(box,
         "x"={
           g<-addG(g,dataLine(data=xaxis,colour="black",linewidth=0.25))
         },
         "y"={
           g<-addG(g,dataLine(data=yaxis,colour="black",linewidth=0.25))
         },
         "both"={
           g<-addG(g,dataLine(data=xaxis,colour="black",linewidth=0.25))
           g<-addG(g,dataLine(data=yaxis,colour="black",linewidth=0.25))
         }
         )
  if (!is.null(xticks)) 
    g<-addG(g,xAxisTicks(breaks=xticks$breaks,labels=xticks$labels,logScale=xticks$logScale,angle=xticks$angle,size=tickSize))
  if (!is.null(xlabel))
    g<-addG(g,xAxisLabel(label=xlabel$label))
  if (!is.null(yticks)) 
    g<-addG(g,yAxisTicks(breaks=yticks$breaks,labels=yticks$labels,logScale=yticks$logScale,angle=yticks$angle,size=tickSize))
  if (!is.null(ylabel))
    g<-addG(g,yAxisLabel(label=ylabel$label))
  return(g)  
}

plotTitle<-function(label,position="centre",size=1.25,fontface="bold") {
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

xAxisLabel<-function(label) {
  voff<-0+braw.env$plotLimits$gap[2]*0.25

  axis<-data.frame(x=reRangeX(mean(braw.env$plotLimits$xlim)),y=rangeY(voff))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(axis,label=label, hjust=0.5, vjust=-voff/1.5, colour="black",size=0.75,angle=90,fontface="bold")
         },
         "horz"={
           axisText(axis,label=label, hjust=0.5, vjust=0, colour="black",size=0.75,fontface="bold")
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
  
  if (is.null(size)) size<-7/max(7,max(nchar(labels)))
  
  voff<-braw.env$plotLimits$ylim[1]
  ticksBottom<-data.frame(x=reRangeX(breaks),y=reRangeY(voff))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(ticksBottom,label=labels, hjust=1.1, vjust=0.5, colour="black",size=size,fontface="plain")
         },
         "horz"={
           if (angle==0) {
             hjust=0.5
           } else {
             hjust=1.1
           }
           axisText(ticksBottom,label=labels, hjust=hjust, vjust=1, dy=-2, colour="black",size=size,fontface="plain")
         }
  )
}
yAxisLabel<-function(label){

  axis<-data.frame(x=rangeX(0.0),y=reRangeY(mean(braw.env$plotLimits$ylim)))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(axis,label=label, hjust=0.5, vjust=2/1.5, colour="black",size=0.75,fontface="bold")
         },
         "horz"={
           axisText(axis,label=label, hjust=0.5, vjust=1, colour="black",size=0.75,angle=90,fontface="bold")
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
  
  ticks<-data.frame(x=reRangeX(braw.env$plotLimits$xlim[1]),y=reRangeY(breaks))
  if (is.null(size)) size<-4/max(4,max(nchar(labels)))

  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(ticks,label=labels, hjust=0.5, vjust=1.1, colour="black",size=size,fontface="plain")
         },
         "horz"={
           if (angle==0) {
             hjust=1.1
           } else {
             hjust=1.1
           }
           axisText(ticks,label=labels, hjust=hjust, vjust=0.5, colour="black",size=size,dx=-2,fontface="plain")
         }
  )
}


vertLine<-function(intercept=NULL,linetype="solid",colour="black",alpha=1,linewidth=0.25){
  data<-data.frame(x=intercept,y=braw.env$plotLimits$ylim)
  return(dataPath(data=data,arrow=NULL,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha))
}
horzLine<-function(intercept=NULL,linetype="solid",colour="black",alpha=1,linewidth=0.25){
  data<-data.frame(x=braw.env$plotLimits$xlim,y=intercept)
  return(dataPath(data=data,arrow=NULL,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha))
}
dataLine<-function(data,arrow=NULL,colour="black",linetype="solid",linewidth=0.25,alpha=1) {
  return(dataPath(data,arrow=arrow,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha))
}
dataBar<-function(data,colour="black",fill="white",alpha=1,barwidth=0.85) {
  bar<-data.frame(x=c(-1,1,1,-1)*barwidth/length(data$x),
                  y=c(0,0,1,1)
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
# dataLabel
# dataText
# dataPath
# dataPoint
# dataPolygon
# dataErrorbar
# dataLegend
# dataContour
dataLabel<-function(data,label, hjust=0, vjust=0, fill="white",colour="black",size=1,angle=0,fontface="plain",background=FALSE,parser=TRUE,label.size=0.25) {
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
axisLabel<-function(data,label, hjust=0, vjust=0, angle=0, fill="white",colour="black",parser=TRUE,fontface="plain",size=1,label.size=0.25) {
  if (!braw.env$graphHTML) {
    mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  if (any(mathlabel)) {
    label<-gsub("=","==",label)
    parser<-TRUE
    voff<-0
  } else {
    if (parser) label<-deparse(bquote(.(label)))
    voff<-0
  }
  if (braw.env$plotLimits$orientation=="vert") {
    a<-hjust; hjust<-vjust; vjust<-a
  }
  g<-geom_label(data=data,aes(x = x, y = y), label=label, angle=angle,
             hjust=hjust, vjust=vjust, nudge_y=voff,
             fill=fill,color=colour,fontface=fontface,
             label.padding=unit(0.1, "lines"),label.size=label.size,
             size=reSizeFont(size)*0.8,parse=parser)
  } else {
    g<-axisText(data,label, hjust=hjust, vjust=vjust, angle=angle, colour=colour,fontface=fontface,size=size,background=TRUE,fill=fill)
  }
  return(g)
}
dataText<-function(data,label, hjust=0, vjust=0, colour="black",size=1,angle=0,fontface="plain",background=FALSE,fill="white") {
  data<-reRangeXY(data)
  g<-axisText(data,label, hjust=hjust, vjust=vjust, colour=colour,size=size,angle=angle,fontface=fontface,background=background,fill=fill)
  return(g)  
}
axisText<-function(data,label, hjust=0, vjust=0, colour="black",size=1,angle=0,dx=0,dy=0,fontface="plain",background=FALSE,fill="white") {
  if (!braw.env$graphHTML) {
    parse<-FALSE
    mathlabel<-grepl("['[']{1}",label) #| grepl("[\\^]{1}",label)
    if (any(mathlabel)) {
      label<-gsub("=","==",label)
      parse=TRUE
      if (fontface=="bold") label<-paste0('bold(',label,')')
    }
    if (braw.env$plotLimits$orientation=="vert") {
      a<-hjust; hjust<-vjust; vjust<-a
    }
    g<-geom_text(data=data,aes(x = x, y = y), label=label, hjust=hjust, vjust=vjust, 
              color=colour,fontface=fontface,angle=angle,
              size=reSizeFont(size),parse=parse)
  } else {
    halign<-' text-anchor="start"' 
    if (hjust==0.5) halign<-' text-anchor="middle"' 
    if (hjust>0.5) halign<-' text-anchor="end"' 
    
    if (fontface=="plain") fontface="normal"
    valign<-' dominant-baseline="auto"' 
    if (vjust==0.5) valign<-' dominant-baseline="middle"'
    if (vjust>0.5) valign<-' dominant-baseline="text-before-edge"'
    
    if (fontface=="plain") fontface="normal"
    
    x<-svgX(data$x)
    y<-svgY(data$y)
    if (containsSubscript(label)) y<-y-0.025*braw.env$plotArea[4]*svgBoxY()
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
    
    for (i in 1:length(x)) {
      thisLabel<-label[i]
      thisLabel<-gsub('\\[([^ ]*?)\\]',
                      paste0('</tspan><tspan baseline-shift="sub" font-size="',
                             reSizeFont(size)*0.8,'">\\1</tspan><tspan>'),
                      thisLabel)
      thisLabel<-gsub('\\^([^ ]*?) ',
                      paste0('</tspan><tspan baseline-shift="super" font-size="',
                             reSizeFont(size)*0.8,'">\\1</tspan><tspan>'),
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
  }
  return(g)
}

dataPath<-function(data,arrow=NULL,colour="black",linetype="solid",linewidth=0.25,alpha=1) {
  data<-reRangeXY(data)
  axisPath(data,arrow=arrow,colour=colour,linetype=linetype,linewidth=linewidth,alpha=alpha)
  }
axisPath<-function(data,arrow=NULL,colour="black",linetype="solid",linewidth=0.25,alpha=1) {
  if (!braw.env$graphHTML) {
    g<-geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,alpha=alpha,
                 linetype=linetype,linewidth=linewidth)
  } else {
    ls<-''
    if (linetype=="dotted") ls<-' stroke-dasharray="2,2"'
    linestyle<-paste0(' fill="none" stroke="',colour,'"',
                      ls,
                      ' stroke-width="',linewidth,'"',
                      ' stroke-opacity="',alpha,'"')
    x<-svgX(data$x)
    y<-svgY(data$y)
    points<-' points="'
    for (i in 1:length(x)) 
      points<-paste0(points,' ',format(x[i]),',',format(y[i]))
    points<-paste0(points,'"')
    
    g<-paste0(
      '<polyline',
      points,
      linestyle,
      ' />'
    )
  }
  return(g)
}
dataPoint<-function(data,shape=21,colour="black",fill="white",alpha=1,size=3) {
  data<-reRangeXY(data)
  axisPoint(data=data,shape=shape,colour=colour,fill=fill,alpha=alpha,size=size)
}
axisPoint<-function(data,shape=21,colour="black",fill="white",alpha=1,size=3) {
  size<-0.75*size*(braw.env$plotArea[4])^0.5
    if (!braw.env$graphHTML) {
      size<-size*1.5
    if (is.null(data$fill)) {
      g<-geom_point(data=data,aes(x=x,y=y),shape=shape,colour=colour,fill=fill,alpha=alpha,size=size*0.9)
    } else {
      g<-geom_point(data=data,aes(x=x,y=y,fill=fill),shape=shape,colour=colour,alpha=alpha,size=size*0.9)
    }
  } else {
    x<-svgX(data$x)
    y<-svgY(data$y)
    if (length(x)==0) return("")
    if (shape==21) {
      sz<-size*4/pi
      g<-""
      for (i in 1:length(x)) {
        g<-paste0(g,
                  '<circle cx="',format(x[i]),'" cy="',format(y[i]),'" r="',sz,'"',
                  ' fill="',fill,'"',
                  ' stroke="',colour,'" stroke-width="1"',
                  ' />'
        )
      }
    } else {
      sz<-size*3
      if (shape==22) tr="" 
      else           tr=paste0(' transform=rotate(45,',format(x[i]),',',format(y[i]),')')
      g<-""
      for (i in 1:length(x)) {
        g<-paste0(g,
                  '<rect x="',format(x[i]-sz/2),'" y="',format(y[i]-sz/2),'"',
                  ' width="',sz,'"',' height="',sz,'"',
                  ' rx="0" ry="0"',
                  ' fill="',fill,'"',
                  ' stroke="',colour,'" stroke-width="1"',
                  tr,
                  ' />'
        )
      }
    }
  }
  return(g)
}
dataPolygon<-function(data,colour="black",fill="white",alpha=1,linewidth=0.25) {
  data<-reRangeXY(data)
  g<-axisPolygon(data,colour=colour,fill=fill,alpha=alpha,linewidth=linewidth)
  return(g)
}
axisPolygon<-function(data,colour="black",fill="white",alpha=1,linewidth=0.25) {
  if (!braw.env$graphHTML) {
    if (!is.na(colour) && colour=="none") colour=NA
    if (!is.null(data$ids)) {
    g<-geom_polygon(data=data,aes(x=x,y=y,group=ids,alpha=alpha*value),colour = colour, fill = fill,linewidth=linewidth)
  } else {
    if (!is.null(data$fill)) {
      g<-geom_polygon(data=data,aes(x=x,y=y, fill = fill),colour = colour,alpha=alpha,linewidth=linewidth)
    } else {
      g<-geom_polygon(data=data,aes(x=x,y=y),colour = colour, fill = fill,alpha=alpha,linewidth=linewidth)
    }
  }
  } else {
    x<-svgX(data$x)
    y<-svgY(data$y)
    if (!is.null(data$ids)) {
      g<-""
      for (i in seq(1,length(x),4)) {
        linestyle<-paste0(' fill="',fill,'" stroke="',colour,'"',
                          ' fill-opacity="',alpha*data$value[i],'"',
                          ' stroke="',colour,'"',
                          ' stroke-width="',linewidth*2,'"',
                          ' stroke-opacity="',1,'"')
        points<-' points="'
        for (j in 1:4)  points<-paste0(points,' ',format(x[i+j-1]),',',format(y[i+j-1]))
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
                        ' stroke-opacity="',1,'"')
      points<-' points="'
      for (i in 1:length(x)) points<-paste0(points,' ',format(x[i]),',',format(y[i]))
      # points<-paste0(points,' ',format(x[1]),',',format(y[1]))
      points<-paste0(points,'"')
      
      g<-paste0(
        '<polyline',
        points,
        linestyle,
        ' />'
      )
    }
  }
  return(g)
}
dataErrorBar<-function(data,colour="black",linewidth=0.25) {
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
    thisError<-data.frame(x=rep(data$x[i],2),y=c(data$ymin[i]-1,data$ymax[i]+1))
    g<-c(g,dataPath(thisError,colour=colour,linewidth=linewidth))
  }
  return(g)
}
dataLegend<-function(data,title="title",fontsize=0.65) {
  dy=0.05*fontsize
  dx=0.025*fontsize
  names<-data$names
  if (nchar(title)>0) tn<-1 else tn<-0
  nrows<-tn+length(names)+1
  ncols<-max(c(nchar(title),nchar(names)))+2
  g<-list(axisPolygon(data=data.frame(x=rangeX(c(1-ncols*dx,1,1,1-ncols*dx,1-ncols*dx)),
                                      y=rangeY(c(1-nrows*dy,1-nrows*dy,1,1,1-nrows*dy))),
                      fill="white",colour="white",linewidth=0.5)
  )
  g<-c(g,list(axisPath(data=data.frame(x=rangeX(c(1-ncols*dx,1,1,1-ncols*dx,1-ncols*dx)),
                                      y=rangeY(c(1-nrows*dy,1-nrows*dy,1,1,1-nrows*dy))),
                      colour="black",linewidth=0.5))
  )
  if (tn>0)
  g<-c(g,list(axisText(data=data.frame(x=rangeX(1-ncols*dx+2*dx),y=rangeY(1-dy)),label=title,size=fontsize,fontface="bold"))
       )

  for (i in 1:length(names)) {
    g<-c(g,
        list(axisPoint(data=data.frame(x=rangeX(1-ncols*dx+dx),y=rangeY(1-dy*(i+tn))),fill=data$colours[i]))
    )
    g<-c(g,
         list(axisText(data=data.frame(x=rangeX(1-ncols*dx+2*dx),y=rangeY(1-dy*(i+tn))),label=data$names[i],vjust=0.5,size=fontsize))
    )
  }
  return(g)
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
