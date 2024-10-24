
svgBoxX<-function() {return(550)}
svgBoxY<-function() {return(400)}
svgX<-function(x) {return(x*svgBoxX())}
svgY<-function(y) {return((1-y)*svgBoxY())}

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
    gap0<-braw.env$plotLimits$gap[2]+braw.env$plotLimits$gap[4]
    gap1<-braw.env$plotLimits$gap[2]
  } else {
    gap0<-0
    gap1<-0
  }
  y<-(y*(1-gap0)+gap1)*braw.env$plotArea[4]+braw.env$plotArea[2]
  return(y)
}
rangeY<-function(y) {
  y<-y*braw.env$plotArea[4]+braw.env$plotArea[2]
  return(y)
}
reRangeX<-function(x) {
  if (!is.null(braw.env$plotLimits)){
    x<-(x-braw.env$plotLimits$xsc[1])/diff(braw.env$plotLimits$xsc)
    gap0<-braw.env$plotLimits$gap[1]+braw.env$plotLimits$gap[3]
    gap1<-braw.env$plotLimits$gap[1]
  } else {
    gap0<-0
    gap1<-0
  }
  x<-(x*(1-gap0)+gap1)*braw.env$plotArea[3]+braw.env$plotArea[1]
  return(x)
}
re2RangeX<-function(x) {
  if (!is.null(braw.env$plotLimits)){
    gap0<-braw.env$plotLimits$gap[1]+braw.env$plotLimits$gap[3]
    gap1<-braw.env$plotLimits$gap[1]
  } else {
    gap0<-0
    gap1<-0
  }
  # x<-(x*(1-gap0)+gap1)*braw.env$plotArea[3]+braw.env$plotArea[1]
  x<-((x-braw.env$plotArea[1])/braw.env$plotArea[3]-gap1)/(1-gap0)
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
  size*braw.env$labelSize*braw.env$plotLimits$fontScale
}

plotLimits<-function(xlim,ylim,orientation="horz",gaps=c(1,1,0,0),fontScale=1) {
    ez<-2
    fS<-1.2*fontScale*max(0.3,min(braw.env$plotArea[3:4]))^(1/ez)
    magnification<-max(0.3,min(braw.env$plotArea[3:4]))^(1/ez)/max(0.3,min(braw.env$plotArea[3:4]))
    gaps<-gaps*magnification
    
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
      # ' style=background-color:','#BFECFF','',
      ' xmlns="http://www.w3.org/2000/svg">'
    )
  } else {
    g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  }
  return(g)
}

startPlot<-function(xlim=c(0,1),ylim=c(0,1),gaps=NULL,box="Both",top=FALSE,tight=FALSE,
                    backC=braw.env$plotColours$graphBack,orientation="horz",fontScale=1,g=NULL) {
  minGap<-0.025
  topGap<-0.1
  bottomGap<-0.1
  leftGap<-0.12
  if (tight) maxGap<-0.075
  # gaps is left, bottom, right, top
  if (is.null(gaps)) {
    switch(box,
           "X"=gaps<-c(0,bottomGap),
           "x"=gaps<-c(minGap,bottomGap),
           "Y"=gaps<-c(leftGap,0),
           "y"=gaps<-c(leftGap,minGap),
           "both"=gaps<-c(minGap,minGap),
           "Both"=gaps<-c(leftGap,bottomGap),
           "none"=gaps<-c(0,0),
           {gaps<-c(minGap,minGap)}
    )
    if (top) gaps<-c(gaps,minGap,topGap)
    else gaps<-c(gaps,minGap,minGap)
  } else {
    gaps<-gaps*c(leftGap,bottomGap,leftGap,bottomGap)
  }
  ratio<-svgBoxX()/svgBoxY()*braw.env$plotArea[3]/braw.env$plotArea[4]
  if (ratio>1) gaps[c(1,3)]<-gaps[c(1,3)]/ratio
  if (ratio<1) gaps[c(2,4)]<-gaps[c(2,4)]*ratio
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
  return(g)  
}

plotTitle<-function(label,position="centre",size=1.25,fontface="bold") {
  ypos<-1-braw.env$plotLimits$gap[4]
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
  voff<-braw.env$plotLimits$gap[2]*0.05

  axis<-data.frame(x=reRangeX(mean(braw.env$plotLimits$xlim)),y=rangeY(voff))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(axis,label=label, hjust=0.5, vjust=-voff/1.5, colour="black",size=1.2,angle=90,fontface="bold")
         },
         "horz"={
           axisText(axis,label=label, hjust=0.5, colour="black",size=1.2,fontface="bold")
         }
  )
}
xAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE,angle=0){
  if (is.null(breaks)) {
    breaks<-axisTicks(usr=braw.env$plotLimits$xlim, log=logScale, axp = NULL, nint = 5)
  }
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  # labels<-as.character(labels)
  
  mn<-5/max(5,max(nchar(labels)))
  
  voff<-braw.env$plotLimits$gap[2]
  ticksBottom<-data.frame(x=reRangeX(breaks),y=rangeY(voff))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(ticksBottom,label=labels, hjust=1.1, vjust=0.5, colour="black",size=mn,fontface="plain")
           # geom_text(data=ticksTop,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
           #         size=mn)
         },
         "horz"={
           if (angle==0) {
             hjust=0.5
           } else {
             hjust=1.1
             # mn<-mn*0.75
           }
           axisText(ticksBottom,label=labels, hjust=hjust, vjust=1, dy=-2, colour="black",size=mn,fontface="plain")
           # geom_text(data=ticksTop,aes(x=x,y=y),label=labels,hjust=hjust,vjust=vjust,
           #           size=reSizeFont(mn),angle=angle)
         }
  )
}
yAxisLabel<-function(label){

  axis<-data.frame(x=rangeX(0.035),y=reRangeY(mean(braw.env$plotLimits$ylim)))
  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(axis,label=label, hjust=0.5, vjust=2/1.5, colour="black",size=1.2,fontface="bold")
         },
         "horz"={
           axisText(axis,label=label, hjust=0.5, colour="black",size=1.2,angle=90,fontface="bold")
         }
  )
}
yAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE){
  if (is.null(breaks)) {
      breaks<-axisTicks(usr=braw.env$plotLimits$ylim, log=logScale, axp = NULL, nint = 5)
  }
  
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  # labels<-as.character(labels)
  
  ticks<-data.frame(x=rangeX(braw.env$plotLimits$gap[1]),y=reRangeY(breaks))
  mn<-4/max(4,max(nchar(labels)))

  switch(braw.env$plotLimits$orientation,
         "vert"={
           axisText(ticks,label=labels, hjust=0.5, vjust=1.1, colour="black",size=mn,fontface="plain")
           # geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=0.5,vjust=1.1,
           #           size=reSizeFont(mn))
         },
         "horz"={
           axisText(ticks,label=labels, hjust=1.0, vjust=0.5, colour="black",size=mn,dx=-2,fontface="plain")
           # geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
           #           size=reSizeFont(mn))
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
dataLabel<-function(data,label, hjust=0, vjust=0, fill="white",colour="black",parser=TRUE,fontface="plain",size=1,label.size=0.25) {
  if (!braw.env$graphHTML) {
    mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  if (any(mathlabel)) {
    label<-deparse(label)
    parser<-TRUE
    voff<-1
  } else {
    if (parser) label<-deparse(bquote(.(label)))
    voff<-0
  }
  if (braw.env$plotLimits$orientation=="vert") {
    a<-hjust; hjust<-vjust; vjust<-a
  }
  data<-reRangeXY(data)
  g<-geom_label(data=data,aes(x = x, y = y), label=label, 
             hjust=hjust, vjust=vjust, nudge_y=voff,
             fill=fill,color=colour,fontface=fontface,
             label.padding=unit(0.1, "lines"),label.size=label.size,
             size=reSizeFont(size),parse=parser)
  } else {
    g<-dataText(data,label, hjust=hjust, vjust=vjust, colour=colour,fontface=fontface,size=size)
  }
  return(g)
}
dataText<-function(data,label, hjust=0, vjust=0, colour="black",size=1,angle=0,fontface="plain") {
  data<-reRangeXY(data)
  g<-axisText(data,label, hjust=hjust, vjust=vjust, colour=colour,size=size,angle=angle,fontface=fontface)
  return(g)  
}
axisText<-function(data,label, hjust=0, vjust=0, colour="black",size=1,angle=0,dx=0,dy=0,fontface="plain") {
  if (!braw.env$graphHTML) {
    parse<-FALSE
    mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
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
    if (vjust>0.5) valign<-' dominant-baseline="hanging"'
    
    if (fontface=="plain") fontface="normal"
    
    x<-svgX(data$x)
    y<-svgY(data$y)
    if (any(grepl('\\[([^ ]*)\\]',label))) y<-y-0.025*braw.env$plotArea[4]*svgBoxY()
    labels<-""
    for (i in 1:length(x)) {
      thisLabel<-label[i]
      thisLabel<-gsub('\\[([^ ]*?)\\]',
                      paste0('</tspan><tspan baseline-shift="sub" font-size="',
                             reSizeFont(size*3)*0.8,'">\\1</tspan><tspan>'),
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
                     '<text x="',x[i],'"',
                     ' y="',y[i],'"',
                     ' fill="',colour,'"',
                     ' text-anchor="middle"', valign,
                     ' transform="rotate(',-angle,',',x[1],',',y[1],')"',
                     ' font-size="',reSizeFont(size*3),'"',
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
  size<-size*braw.env$plotArea[4]
    if (!braw.env$graphHTML) {
    if (is.null(data$fill)) {
      g<-geom_point(data=data,aes(x=x,y=y),shape=shape,colour=colour,fill=fill,alpha=alpha,size=size)
    } else {
      g<-geom_point(data=data,aes(x=x,y=y,fill=fill),shape=shape,colour=colour,alpha=alpha,size=size)
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
  dx=0.02*fontsize
  names<-data$names
  nrows<-1+length(names)+1
  ncols<-max(c(nchar(title),nchar(names)))+2
  g<-list(axisPolygon(data=data.frame(x=c(1-ncols*dx,1,1,1-ncols*dx,1-ncols*dx),
                                      y=c(1-nrows*dy,1-nrows*dy,1,1,1-nrows*dy)),
                      fill="white",colour="white",linewidth=0.5)
  )
  g<-c(g,list(axisPath(data=data.frame(x=c(1-ncols*dx,1,1,1-ncols*dx,1-ncols*dx),
                                      y=c(1-nrows*dy,1-nrows*dy,1,1,1-nrows*dy)),
                      colour="black",linewidth=0.5))
  )
  g<-c(g,list(axisText(data=data.frame(x=(1-ncols*dx+2*dx),y=1-dy),label=title,size=fontsize,fontface="bold"))
       )

  for (i in 1:length(names)) {
    g<-c(g,
        list(axisPoint(data=data.frame(x=1-ncols*dx+dx,y=1-dy*(i+1)),fill=data$colours[i]))
    )
    g<-c(g,
         list(axisText(data=data.frame(x=1-ncols*dx+2*dx,y=1-dy*(i+1)),label=data$names[i],vjust=0.5,size=fontsize))
    )
  }
  return(g)
  
  mathlabel<-grepl("['^']{1}",title) | grepl("['[']{1}",title)
  if (any(mathlabel)) {
    title<-deparse(title)
  } else {
    title<-deparse(bquote(bold(.(title))))
  }
  
  fontsize<-reSizeFont(fontsize*0.8)
  names<-data$names
  width<-((max(c(nchar(title),nchar(names)))+2)*fontsize*2)*braw.env$graphicsSize[1]/5000
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
              aes(x=x,y=y,label=label),hjust=0,size=fontsize),
    geom_text(data=data.frame(x=ptsX[1],y=titleY,label=title),
              aes(x=x,y=y,label=label),hjust=0,size=fontsize,parse=TRUE)
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
