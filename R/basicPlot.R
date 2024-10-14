
svgBoxX<-function() {return(800)}
svgBoxY<-function() {return(600)}
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
reSizeFont<-function(size) {
  size*braw.env$labelSize*braw.env$plotLimits$fontScale
}

plotLimits<-function(xlim,ylim,orientation="horz",gaps=c(1,1,0,0),fontScale=1) {
  if (is.null(fontScale)) fontScale<-min(braw.env$plotArea[3:4])^0.5
    
  switch(orientation,
         "horz"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=xlim,ysc=ylim,
                                           orientation=orientation,gap=gaps,fontScale=fontScale,
                                           xAxisTickSize=5,yAxisTickSize=5)},
         "vert"={braw.env$plotLimits<-list(xlim=xlim,ylim=ylim,xsc=ylim,ysc=xlim,
                                           orientation=orientation,gap=gaps[c(2,1,4,3)],fontScale=fontScale,
                                           xAxisTickSize=5,yAxisTickSize=5)}
         )
}

nullPlot<-function() {
  if (braw.env$graphHTML) {
    g<-paste0(
      '<svg width=',format(svgBoxX()),' height=',format(svgBoxY()),' xmlns="http://www.w3.org/2000/svg">'
    )
  } else {
    g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  }
  return(g)
}

startPlot<-function(xlim=c(0,1),ylim=c(0,1),box="both",top=FALSE,tight=FALSE,backC=braw.env$plotColours$graphBack,orientation="horz",fontScale=NULL,g=NULL) {
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

  if (is.null(g)) g<-nullPlot()
  back<-data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)])
  xaxis<-data.frame(x=xlim,y=ylim[1])
  yaxis<-data.frame(x=xlim[1],y=ylim)
  
  if (!(is.character(backC) && backC=="transparent"))
    g<-addG(g,dataPolygon(data=back, fill=backC, colour=backC))
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
  switch(position,
         "left"={
           dataText(data.frame(x=braw.env$plotLimits$xlim[1],y=braw.env$plotLimits$ylim[2]),label,
                    hjust=0,vjust=-1,size=size,fontface=fontface)
         },
         "centre"={
           dataText(data.frame(x=mean(braw.env$plotLimits$xlim),y=braw.env$plotLimits$ylim[2]),label,
                    hjust=0.5,vjust=-1,size=size,fontface=fontface)
         },
         "right"={
           dataText(data.frame(x=braw.env$plotLimits$xlim[2],y=braw.env$plotLimits$ylim[2]),label,
                     hjust=1,vjust=-0.1,size=size,fontface=fontface)
         },
  )
  
}

xAxisLabel<-function(label) {
  # mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  # if (any(mathlabel)) {
  #   label<-deparse(label)
  # } else {
  #   label<-deparse(bquote(bold(.(label))))
  # }
  voff<-braw.env$plotLimits$xAxisTickSize

  axis<-data.frame(x=mean(braw.env$plotLimits$xlim),y=braw.env$plotLimits$ylim[1])
  # axis<-reRangeXY(axis)
  switch(braw.env$plotLimits$orientation,
         "vert"={
           dataText(axis,label=label, hjust=0.5, vjust=-voff/1.5, colour="black",size=1.25,angle=90,fontface="bold")
           # geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
           #           hjust=0.5,vjust=-voff/1.5,
           #           size=reSizeFont(1.25),angle=90,fontface="bold")      
         },
         "horz"={
           dataText(axis,label=label, hjust=0.5, vjust=4/1.5, colour="black",size=1.25,fontface="bold")
           # geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
           #           hjust=0.5,vjust=4/1.5,
           #           size=reSizeFont(1.25),angle=0,fontface="bold")      
         }
  )
}
yAxisLabel<-function(label){
  # mathlabel<-grepl("['^']{1}",label) | grepl("['[']{1}",label)
  # if (any(mathlabel)) {
  #   label<-deparse(label)
  # } else {
  #   label<-deparse(bquote(bold(.(label))))
  # }
  voff<-braw.env$plotLimits$yAxisTickSize-0.25
  
  axis<-data.frame(x=braw.env$plotLimits$xlim[1],y=mean(braw.env$plotLimits$ylim))
  # axis<-reRangeXY(axis)
  switch(braw.env$plotLimits$orientation,
         "vert"={
           dataText(axis,label=label, hjust=0.5, vjust=2/1.5, colour="black",size=1.25,fontface="bold")
           # geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
           #           hjust=0.5,vjust=2/1.5,
           #           size=reSizeFont(1.25),angle=0,fontface="bold")      
         },
         "horz"={
           dataText(axis,label=label, hjust=0.5, vjust=-voff, colour="black",size=1.25,angle=90,fontface="bold")
           # geom_text(data=axis,aes(x=x,y=y),label=label, parse = TRUE,
           #           hjust=0.5,vjust=-voff,
           #           size=reSizeFont(1.25),angle=90,fontface="bold")      
         }
  )
}
yAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE){
  if (is.null(breaks)) {
      breaks<-axisTicks(usr=braw.env$plotLimits$ylim, log=logScale, axp = NULL, nint = 7)
  }
  
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  # labels<-as.character(labels)
  
  ticks<-data.frame(x=braw.env$plotLimits$xlim[1],y=breaks)
  mn<-5/max(5,max(nchar(labels)))
  braw.env$plotLimits$yAxisTickSize<-min(3,median(nchar(labels)))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           dataText(ticks,label=labels, hjust=0.5, vjust=1.1, colour="black",size=mn,fontface="plain")
           # geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=0.5,vjust=1.1,
           #           size=reSizeFont(mn))
         },
         "horz"={
           dataText(ticks,label=labels, hjust=1.1, vjust=0.5, colour="black",size=mn,fontface="plain")
           # geom_text(data=ticks,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
           #           size=reSizeFont(mn))
         }
  )
}
xAxisTicks<-function(breaks=NULL,labels=NULL,logScale=FALSE,angle=0){
  if (is.null(breaks)) {
      breaks<-axisTicks(usr=braw.env$plotLimits$xlim, log=logScale, axp = NULL, nint = 7)
  }
  if (is.null(labels)) labels<-breaks
  if (logScale) breaks<-log10(breaks)
  # labels<-as.character(labels)
  
  yoff<-diff(braw.env$plotLimits$ylim)/20
  ticksTop<-data.frame(x=breaks,y=braw.env$plotLimits$ylim[1])
  ticksBottom<-data.frame(x=breaks,y=braw.env$plotLimits$ylim[1]-yoff)
  
  sz<-100/length(breaks)
  mn<-sz*0.9/max(sz,max(nchar(labels)))
  braw.env$plotLimits$xAxisTickSize<-min(5,max(nchar(labels)))
  
  switch(braw.env$plotLimits$orientation,
         "vert"={
           dataText(ticksTop,label=labels, hjust=1.1, vjust=0.5, colour="black",size=mn,fontface="plain")
             # geom_text(data=ticksTop,aes(x=x,y=y),label=labels,hjust=1.1,vjust=0.5,
             #         size=mn)
         },
         "horz"={
           if (angle==0) {
             hjust=0.5
           vjust=1.1
           } else {
             hjust=1.1
             vjust=0.5
             mn<-mn*0.75
           }
             dataText(ticksTop,label=labels, hjust=hjust, vjust=vjust, colour="black",size=mn,fontface="plain")
           # geom_text(data=ticksTop,aes(x=x,y=y),label=labels,hjust=hjust,vjust=vjust,
           #           size=reSizeFont(mn),angle=angle)
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
    g<-""
  }
  return(g)
}
dataText<-function(data,label, hjust=0, vjust=0, colour="black",size=1,angle=0,fontface="plain") {
  data<-reRangeXY(data)
  if (!braw.env$graphHTML) {
    parse<-FALSE
    nt<-nrow(data)
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
   
    x<-svgX(data$x)
    y<-svgY(data$y)
    labels<-""
    for (i in 1:length(x)) {
      thisLabel<-label[i]
      thisLabel<-gsub('\\[([^ ]*)\\]',
                      paste0('</tspan><tspan baseline-shift="sub" font-size="',
                             reSizeFont(size*5)*0.8,'">\\1</tspan><tspan>'),
                      thisLabel)
      thisLabel<-paste0(
                     '<tspan',halign,' dy="',vjust/size,'em"',
                     '>',
                     thisLabel,
                     '</tspan>'
      )
      labels<-paste0(labels,
                     '<text x="',x[i],'"',
                     ' y="',y[i],'"',
                     ' fill="',colour,'"',
                     ' text-anchor="middle" dominant-baseline="central"',
                     ' transform="rotate(',-angle,',',x[1],',',y[1],')"',
                     ' font-size="',reSizeFont(size*5),'"',
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
  if (!braw.env$graphHTML) {
    g<-geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,alpha=alpha,
                 linetype=linetype,linewidth=linewidth)
  } else {
    linestyle<-paste0(' fill="none" stroke="',colour,
                      '" stroke-width="',linewidth,'"',
                      '" stroke-opacity="',alpha,'"')
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
    sz<-size*2
    if (shape==21) {
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
  if (!braw.env$graphHTML) {
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
                          ' stroke-width="',linewidth,'"',
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
                        ' stroke-width="',linewidth,'"',
                        ' stroke-opacity="',1,'"')
      points<-' points="'
      for (i in 1:length(x)) points<-paste0(points,' ',format(x[i]),',',format(y[i]))
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
dataLegend<-function(data,title="title",fontsize=1.2) {
  mathlabel<-grepl("['^']{1}",title) | grepl("['[']{1}",title)
  if (any(mathlabel)) {
    title<-deparse(title)
  } else {
    title<-deparse(bquote(bold(.(title))))
  }
  
  fontsize<-reSizeFont(fontsize*0.8)
  names<-data$names
  width<-((max(c(nchar(title),nchar(names)))+2)*fontsize)*braw.env$graphicsSize[1]/5000
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
