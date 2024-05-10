plotGLM<-function(DV,IVs,result,whichR) {
  
  switch(whichR,
         "Direct"={
           r<-result$r.direct
           p<-result$p.direct
         },
         "Unique"={
           r<-result$r.unique
           p<-result$p.unique
         },
         "Total"={
           r<-result$r.total
           p<-result$p.total
         }
  )
  
  fontSize<-braw.env$labelSize*1.25
  if (length(r)>10) fontSize<-braw.env$labelSize
  
  braw.env$plotArea<-c(0,0,1,1)
  g<-ggplot()+braw.env$plotRect+braw.env$blankTheme()
  g<-startPlot(xlim=c(-1,1)*15,ylim=c(-1,1)*10,back="transparent",box="none",g=g)
  g<-g+dataPolygon(data.frame(x=c(-1,-1,1,1)*14,y=c(-1,1,1,-1)*9),col=braw.env$plotColours$graphBack,fill=braw.env$plotColours$graphBack)
  
  g<-g+dataLabel(data.frame(x=0,y=0),label=bquote(bold(.(DV$name))),hjust=0.5,vjust=0.5,fontface="bold",size=1.25)
  xStart<-4+nchar(DV$name)/2*(fontSize/14)
  arrowLength<-4-1
  yRange<-max(sum(r>0),sum(r<0))/2
  
  if (any(r>0)) {
    use<-which(r>0)
    useOrder<-order(r[use])
    use<-use[useOrder]
    y<-seq(0.65,100,1.3)
    if (y[length(use)]>3*2.5) y<-y-(3*2.5-y[length(use)])
    # y<-seq(1,-1,length.out=length(use))*yRange
    for (i in 1:length(use)) {
      col<-"#DD0000"
      colArrow<-"#FF0000"
      if (p[use[i]]<braw.env$alphaSig) {
        fill<-"white"
        width<-0.2
      } else {
        fill<-braw.env$plotColours$graphBack
        width<-0.1
      }
      colArrow<-desat(colArrow,gain=abs(r[use[i]])^0.5)
      g<-g+dataLabel(data.frame(x=-xStart,y=y[i]),label=bquote(bold(.(IVs$name[use[i]]))),hjust=1,vjust=0.5,col=col,fill=fill,size=1.25,label.size=width*4)
      direction<-atan2(y[i]*0.9,arrowLength+0.25)
      g<-g+drawArrow(start=c(-xStart,y[i]),arrowLength,direction=90-direction*180/pi,width=width,ends="last",col=colArrow,fill=colArrow)
    }
  }
  
  if (any(r<0)) {
    use<-which(r<0)
    useOrder<-order(-r[use])
    use<-use[useOrder]
    y<-seq(-0.65,-100,-1.3)
    if (y[length(use)]< -3*2.5) y<-y+(-3*2.5-y[length(use)])
    # y<-seq(1,-1,length.out=length(use))*yRange
    for (i in 1:length(use)) {
        col<-"#0000DD"
        colArrow<-"#0000FF"
        if (p[use[i]]<braw.env$alphaSig) {
          fill<-"white"
          width<-0.2
        } else {
          fill<-braw.env$plotColours$graphBack
          width<-0.1
        }
      colArrow<-desat(colArrow,gain=abs(r[use[i]])^0.5)
      g<-g+dataLabel(data.frame(x=xStart,y=y[i]),label=bquote(bold(.(IVs$name[use[i]]))),hjust=0,vjust=0.5,col=col,fill=fill,size=1.25,label.size=width*5)
      direction<-atan2(y[i]*0.9,arrowLength+0.25)
      g<-g+drawArrow(start=c(xStart,y[i]),arrowLength,direction=-90+direction*180/pi,width=width,ends="last",col=colArrow,fill=colArrow)
    }
  }
  g<-g+dataText(data.frame(x=0,y=-10),label=bquote(bold(r[model] ~ "=" ~ .(brawFormat(result$r.full,3)))),hjust=0.5)
  
  return(g)

}