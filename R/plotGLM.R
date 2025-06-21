
#' plot a fitted GLM model 
#' @return sample object 
#' @examples
#' plotGLM<-function(DV,IVs,result,whichR)
#' @export
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
  
  xlim<-c(-1,1)*15
  ylim<-c(-1,1)*10
  braw.env$plotArea<-c(0,0,1,1)
  g<-startPlot(xlim=xlim,ylim=ylim,box="none",g=NULL)
  # g<-addG(g,dataPolygon(data.frame(x=c(-1,-1,1,1)*14,y=c(-1,1,1,-1)*9),col=braw.env$plotColours$graphBack,fill=braw.env$plotColours$graphBack))
  
  fill<-"#FFAAAA"
  g<-addG(g,dataLabel(data.frame(x=0,y=0),label=DV$name,hjust=0,vjust=0.5,fontface="bold",size=1,fill=fill))
  xStart<-4+nchar(DV$name)/2*(fontSize/14)
  arrowLength<-4-1
  yRange<-max(sum(r>0),sum(r<0))/2
  
  
    use<-order(r)
    y<-seq(0.65,100,2)
    if (y[length(use)]>3*2.5) y<-y-(3*2.5-y[length(use)])
    # y<-seq(1,-1,length.out=length(use))*yRange
    for (i in 1:length(use)) {
      r1<-r[use[i]]
      
      colLine<-"#000000"
      arrowWidth<-0.3
      if (r1<0) colArrow<-"#0088FF"
      else      colArrow="#FFEE00"
      colLabel<-colArrow

      if (abs(r1)<0.1) {
        colArrow<-desat(colArrow,0.1)
        colLine<-desat(colLine,0.1)
        arrowWidth<-0.1
      }
      if (abs(r1)>=0.1 && abs(r1)<0.3) {
        colArrow<-desat(colArrow,0.6)
        colLine<-desat(colLine,0.6)
        arrowWidth<-0.2
      }
      
      arrowLength<-sqrt((y[i])^2+(-xStart)^2)
      direction<- atan2((-y[i]),(xStart))*180/pi
      
      labelWidth<-arrowWidth*4
      arrowWidth<-arrowWidth*1.6
      # colArrow<-desat(colArrow,gain=abs(r[use[i]])^0.5)
      fill<-"#CCFF44"
      g<-addG(g,dataLabel(data.frame(x=-xStart,y=y[i]),label=IVs$name[use[i]],hjust=1,vjust=0.5,
                          col="#000000",fill=fill,size=1,label.size=labelWidth))
      g<-addG(g,drawArrow(start=c(-xStart,y[i]),arrowLength,direction=90+direction,ends="last",finAngle=60,
                          col=colLine,fill=colArrow,width=arrowWidth))
      g<-addG(g,dataLabel(data.frame(x=-xStart/2,y=y[i]/2),label=brawFormat(r1,digits=2),hjust=0.5,vjust=0.5,
                          col=colLine,fill=colArrow,size=0.75))
    }

    g<-addG(g,dataText(data.frame(x=xlim[1],y=ylim[1]),label=paste0("r[model]=",brawFormat(result$r.full,3)),vjust=0))
  
  return(g)

}