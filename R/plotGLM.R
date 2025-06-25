
#' plot a fitted GLM model 
#' @return sample object 
#' @examples
#' plotGLM<-function(DV,IVs,result,whichR)
#' @export
plotGLM<-function(lm,whichR="Unique") {
  DV<-lm$DV
  IVs<-lm$IVs
  result<-lm$result
  
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
  
  
  xlim<-c(-1,1)*15
  ylim<-c(-1,1)*10
  
  dy<-20/(length(r)+1)
  dx<-30/2
  fontSize<-min(c(0.85,dy/8,dx/12))

  braw.env$plotArea<-c(0,0,1,1)
  g<-startPlot(xlim=xlim,ylim=ylim,box="none",g=NULL)
  # g<-addG(g,dataPolygon(data.frame(x=c(-1,-1,1,1)*14,y=c(-1,1,1,-1)*9),col=braw.env$plotColours$graphBack,fill=braw.env$plotColours$graphBack))
  
  xEnd<- dx*(2-1)/2
  
  fill<-"#FFAAAA"
  # xEnd<- (4+nchar(DV$name)/2*(fontSize/14))
  xStart<- xEnd-dx
  yEnd<- 0
  g<-addG(g,dataLabel(data.frame(x=xEnd,y=yEnd),label=DV$name,hjust=0.5,vjust=0.5,fontface="bold",size=fontSize,fill=fill))
  
  y<-dy*(length(r)-1)/2
    # use<-rev(order(r))
    use<-1:length(r)

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
      
      
      labelWidth<-arrowWidth*4
      arrowWidth<-arrowWidth*1.6
      # colArrow<-desat(colArrow,gain=abs(r[use[i]])^0.5)
      fill<-"#CCFF44"
      g<-addG(g,dataLabel(data.frame(x=xStart,y=y),label=IVs$name[use[i]],hjust=0.5,vjust=0.5,
                          col="#000000",fill=fill,size=fontSize,label.size=labelWidth,fontface="bold",))

      xStartA<-xStart+(nchar(IVs$name[use[i]])+1)*fontSize*0.35
      xEndA<-xEnd-(nchar(DV$name)+1)*fontSize*0.35
      arrowLength<-sqrt((y-yEnd)^2+(xStartA-xEndA)^2)
      direction<- atan2((yEnd-y),(xEndA-xStartA))*180/pi
      
      g<-addG(g,drawArrow(start=c(xStartA,y),arrowLength,direction=90+direction,ends="last",finAngle=60,
                          col=colLine,fill=colArrow,width=arrowWidth))
      g<-addG(g,dataLabel(data.frame(x=xStart+(xEnd-xStart)/2,y=y+(yEnd-y)/2),label=brawFormat(r1,digits=2),hjust=0.5,vjust=0.5,
                          colour="#000000",fill=darken(colArrow,1,0.2),size=fontSize*0.85))
      y<-y-dy
    }

    label<-paste0("r[model]=",brawFormat(result$r.full,3),"  AIC=",brawFormat(result$AIC,1))
    g<-addG(g,dataLabel(data.frame(x=xlim[2]-diff(xlim)/30,y=ylim[1]+diff(ylim)/30),label,hjust=1,size=fontSize*0.85))

  return(g)

}
