plotPathModel<-function(pathmodel) {
  
  digits<-2
  r<-pathmodel$ES_table
  
  switch(pathmodel$depth,
         '1'= depth<-1,
         '2'= depth<-2,
         '3'= depth<-length(stages)
  )

  xlim<-c(-1,1)*15
  ylim<-c(-1,1)*10
  braw.env$plotArea<-c(0,0,1,1)
  g<-startPlot(xlim=xlim,ylim=ylim,box="none",g=NULL)

  dy<-20/max(sapply(pathmodel$stages,length)+1)
  dx<-30/(length(pathmodel$stages))
  fontSize<-min(dy/8,dx/12)
  
  names<-c()
  xs<-c()
  ys<-c()
  ar<-c()
  ax<-c()
  ay<-c()
  aLen<-c()
  aWid<-c()
  rlabels<-c()
  rx<-c()
  ry<-c()
  rcol<-c()
  rfg<-c()
  rdir<-c()
  
  # get the locations sorted to start with
  x<- -dx*(length(pathmodel$stages)-1)/2
  offset<-TRUE
  for (st in 1:length(pathmodel$stages))  {
    thisStage<-pathmodel$stages[[st]]
    offset<-!offset
    y<-dy*(length(thisStage)-1)/2
    if ((rem(length(thisStage)-1,2)==1) == offset)  y<-y+dy/2
    for (vr in 1:length(thisStage)) {
      names<-c(names,thisStage[vr])
      xs<-c(xs,x)
      ys<-c(ys,y)
      y<-y-dy
    }
    x<-x+dx
  }
  
  # draw arrows first
  rnames<-rownames(pathmodel$ES_table)
  cnames<-colnames(pathmodel$ES_table)
  for (i1 in 1:ncol(pathmodel$ES_table))
    for (i2 in 1:nrow(pathmodel$ES_table))
      if (!is.na(r[i2,i1])) {
        use1<-which(names==cnames[i1])
        use2<-which(names==rnames[i2])
        yStart<-ys[use1]
        xStart<-xs[use1]+(nchar(cnames[i1])+1)*fontSize*0.25
        yEnd<-ys[use2]
        xEnd<-xs[use2]-(nchar(rnames[i2])+2)*fontSize*0.25
        arrowLength<-sqrt((yStart-yEnd)^2+(xStart-xEnd)^2)
        direction<- atan2((yEnd-yStart),(xEnd-xStart))*180/pi

        r1<-r[i2,i1]
          colLine<-"#000000"
          arrowWidth<-0.3
          if (r1<0) colArrow<-"#0088FF"
          else      colArrow="#FFEE00"
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
          arrowWidth<-arrowWidth*1.6
          ar<-c(ar,r1)
          ax<-c(ax,xStart)
          ay<-c(ay,yStart)
          aLen<-c(aLen,arrowLength)
          aWid<-c(aWid,arrowWidth)
          
          rlabels<-c(rlabels,brawFormat(r1,digits=digits))
          rx<-c(rx,xStart+(xEnd-xStart)*fontSize*0.3)
          ry<-c(ry,yStart+(yEnd-yStart)*fontSize*0.3)
          rcol<-c(rcol,colArrow)
          rfg<-c(rfg,colLine)
          rdir<-c(rdir,direction)
        }

  # arrows first in increasing size order
  for (ai in order(abs(ar))) {
    g<-addG(g,drawArrow(c(ax[ai],ay[ai]),aLen[ai],direction=rdir[ai]+90,
                        width=aWid[ai],ends="last",col=rfg[ai],fill=rcol[ai]))
    
  }
  
  # now the effect size labels
  for (ri in order(abs(ar))) {
    if (ry[ri]<0) vjust=1 else vjust=0
    g<-addG(g,dataLabel(data.frame(x=rx[ri],y=ry[ri]),label=rlabels[ri],
                        size=0.6,hjust=0.5,vjust=0.5,# angle=rdir[ri],
                        fill=darken(rcol[ri],1,0.2),colour=rfg[ri]))
  }
  
  # now draw variable names
  for (ni in 1:length(names)) {
    fill="white"
    if (is.element(names[ni],colnames(pathmodel$Bdesign)) && all(pathmodel$Bdesign[,names[ni]]==0)) fill<-"#FFAAAA"
    if (!is.element(names[ni],rownames(pathmodel$Bdesign))) fill<-"#CCFF44"
    g<-addG(g,dataLabel(data.frame(x=xs[ni],y=ys[ni]),label=names[ni],
                        size=fontSize,hjust=0.5,vjust=0.5,fontface="bold",fill=fill))
  }

  # now summary result
  label<-paste0("AIC = ",brawFormat(pathmodel$eval$AIC),";  R^2 = ",brawFormat(pathmodel$eval$Rsquared),
                ";  k = ",brawFormat(pathmodel$eval$k),";  n = ",brawFormat(pathmodel$eval$n))
  g<-addG(g,dataLabel(data.frame(x=xlim[2]-diff(xlim)/30,y=ylim[1]+diff(ylim)/30),label,hjust=1,size=0.7))
  return(g)

}