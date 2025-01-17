drawArrow<-function(start,len,direction,ends,col="black",fill="white",alpha=1, width=0.1,position="start",finAngle=45) {
  if (position=="centre") {
    start<-c(
      start[1]-len/2*sin((direction-90)/(180/pi)),
      start[2]-len/2*cos((direction-90)/(180/pi))
    )
  }
  finAngle<-finAngle/(180/pi)
  d=width*sin(finAngle)
  dx=d*cos(finAngle) 
  dy=d*sin(finAngle)
  longSidex=(2*dx+d/2)
  longSidey=dy*2.5
  switch (ends,
          "last"={
            arrow_x<-cumsum(c(0, d/2,0,dx,dx,-longSidex,-longSidex,dx,dx,0,d/2))
            arrow_y<-cumsum(c(0, 0,len-longSidey,-dy,dy,longSidey,-longSidey,-dy,dy,-(len-longSidey),0))
            # r<-width*2.5
            # arrow_x<-c(r*sin(finAngle),-width*sin(finAngle),r*sin(finAngle)-width*sin(finAngle)-width/2,0,-width/2)
            # arrow_x<-c(arrow_x,rev(arrow_x))
            # arrow_y<-c(r*cos(finAngle),0,(r*sin(finAngle)-width*sin(finAngle)-width/2)/tan(finAngle),len-r*cos(finAngle)+(r*sin(finAngle)-width*sin(finAngle)-width/2)/tan(finAngle),0)
            # arrow_y<-c(arrow_y,-rev(arrow_y))
            # arrow_x<-cumsum(arrow_x)
            # arrow_y<-cumsum(arrow_y)
          },
          "both"={
            arrow_x<-cumsum(c(0,  longSidex,-dx,-dx,0,              dx,dx,-longSidex,-longSidex,dx,dx,0,               -dx,-dx,longSidex))
            arrow_y<-cumsum(c(0,  longSidey, dy,-dx,len-2*longSidey,-dy,dy,longSidey,-longSidey,-dy,dy,-(len-2*longSidey),dy,-dy,-longSidey))
            
          },
          "join"={
            fin=0.6
            finx=fin*cos(45/(180/pi))
            finy=fin*sin(45/(180/pi))
            longSidex<-longSidex
            arrow_x<-cumsum(c(d/2,0,dx,dx,-longSidex,-longSidex,dx,dx,0,-finx,dx,finx-dx/3.3,finx-dx/3.3,dx,-finx    ))
            arrow_y<-cumsum(c(  0,len-longSidey,-dy,dy,longSidey,-longSidey,-dy,dy,-(len-longSidey),-finy,-dy,finy,-finy,dy,finy))
          }
  )
  x<-arrow_x*cos(direction/(180/pi))+arrow_y*sin(direction/(180/pi))
  y<-arrow_x*sin(direction/(180/pi))-arrow_y*cos(direction/(180/pi))
  pts<-data.frame(x=x+start[1],y=y+start[2])
  
  dataPolygon(data=pts,colour=col,fill=fill,alpha=alpha)
  
}


#' export
showEffect<-function(r,t=1,showValue=TRUE,plotArea=NULL,g=NULL){

  if (!is.null(plotArea)) braw.env$plotArea<-plotArea
  if (length(r)>1) {rSD<-r[2]; r<-r[1]} else {rSD<-NULL}
  
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1),back="transparent",box="none",g=g)
  
  switch (t,
          # 1
          {start=c(0,0.92)
          direction=0
          len=0.9
          labelpts<-data.frame(x=-0.1,y=0.5)
          hjust<-1
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=1
          },
          # 2
          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=atan(0.55/0.9)*57.296
          labelpts<-data.frame(x=0,y=0.3)
          hjust<- 0.35
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 3
          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=-atan(0.55/0.9)*57.296
          labelpts<-data.frame(x=0,y=0.3)
          hjust<- 0.65
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 4
          {start=c(0.6,0.5)
          direction=-90
          len=1.2
          labelpts<-data.frame(x=0,y=0.6)
          hjust<-0.5
          ends="both"
          fill=braw.env$plotColours$covariationES
          size=0.7
          },
          # 5
          {start=c(0,0.46)
          direction=0
          len=0.45
          labelpts<-data.frame(x=0,y=0.7)
          hjust<-0.5
          ends="join"
          fill=braw.env$plotColours$interactionES
          size=0.7
          },
          # 6
          {start=c(0,0.92)
          direction=0
          len=0.9
          labelpts<-data.frame(x=-0.1,y=0.5)
          hjust<-1
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 7
          {start=c(0.92,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=-atan(0.55/0.35)*57.296
          labelpts<-data.frame(x=0.6,y=0.4)
          hjust<- 0.35
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 8
          {start=c(0,0.82)
          len=sqrt(0.9^2+0.55^2)
          direction=atan(0.55/0.35)*57.296
          labelpts<-data.frame(x=0.6,y=0.7)
          hjust<- 0.35
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 9
          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=atan(0.55/0.35)*57.296
          labelpts<-data.frame(x=0.0,y=0.5)
          hjust<- 0.35
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 10
          {start=c(0.92,0.92)
          direction=0
          len=0.9
          labelpts<-data.frame(x=1.1,y=0.5)
          hjust<-0
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          },
          # 11
          {start=c(0.92,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=-atan(0.55/0.35)*57.296
          labelpts<-data.frame(x=0.0,y=0.7)
          hjust<- 0.35
          ends="last"
          fill=braw.env$plotColours$maineffectES
          size=0.7
          }
  )
  col<-"#000000"
  alpha<-1
  if (is.element(t,c(4,6,7,8)) && !is.null(r) && r==0) {
    fill<-darken(desat(fill,0.5),off=0.8)
    # alpha<-0.2
    col<-darken(col,off=0.8)
  }
  g<-addG(g,drawArrow(start,len,direction,ends,col=col,fill=fill,alpha=alpha))
  
  
  if (showValue && braw.env$simData && !is.null(r)) {
    if (t==1){
      lbl=paste("r[p]=",brawFormat(r,digits=2),sep="")
      if (!is.null(rSD) && rSD!=0) lbl<-paste0(lbl,"\u00B1",brawFormat(rSD,digits=1))
    }else{ 
      if (r==0) lbl<-"0.0" else lbl<-as.character(r)
    }
    g<-addG(g,dataText(data=labelpts, label = lbl, size=size*1, hjust=hjust, colour=col, fontface="bold"))
  }
  
  return(g)

}
