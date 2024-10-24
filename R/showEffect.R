drawArrow<-function(start,len,direction,ends,col="black",fill="white",width=0.08) {
  d=width
  dx=d*cos(45/(180/pi)) 
  dy=d*sin(45/(180/pi))
  longSidex=(2*dx+d/2)
  longSidey=dy*2.5
  switch (ends,
          "last"={
            arrow_x<-cumsum(c(0, d/2,0,dx,dx,-longSidex,-longSidex,dx,dx,0,d/2))
            arrow_y<-cumsum(c(0, 0,len-longSidey,-dy,dy,longSidey,-longSidey,-dy,dy,-(len-longSidey),0))
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
  
  dataPolygon(data=pts,colour=col,fill=fill)
  
}


#' export
showEffect<-function(r,t=1,showValue=TRUE,plotArea=NULL,g=NULL){

  if (!is.null(plotArea)) braw.env$plotArea<-plotArea
  
  g<-startPlot(xlim=c(-1,1),ylim=c(0,1),back="transparent",box="none",g=g)
  
  switch (t,
          {start=c(0,0.92)
          direction=0
          len=0.9
          labelpts<-data.frame(x=-0.1,y=0.5)
          hjust<-1
          ends="last"
          fill=braw.env$plotColours$maineffectES},
          
          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=atan(0.55/0.9)*57.296
          labelpts<-data.frame(x=0,y=0.3)
          hjust<- 0.35
          ends="last"
          fill=braw.env$plotColours$maineffectES
          },

          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=-atan(0.55/0.9)*57.296
          labelpts<-data.frame(x=0,y=0.3)
          hjust<- 0.65
          ends="last"
          fill=braw.env$plotColours$maineffectES
          },
          
          {start=c(0.6,0.5)
          direction=-90
          len=1.2
          labelpts<-data.frame(x=0,y=0.6)
          hjust<-0.5
          ends="both"
          fill=braw.env$plotColours$covariationES},
          
          {start=c(0,0.46)
          direction=0
          len=0.45
          labelpts<-data.frame(x=0,y=0.7)
          hjust<-0.5
          ends="join"
          fill=braw.env$plotColours$interactionES}
  )
  g<-addG(g,drawArrow(start,len,direction,ends,fill=fill))
  
  
  if (showValue && braw.env$simData && !is.null(r)) {
    if (t==1){
      lbl=paste("r[p]=",as.character(r),sep="")
    }else{ 
      if (r==0) lbl<-"0.0" else lbl<-as.character(r)
    }
    g<-addG(g,dataText(data=labelpts, label = lbl, size=1, hjust=hjust, fontface="bold"))
  }
  
  return(g)

}
