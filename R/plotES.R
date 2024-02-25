#' export
drawEffectES<-function(r,t=1){

  xoff<-0
  switch (t,
          {start=c(0,0.92)
          direction=0
          len=0.9
          labelpts<-data.frame(x=0.05,y=0.6)
          ends="last"
          col=braw.env$plotColours$maineffectES},
          
          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=atan(0.55/0.9)*57.296
          labelpts<-data.frame(x=0.15,y=0.6)
          ends="last"
          col=braw.env$plotColours$maineffectES
          xoff<- 0
          },

          {start=c(0,0.92)
          len=sqrt(0.9^2+0.55^2)
          direction=-atan(0.55/0.9)*57.296
          labelpts<-data.frame(x=-0.15,y=0.6)
          ends="last"
          col=braw.env$plotColours$maineffectES
          xoff<- 0
          },
          
          {start=c(0.6,0.5)
          direction=-90
          len=1.2
          labelpts<-data.frame(x=0,y=0.6)
          ends="both"
          col=braw.env$plotColours$covariationES},
          
          {start=c(0,0.46)
          direction=0
          len=0.45
          labelpts<-data.frame(x=0,y=0.6)
          ends="join"
          col=braw.env$plotColours$interactionES}
  )
      d=0.08
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
  g<-ggplot(pts,aes(x=x,y=y))+
    geom_polygon(color="black",fill=col, lwd=0.5)#+coord_fixed(1,xlim=c(-1,1),ylim=c(0,1))
  
  if (braw.env$simData) {
    if (t==1){
      lbl=paste("r=",as.character(r),sep="")
    }else{ lbl=as.character(r)
    }
    g<-g+geom_label(data=labelpts,aes(x = mean(x), y = mean(y), label = lbl), color="black", fill = "white",size=braw.env$labelSize)
  }
  
  ESplotMargins<-margin(-0.5,-0.5,-0.5,-1,"cm")

  g + 
    braw.env$blankTheme+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(panel.spacing=margin(0,0,0,0,"cm"),plot.margin=ESplotMargins)+
    theme(panel.background = element_rect(fill="transparent", colour="transparent"),
          plot.background = element_rect(fill="transparent", colour="transparent"))+
    labs(x="",y="")+
    coord_cartesian(c(-1,1)*0.6+xoff, ylim = 0.5+c(-1, 1)*0.45)
  
}
