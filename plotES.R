
ESplotMargins<-margin(0.0,-0.2,0,-1,"cm")

drawEffectES<-function(r,t=1){
  switch (t,
          {start=c(0,0.95)
          direction=0
          len=0.9
          labelpts<-data.frame(x=0.45,y=0.55)
          ends="last"
          col=plotcolours$maineffectES},
          
          {start=c(-0.4,0.75)
          direction=45
          len=sqrt(2)*0.75
          labelpts<-data.frame(x=-0.25,y=0.375)
          ends="last"
          col=plotcolours$maineffectES},

          {start=c(0.4,0.75)
          direction=-45
          len=sqrt(2)*0.75
          labelpts<-data.frame(x=0.25,y=0.375)
          ends="last"
          col=plotcolours$maineffectES},
          
          {start=c(0.7,0.5)
          direction=-90
          len=1.4
          labelpts<-data.frame(x=0,y=0.55)
          ends="both"
          col=plotcolours$covariationES},
          
          {start=c(0,0.5)
          direction=0
          len=0.5
          labelpts<-data.frame(x=0,y=0.85)
          ends="join"
          col=plotcolours$interactionES}
  )
      d=0.08
    dx=d*cos(45/(180/pi)) 
    dy=d*sin(45/(180/pi))
      longSidex=(2*dx+d/2)
      longSidey=dy*4
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
              arrow_x<-cumsum(c(d/2,0,dx,dx,-longSidex,-longSidex,dx,dx,0,-finx,dx,finx,finx,dx,-finx    ))
              arrow_y<-cumsum(c(  0,len-longSidey,-dy,dy,longSidey,-longSidey,-dy,dy,-(len-longSidey),-finy,-dy,finy,-finy,dy,finy))
            }
    )
    x<-arrow_x*cos(direction/(180/pi))+arrow_y*sin(direction/(180/pi))
    y<-arrow_x*sin(direction/(180/pi))-arrow_y*cos(direction/(180/pi))
    pts<-data.frame(x=x+start[1],y=y+start[2])
  g<-ggplot(pts,aes(x=x,y=y))+
    geom_polygon(color="black",fill=col, lwd=0.5)+coord_fixed(1,xlim=c(-1,1),ylim=c(0,1))
  
  if (simData) {
    if (t==1){
      lbl=paste("r=",as.character(r),sep="")
    }else{ lbl=as.character(r)
    }
    g<-g+geom_label(data=labelpts,aes(x = mean(x), y = mean(y), label = lbl), color="black", fill = "white",size=labelSize)
  }
  
  g + 
    plotTheme+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(plot.margin=ESplotMargins)+
    theme(panel.background = element_rect(fill="transparent", colour="transparent"),
          plot.background = element_rect(fill="transparent", colour="transparent"))+
    labs(x="",y="")#+
    # coord_cartesian(c(-1,1), ylim = c(0, 1))
  
}
