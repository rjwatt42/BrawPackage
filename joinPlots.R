joinPlots<-function(g1,g2=NULL,g3=NULL,layout="triangle") {
  
  gridTheme<-theme(plot.margin=margin(-0.3,0,0,0,"cm"))
  
  g<-ggplot()+BrawOpts$blankTheme+theme(plot.margin=margin(0.3,-0.2,0,0,"cm"))
  g<-g+scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+
       scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
  
  if (is.null(g2)) {
    return(g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=10,ymin=0,ymax=10)
    )
  }
  if (is.null(g3)) {
    return(g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4.5,ymin=0,ymax=10)+
             annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5,xmax=10,ymin=0,ymax=10)
    )
  } 
  switch(layout,
         "triangle"={
           return(g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)+
                    annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=0,xmax=4,ymin=0,ymax=5)+
                    annotation_custom(grob=ggplotGrob(g3+gridTheme),xmin=6,xmax=10,ymin=0,ymax=5)
           )
         },
         "linear"={
           return(g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4,ymin=0,ymax=10)+
                    annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=4,xmax=7,ymin=0,ymax=10)+
                    annotation_custom(grob=ggplotGrob(g3+gridTheme),xmin=7,xmax=10,ymin=0,ymax=10)
           )
         }
  )
}
