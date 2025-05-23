makeFig<-function(hypothesis=braw.def$hypothesis,design=braw.def$design,targetSample=0.4) {
  
azimuth=-55
elevation=15
distance=8
draw_lower_limit<-0.001
  
colSsum<-"#FFBB88"
colSsumLine<-darken(colSsum,off=-0.5)
colP<-"white"
alpha<-0.75

BoxCol<-"#666666"
BoxColFloor<-darken(BoxCol,off=0.5)
BoxColSamples<-darken(BoxCol,off=0.35)
BoxColPopulations<-darken(BoxCol,off=0.4)

label.x<-"λ"
xlim<-c(0,1)
label.y<-"z[s]"
ylim<-c(-1,1)*braw.env$z_range
label.z<-"Probability Density"
zlim<-c(0,1)


braw.env$plotArea<-c(0,0,1,1)
mapping<-mapping3D(azimuth,elevation,distance,xlim,ylim,zlim)
g<-startPlot(xlim=c(-1,1),ylim=c(-1,1),
             box="none",backC=braw.env$plotColours$graphC)

  # floor
  g<-addG(g,
          dataPolygon(rotate3D(data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)],z=c(0,0,0,0)+zlim[1]),mapping),
                      colour=BoxColFloor,fill=BoxColFloor)
          )
  
  # back walls            
  g<-addG(g,
          dataPolygon(rotate3D(data.frame(x=xlim[c(1,1,1,1)],y=ylim[c(1,1,2,2)],z=zlim[c(1,2,2,1)]),mapping),
                      colour=BoxColSamples,fill=BoxColSamples
          ),
          dataPolygon(rotate3D(data.frame(x=xlim[c(1,1,2,2)],y=ylim[c(2,2,2,2)-1],z=zlim[c(1,2,2,1)]),mapping),
                      colour=BoxColPopulations, fill=BoxColPopulations
          )
  )

  # ticks and labels
  tick_grow<-4
  tick_more<-2
  xtick_length<-0.03*diff(xlim)
  ytick_length<-0.03*diff(ylim)
  zyt<- 0
  zxt<-1
  
  # z vertical axis
  g<-addG(g,dataPath(rotate3D(data.frame(x=c(0,0)+xlim[2],
                                         y=c(0,0)+ylim[1],
                                         z=zlim),mapping),colour="#000000")
  )
  # z ticks
  plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)
  tick.z.start <- rotate3D(data.frame(x=xlim[2],y=ylim[1],z=plot_ticks), mapping)
  tick.z.end <- rotate3D(data.frame(x=xlim[2]+zxt*xtick_length,y=ylim[1]+zyt*ytick_length,z=plot_ticks), mapping)
  for (i in 1:length(tick.z.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.z.start$x[i],tick.z.end$x[i]),y=c(tick.z.start$y[i],tick.z.end$y[i]))))
  # long ticks
  long_ticks<-seq(zlim[1],zlim[2],diff(zlim)/2)
  tick.z.start <- rotate3D(data.frame(x=xlim[2],y=ylim[1],z=long_ticks), mapping)
  tick.z.end <- rotate3D(data.frame(x=xlim[2]+zxt*xtick_length,y=ylim[1]+zyt*ytick_length,z=long_ticks), mapping)
  for (i in 1:length(tick.z.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.z.start$x[i],tick.z.end$x[i]),y=c(tick.z.start$y[i],tick.z.end$y[i]))))
  # label
  pos.z<-rotate3D(data.frame(x=xlim[2]+zxt*diff(xlim)*0.08,y=ylim[1]+zyt*diff(ylim)*0.08,z=mean(zlim)),mapping)
  rotate.z=rotate3D(data.frame(x=c(xlim[2],xlim[2]),
                               y=c(ylim[1],ylim[1]),
                               z=zlim),mapping)
  rotate.z<- 180-zyt*atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296+zxt*atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296
  g<-addG(g,dataText(data.frame(x=pos.z$x,y=pos.z$y),label=label.z,angle=rotate.z,hjust=0.5,size=0.7,fontface="bold"))
  

  # x ticks
  g<-addG(g,dataPath(rotate3D(data.frame(x=xlim,
                                         y=c(ylim[2],ylim[2]),
                                         z=c(zlim[1],zlim[1])),mapping),colour="#000000")
  )
  plot_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
  long_ticks<-seq(ceil(xlim[1]*2),floor(xlim[2]*2))/2
  if (length(long_ticks)==1)
    long_ticks<-seq(ceil(xlim[1]*5),floor(xlim[2]*5))/5
  if (length(long_ticks)==1)
    long_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
  
  # short ticks  
  tick.x.start <- rotate3D(data.frame(x=plot_ticks, y=ylim[2], z=zlim[1]), mapping)
  tick.x.end <- rotate3D(data.frame(x=plot_ticks, y=ylim[2]+ytick_length, z=zlim[1]), mapping)
  for (i in 1:length(tick.x.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.x.start$x[i],tick.x.end$x[i]),y=c(tick.x.start$y[i],tick.x.end$y[i]))))
  # long ticks
  tick.x.start <- rotate3D(data.frame(x=long_ticks, y=ylim[2], z=zlim[1]), mapping)
  tick.x.end <- rotate3D(data.frame(x=long_ticks, y=ylim[2]+ytick_length*tick_more, z=zlim[1]), mapping)
  for (i in 1:length(tick.x.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.x.start$x[i],tick.x.end$x[i]),y=c(tick.x.start$y[i],tick.x.end$y[i]))))
  # tick labels
  ticks.x<-rotate3D(data.frame(x=long_ticks,y=ylim[2]+ytick_length*tick_grow,z=zlim[1]),mapping)
  g<-addG(g,dataText(data.frame(x=ticks.x$x,y=ticks.x$y),1-long_ticks,hjust=1,vjust=0.5,size=0.7))
  
  # y ticks
  g<-addG(g,dataPath(rotate3D(data.frame(x=c(xlim[2],xlim[2]),
                                         y=ylim,
                                         z=c(zlim[1],zlim[1])),mapping),colour="#000000")
  )
  plot_ticks<-seq(ceil(ylim[1]*4),floor(ylim[2]*4))/4
  long_ticks<-seq(ceil(ylim[1]),floor(ylim[2]))
  if (length(long_ticks)==1)
    long_ticks<-seq(ceil(ylim[1]*5),floor(ylim[2]*5))/5
  # short ticks  
  tick.y.start <- rotate3D(data.frame(x=xlim[2], y=plot_ticks, z=zlim[1]), mapping)
  tick.y.end <- rotate3D(data.frame(x=xlim[2]+xtick_length, y=plot_ticks, z=zlim[1]), mapping)
  for (i in 1:length(tick.y.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.y.start$x[i],tick.y.end$x[i]),y=c(tick.y.start$y[i],tick.y.end$y[i]))))
  # long ticks
  tick.y.start <- rotate3D(data.frame(x=xlim[2], y=long_ticks, z=zlim[1]), mapping)
  tick.y.end <- rotate3D(data.frame(x=xlim[2]+xtick_length*tick_more, y=long_ticks, z=zlim[1]), mapping)
  for (i in 1:length(tick.y.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.y.start$x[i],tick.y.end$x[i]),y=c(tick.y.start$y[i],tick.y.end$y[i]))))
  # tick labels
  ticks.y<-rotate3D(data.frame(x=xlim[2]+xtick_length*tick_grow,y=long_ticks,z=zlim[1]),mapping)
  g<-addG(g,dataText(data.frame(x=ticks.y$x,y=ticks.y$y),long_ticks,hjust=0.5,vjust=0.5,size=0.7))
  
  pos.x<-rotate3D(data.frame(x=sum(xlim)/2,y=ylim[2]+ytick_length*tick_grow*1.5,z=zlim[1]),mapping)
  g<-addG(g,dataText(pos.x,label.x,hjust=0.5,vjust=0.5,fontface="bold"))
  
  pos.y<-rotate3D(data.frame(x=xlim[2]+xtick_length*tick_grow*2.2,y=sum(ylim)/2,z=zlim[1]),mapping)
  g<-addG(g,dataText(pos.y,label.y,hjust=0.5,vjust=0.5,fontface="bold"))
  
  
  zs<-seq(-1,1,length.out=101)*3
  lambdas<-seq(1,0.01,length.out=101)
  g<-addG(g,dataPath(rotate3D(data.frame(x=c(0,1),y=rep(targetSample,2),z=c(0,0)),mapping)))
  
  world<-hypothesis$effect$world
  v<-c()
  for (il in 1:length(lambdas)) {
    world$populationPDFk<-lambdas[il]
    sampDist<-fullRSamplingDist(tanh(zs),world,design,sigOnly=FALSE)
    sampDist<-rdens2zdens(sampDist,tanh(zs))
    gain<-6
    # if (il==1) gain<-1/max(sampDist)
    popDist<-rPopulationDist(tanh(zs),world)
    use<-sampDist>draw_lower_limit
    data<-data.frame(x=1-rep(lambdas[il],sum(use)),y=zs[use],z=sampDist[use]*gain)
    v<-c(v,approx(zs,sampDist*gain,targetSample)$y)
    if (round((il-1)/10)*10+1 == il) {
    g<-addG(g,dataPolygon(rotate3D(data,mapping),colour=colSsumLine,fill=colSsum,alpha=alpha))
    g<-addG(g,dataPath(rotate3D(data.frame(x=1-lambdas[il]+c(0,0),y=rep(targetSample,2),z=c(0,v[il])),mapping)))
    }
  }
  g<-addG(g,dataPath(rotate3D(data.frame(x=1-lambdas,y=rep(targetSample,length(lambdas)),z=v),mapping)))
  print(g)
}
