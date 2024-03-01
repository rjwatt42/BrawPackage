g<-startPlot(c(-3,3),c(-1,1),"both","horz")
g<-g+xAxisLabel("x")+xAxisTicks(seq(-3,3,1))
g<-g+yAxisLabel("y")+yAxisTicks(seq(-1,1,0.5))

x<-seq(-3,3,length.out=201)
y<-sin(x/3*2*pi)
data<-data.frame(x=x,y=y)
g<-g+dataLine(data)
g<-g+dataPolygon(data)

x<-seq(-3,3,length.out=31)
y<-sin(x/3*2*pi)
data<-data.frame(x=x,y=y)
g<-g+dataPoint(data)

g<-g+vertLine(0)+horzLine(0.5)
g<-g+dataLabel(data.frame(x=0.0,y=0.5),label="test")

print(g)