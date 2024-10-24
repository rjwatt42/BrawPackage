rotate3D <- function(data,azimuth,elevation, distance)
{
  azimuth <- azimuth*pi/180
  elevation <- elevation*pi/180
    caz <- cos(azimuth)
    saz <- sin(azimuth)
    cel <- cos(elevation)
    sel <- sin(elevation)
  rot.mat<-matrix(c(caz, 0, saz, 0,
                    saz*sel,cel,-caz*sel, 0,
                    -cel*saz, sel,caz*cel, 0,
                    0,0,0,1
                    ),4,4,byrow=TRUE)

  tdata <- rot.mat %*% rbind(data, 1)
  tdata[1,] <- tdata[1,]/tdata[4,]
  tdata[2,] <- tdata[2,]/tdata[4,]
  tdata[3,] <- tdata[3,]/tdata[4,]
  
  scale<-rot.mat%*%matrix(c(1,1,1,1,
                            -1,1,1,1,
                            -1,-1,1,1,
                            -1,1,-1,1,
                            1,1,1,-1,
                            -1,1,1,-1,
                            -1,-1,1,-1,
                            -1,1,-1,-1
  ),ncol=8)
  scale[1,]<-scale[1,]/scale[4,]
  scale[2,]<-scale[2,]/scale[4,]
  scale[3,]<-scale[3,]/scale[4,]
  
  if (distance != 0)  
  {
    tdata[1,] <- tdata[1,] / (distance - tdata[3,])
    tdata[2,] <- tdata[2,] / (distance - tdata[3,])
    scale[1,]<-scale[1,]/(distance-scale[3,])
    scale[2,]<-scale[2,]/(distance-scale[3,])
  }
  
  return(tdata[1:2,]/max(scale[1:2,]))
}

  azimuth=10
  elevation=10
  distance=20
  
corners <-
  rbind(x = c(-1,1,1,-1,-1,1,1,-1),
        y = c(-1,-1,-1,-1,1,1,1,1) ,
        z = c(-1,-1,1,1,-1,-1,1,1)
        )

corners <- rotate3D(corners, azimuth,elevation, distance)
plot(corners[1,],corners[2,])
use<-c(1:4,1)
lines(corners[1,use],corners[2,use])
lines(corners[1,4+use],corners[2,4+use])
use<-c(1,4,8,5,1)
lines(corners[1,use],corners[2,use])
use<-c(2,3,7,6,2)
lines(corners[1,use],corners[2,use])

print(max(corners))
