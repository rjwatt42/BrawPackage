min_p=0.0001
truncate_p=FALSE
min_nw=10
max_nw=10000
horiz_scatter=0.5

se_size=0.75
se_arrow=0.3
CI=0.95

histGain<-NA
histGainrange<-c(NA,NA)

varLab<-function(label,orientation) {
  switch(orientation,
         "vert"={
           ylab(label)
         },
         "horz"={
           xlab(label)
         }
  )
}
densLab<-function(label,orientation){
  switch(orientation,
         "vert"={
           if (is.null(label)) {
             theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())
           } else {
             ylab(label)
           }
         },
         "horz"={
           if (is.null(label)) {
             ylab("Density")
           } else {
             ylab(label)
           }
         }
  )
}
dataLabel<-function(data,orientation) {
  geom_label(data=data,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
}
varScale<-function(breaks,labels,orientation){
  switch(orientation,
         "vert"={
           scale_y_continuous(breaks=breaks,labels=labels)
         },
         "horz"={
           scale_x_continuous(breaks=breaks,labels=labels)
         }
  )
}
densScale<-function(breaks,labels,orientation){
  switch(orientation,
         "vert"={
           scale_x_continuous(breaks=breaks,labels=labels)
         },
         "horz"={
           scale_y_continuous(breaks=breaks,labels=labels)
         }
  )
}
plotLimits<-function(xlim,ylim,orientation) {
  switch(orientation,
         "vert"={
           coord_cartesian(xlim=xlim,ylim=ylim)
         },
         "horz"={
           xlim[1]<-0
           coord_cartesian(xlim=ylim,ylim=xlim)
         }
  )
}
varLine<-function(intercept=NULL,linetype="solid",color,linewidth,orientation){
  switch(orientation,
         "vert"={
           geom_hline(yintercept=intercept,linetype=linetype, color=color, linewidth=linewidth)
         },
         "horz"={
           geom_vline(xintercept=intercept,linetype=linetype, color=color, linewidth=linewidth)
         }
         )
}
dataLine<-function(data,arrow=NULL,colour,linewidth,orientation) {
  switch(orientation,
         "vert"={
           geom_line(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linewidth=linewidth)
         },
         "horz"={
           geom_line(data=data,aes(x=y,y=x),arrow=arrow,colour=colour,linewidth=linewidth)
         }
  )
}
dataPath<-function(data,arrow=NULL,colour,linewidth,orientation) {
  switch(orientation,
         "vert"={
           geom_path(data=data,aes(x=x,y=y),arrow=arrow,colour=colour,linewidth=linewidth)
         },
         "horz"={
           use<-data$x>=0 & c(diff(data$y),0)>=0
           geom_path(data=data[use,],aes(x=y,y=x),arrow=arrow,colour=colour,linewidth=linewidth)
         }
  )
}
dataPoint<-function(data,shape,colour,fill,size,orientation) {
  switch(orientation,
         "vert"={
           geom_point(data=data,aes(x=x,y=y),shape=shape, colour = colour, fill = fill, size = size)
         },
         "horz"={
           geom_point(data=data,aes(x=y,y=x),shape=shape, colour = colour, fill = fill, size = size)
         }
  )
}
dataPolygon<-function(data,colour,fill,alpha=1, orientation) {
  switch(orientation,
         "vert"={
           geom_polygon(data=data,aes(x=x,y=y),colour = colour, fill = fill,alpha=alpha)
         },
         "horz"={
           data$x[data$x<0]<-0
           geom_polygon(data=data,aes(x=y,y=x),colour = colour, fill = fill,alpha=alpha)
         }
  )
}

collectData<-function(result) {
  use<-(!is.na(result$rIV))
  ns<-cbind(result$nval[use])
  df1<-cbind(result$df1[use])
  rp<-cbind(result$rpIV[use])
  ro<-cbind(result$roIV[use])
  po<-cbind(result$poIV[use])
  
  if (all(is.na(result$rIV2))){
    rs<-cbind(result$rIV[use])
    ps<-cbind(result$pIV[use])
  } else {
    switch (result$showType,
            "direct"={
              rs<-rbind(result$r$direct[use,])
              ps<-rbind(result$p$direct[use,])
            },
            "unique"={
              rs<-rbind(result$r$unique[use,])
              ps<-rbind(result$p$unique[use,])
            },
            "total"={
              rs<-rbind(result$r$total[use,])
              ps<-rbind(result$p$total[use,])
            },
            "all"={
              rs<-c()
              ps<-c()
              ysc=1/3
              xoff=c(0,0,0,2,2,2,4,4,4)
              for (jk in 1:ncol(result$r$direct)) {
                rs<-cbind(rs,result$r$direct[use,jk],result$r$unique[use,jk],result$r$total[use,jk])
                ps<-cbind(ps,result$p$direct[use,jk],result$p$unique[use,jk],result$p$total[use,jk])
              }
            },
            "coefficients"={
              rs<-rbind(result$r$coefficients[use])
              ps<-rbind(result$p$direct[use,])
            }
    )
  }
  if (truncate_p) {
    ps[ps<min_p]<-min_p
    po[po<min_p]<-min_p
  }
  out<-list(rs=rs,ps=ps,ns=ns,df1=df1,rp=rp,ro=ro,po=po)
}

makeFiddle<-function(y,yd,orientation){
  yz<-c()
  xz<-c()
  xd<-0.15
  
  for (i in 1:length(y)){
    found<-(abs(yz-y[i])<yd)
    if (any(found,na.rm=TRUE)) {
      x_max<-max(xz[found])
      x_which<-which.max(xz[found])
      y_at_max<-yz[found][x_which]
      x_min<-min(xz[found])
      x_which<-which.min(xz[found])
      y_at_min<-yz[found][x_which]
      if (orientation=="vert" && abs(x_min)<x_max) {
        x_inc<-sqrt(1-((y[i]-y_at_min)/yd)^2)
        xz<-c(xz,x_min-x_inc*xd)
        yz<-c(yz,y[i])
      } else {
      x_inc<-sqrt(1-((y[i]-y_at_max)/yd)^2)
      xz<-c(xz,x_max+x_inc*xd)
      yz<-c(yz,y[i])
      }
    } else {
      xz<-c(xz,0)
      yz<-c(yz,y[i])
    }
  }
  if (orientation=="horz") xz<-xz/2
  return(xz)
}

get_upperEdge<-function(allvals,svals){
  target1<-min(svals,na.rm=TRUE)
  if (any(allvals<target1,na.rm=TRUE)){
    target2<-max(allvals[allvals<target1],na.rm=TRUE)
    target<-(target1+target2)/2
  } else target<-target1+0.001
}
get_lowerEdge<-function(allvals,svals) {
  target1<-min(svals,na.rm=TRUE)
  if (any(allvals<target1)){
    target2<-max(allvals[allvals<target1],na.rm=TRUE)
    if (target2==-Inf) target2=target1-0.5
    target<-(target1+target2)/2
  } else {target<-target1-0.5}
}

getBins<-function(vals,nsvals,target,minVal,maxVal,fixed=FALSE) {
  if (min(vals,na.rm=TRUE)==max(vals,na.rm=TRUE)) {
    bins<-min(vals)+min(vals)/10*c(-1.5,-0.5,0.5,1.5)
    return(bins)
    }
  
  nv=max(length(nsvals),length(vals))
  nb<-round(sqrt(nv)*0.75)
  
  high_p<-max(vals,na.rm=TRUE)+0.2
  low_p<-min(vals,na.rm=TRUE)-0.2
  if (!is.null(minVal)) {
    low_p<-max(minVal,low_p,na.rm=TRUE)
  }
  if (!is.null(maxVal)) {
    high_p<-min(maxVal,high_p,na.rm=TRUE)
  }
  
  if ((length(nsvals)==0) || (length(nsvals)==length(vals))){
    bins<-seq(low_p,high_p,length.out=nb)
    return(bins)
  }

  if (fixed) {
    target_low<-max(-target,low_p)
    target_high<-min(target,high_p)
    targetRange<-target_high-target_low
    nbs<-ceiling(nb*targetRange/(high_p-low_p))
    binStep<-targetRange/nbs
    bins<-seq(target_low,target_high,binStep)
    if (target<high_p) {
      bins<-c(bins,seq(target+binStep,high_p+binStep,binStep))
    }
    if (-target>low_p) {                                
      bins<-c(rev(seq(-target-binStep,low_p-binStep,-binStep)),bins)
    }
    return(bins)
  } 
  
  # make sure it goes through target
  if (length(target)>1) {
    if (high_p>target[2] && low_p< target[1]) {
      nbs<-ceiling(nb*(target[2]-0)/(high_p-low_p))
      binStep<-target[2]/nbs
      bins<-c(rev(seq(0,low_p-binStep,-binStep)),seq(binStep,high_p,binStep))
      return(bins)
    }
    if (high_p>target[2]) {
      nbs<-ceiling(nb*(high_p-target[2])/(high_p-low_p))
      binStep<-(high_p-target[2])/nbs
      bins<-rev(seq(high_p,low_p-binStep,-binStep))
      return(bins)
    } 
    if (low_p<target[1]) {
      nbs<-ceiling(nb*(target[1]-low_p)/(high_p-low_p))
      binStep<-(target[1]-low_p)/nbs
      bins<-seq(low_p-binStep,high_p,binStep)
      return(bins)
    } 
  } else {
    if (high_p>target) {
      nbs<-ceiling(nb*(high_p-target)/(high_p-low_p))
      binStep<-(high_p-target)/nbs
      bins<-rev(seq(high_p,low_p-binStep,-binStep))
      return(bins)
    } 
    if (low_p<target) {
      nbs<-ceiling(nb*(target-low_p)/(high_p-low_p))
      binStep<-(target-low_p)/nbs
      bins<-seq(low_p-binStep,high_p,binStep)
      return(bins)
    } 
  }
  # if all else fails
  binStep<-(high_p-low_p)/nb
  bins<-seq(low_p-binStep,high_p,binStep)
  return(bins)
}

expected_hist<-function(vals,svals,valType){

  if (is.null(valType)) valType<-"r"
  if (is.element(valType,c("r1","ci1","ci2"))) valType<-"r"
  if (is.element(valType,c("e1","e2","p1"))) valType<-"p"
  if (is.element(valType,c("wp"))) valType<-"w"
  
  switch (valType,
          "r"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "ra"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "rp"=  { # ns is small
            target<-0.3
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "p"=  { # ns is large
            if (pPlotScale=="log10") {
              target<-log10(alphaSig)
              bins<-getBins(vals,svals,target,log10(min_p),log10(1))
            } else {
              target<-alphaSig
              bins<-getBins(vals,svals,target,0,1)
              bins<-c(0,bins[bins>0])
            }
          },
            
          "log(lrs)"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),0,lrRange)
          },
          
          "e1d"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-lrRange,lrRange)
          },
          
          "log(lrd)"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-lrRange,lrRange)
          },
          
          "e2d"={
            target<-alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-lrRange,lrRange)
          },
          
          "w"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,log10(min_p),NULL)
          },
          
          "t"=  { # ns is small
            target<-0
            bins<-getBins(vals,svals,target,NULL,NULL)
          },
          
          "n"= { # ns is small
            target<-get_lowerEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,10000)
            if (is.integer(vals)) {
              bins<-unique(floor(bins))
              binStep<-max(floor(median(diff(bins))),1)
              bins<-seq(bins[1],bins[length(bins)],binStep)
            }
          },
          
          "nw"= { # ns is large
            target<-get_lowerEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,max_nw)
          }
  )
  use<-vals>=bins[1] & vals<bins[length(bins)]
  dens<-hist(vals[use],breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens<-dens$counts

  use<-svals>=bins[1] & svals<bins[length(bins)]
  sdens<-hist(svals[use],breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  sdens<-sdens$counts

  if (is.na(histGain)) {
    sdens<-sdens/max(dens,na.rm=TRUE)/2
    dens<-dens/max(dens,na.rm=TRUE)/2
  } else {
    use<- (vals>=histGainrange[1]) & (vals<=histGainrange[2])
    sdens<-sdens/(sum(use)*(bins[2]-bins[1]))*histGain
    dens<-dens/(sum(use)*(bins[2]-bins[1]))*histGain
  }
  # browser()
  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
  y2<-c(0,as.vector(matrix(c(sdens,sdens),2,byrow=TRUE)),0)
  data.frame(y1=c(-y1,rev(y1)), y2=c(-y2,rev(y2)), x=c(x,rev(x)))
}

start_plot<-function(orientation) {
  g<-ggplot()
  g<-g+theme(legend.position = "none")+plotTheme
  g<-g+densScale(breaks=NULL,labels=NULL,orientation=orientation)
  g<-g+densLab(NULL,orientation)
}

expected_plot<-function(g,pts,expType=NULL,result=NULL,IV=NULL,DV=NULL,i=1,scale=1,col="white",orientation="vert"){
  dotSize<-(plotTheme$axis.title$size)/3*scale
  
  if (!is.null(expType)) {
    if (useSignificanceCols){
      c1=plotcolours$infer_sigC
      c2=plotcolours$infer_nsigC
    } else {
      c1=plotcolours$descriptionC
      c2=plotcolours$descriptionC
    }
    if (expType=="e1") {
      c1=plotcolours$infer_sigNull
      c2=plotcolours$infer_nsNull
    }
    if (expType=="e2") {
      c1=plotcolours$infer_sigNonNull
      c2=plotcolours$infer_nsNonNull
    }
    if (expType=="e1d") {
      c1=plotcolours$infer_sigNull
      c2=plotcolours$infer_nsdNull
      c3<-plotcolours$infer_isigNull
    }
    if (expType=="e2d") {
      c1=plotcolours$infer_sigNonNull
      c2=plotcolours$infer_nsdNonNull
      c3<-plotcolours$infer_isigNonNull
    }
  } else {
    c1=col
    c2=col
  }
  
  if (length(pts$y1)<=points_threshold) {
    if (!is.null(result) && is.element(expType,c("r","p")) && length(pts$y1)==1) {
      switch(i,
             {rCI<-result$rIVCI
             pCI<-result$pIVCI
             if (isSignificant(STMethod,result$pIV,result$rIV,result$nval,result$df1,result$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-result$rIV2CI
             pCI<-result$pIV2CI
             if (isSignificant(STMethod,result$pIV2,result$rIV2,result$nval,result$df2,result$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-result$rIVIV2CI
             pCI<-result$pIVIV2CI
             if (isSignificant(STMethod,result$pIVIV2DV,result$rIVIV2DV,result$nval,result$df12,result$evidence)) {c<-c1} else (c<-c2)
             }
             )
      if (expType=="r" && !is.null(rCI)){
        pts1se<-data.frame(x=pts$x,y=rCI)
        g<-g+dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size,orientation=orientation)
      }
      if (expType=="p" && !is.null(result$pIVCI)){
        pts1se<-data.frame(x=pts$x,y=log10(pCI))
        g<-g+dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size,orientation=orientation)
      }
    }
      
    xr<-makeFiddle(pts$y1,2/40,orientation)*scale*scale
    pts$x<-pts$x+xr
    
    if (scale<1) {
      co1<-c1
      co2<-c2
    } else {
      co1<-"black"
      co2<-"black"
    }
    pts1=pts[!pts$y2,]
    g<-g+dataPoint(data=data.frame(x=pts1$x,y=pts1$y1),shape=shapes$study, colour = co2, fill = c2, size = dotSize,orientation=orientation)
    pts2=pts[pts$y2,]
    g<-g+dataPoint(data=data.frame(x=pts2$x,y=pts2$y1),shape=shapes$study, colour = co1, fill = c1, size = dotSize,orientation=orientation)
    if (!is.null(expType))
      if (is.element(expType,c("e1d","e2d"))) {
        pts3=pts[pts$y3,]
        g<-g+dataPoint(data=data.frame(y=pts3$x,x=pts3$y1),shape=shapes$study, colour = co1, fill = c3, size = dotSize,orientation=orientation)
      }
    
  } else {
    if (is.logical(pts$y2)) {
      pts1<-expected_hist(pts$y1,pts$y1[pts$y2],expType)
    } else {
      pts1<-expected_hist(pts$y1,pts$y2,expType)
    }
    xoff<-pts$x[1]
    if (orientation=="vert") {
      simAlpha<-0.8
    } else {
      simAlpha<-0.7
    }
    g<-g+
      dataPolygon(data=data.frame(y=pts1$x,x=pts1$y1*scale*scale+xoff),colour=NA, fill = c2,alpha=simAlpha, orientation=orientation)+
      dataPolygon(data=data.frame(y=pts1$x,x=pts1$y2*scale*scale+xoff),colour=NA, fill = c1,alpha=simAlpha, orientation=orientation)
    if (!is.null(expType))
      if (is.element(expType,c("e1d","e2d"))) {
        if (is.logical(pts$y3)) {
          pts1<-expected_hist(pts$y1,pts$y1[pts$y3],expType)
        }
        g<-g+
          dataPolygon(data=data.frame(y=pts1$x,x=pts1$y2+xoff),colour=NA, fill = c3,alpha=simAlpha, orientation=orientation)
      }
  }
  g
}


r_plot<-function(result,expType="r",logScale=FALSE,otherresult=NULL,orientation="vert",showType="direct"){

  r<-result$effect$rIV
  if (!is.null(result$IV2)){
    r<-c(r,result$effect$rIV2,result$effect$rIVIV2DV)
  }
  rlims<-c(-1,1)
  rlab<-"r"
  
  if (RZ=="z") {
    r<-atanh(r)
    rlims<-c(-1,1)*z_range
    rlab<-"z"
  }
  rActual<-r
  rActual[is.na(r)]<-0

  if (all(is.na(result$rIVIV2DV)) && is.null(result$IV2)){
    xoff=0
  } else {
    if (is.na(result$rIVIV2DV[1])){
      xoff=c(0,2)
    } else {
      xoff=c(0,2,4)
    }
  }
  
  switch (expType,
          "r"={
            ylim<-rlims
            if (RZ=="z") ylabel<-zsLabel
            else ylabel<-rsLabel 
            },
          "p"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          },
          "p1"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p[1])
          },
          "log(lrs)"={
            ylim<-c(0, lrRange)
            ylabel<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            ylim<-c(-lrRange, lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "e1d"={
            ylim<-c(-lrRange, lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "e2d"={
            ylim<-c(-lrRange, lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "w"={
            ylim<-c(0.01, 1)
            ylabel<-bquote(w)
          },
          "nw"={
            ylim<-c(1, max_nw)
            ylabel<-bquote(n[w=80])
            },
          "n"={
            ylim<-c(1, result$design$sN*5*1.1)
            ylabel<-"n"
          },
          "t"={
            ylim<-c(-5,5)
            ylabel<-"t"
          },
          "rp"={
            ylim<-rlims
            if (RZ=="z") ylabel<-zpLabel
            else ylabel<-rpLabel 
          },
          "ra"={
            ylim<-rlims
            if (RZ=="z") ylabel<-zaLabel
            else ylabel<-raLabel 
          },
          "r1"={
            ylim<-rlims
            ylabel<-bquote(r[1])
          },
          "wp"={
            ylim<-c(0.01, 1)
            ylabel<-bquote(w)
          },
          "ci1"={
            ylim<-rlims
            ylabel<-rlab
          },
          "ci2"={
            ylim<-rlims
            ylabel<-rlab
          },
          "e1"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          },
          "e2"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          }
  )
  if (logScale) {
    ylim<-log10(ylim)
    ylabel<-bquote(bold(log['10'](.(ylabel))))
  }  else {
    ylabel<-bquote(bold(.(ylabel)))
  }
  
  if (!all(is.na(result$rIV))) {
    data<-collectData(result)
    if (RZ=="z") {
      data$rs<-atanh(data$rs)
      data$rp<-atanh(data$rp)
      data$ra<-atanh(data$ra)
      data$ro<-atanh(data$ro)
    }
    switch (expType,
            "r"={data$sh<-data$rs},
            "rp"={data$sh<-data$rp},
            "ra"={data$sh<-data$ra},
            "r1"={data$sh<-data$ro},
            "p"={data$sh<-data$ps},
            "p1"={data$sh<-data$po},
            "log(lrs)"={data$sh<-cbind(res2llr(result,"sLLR"))},
            "log(lrd)"={data$sh<-cbind(res2llr(result,"dLLR"))},
            "e1d"={data$sh<-cbind(res2llr(result,"dLLR"))},
            "e2d"={data$sh<-cbind(res2llr(result,"dLLR"))},
            "n"={data$sh<-data$ns},
            "t"={data$sh<-data$ts},
            "w"={data$sh<-rn2w(data$rs,data$ns)},
            "wp"={data$sh<-rn2w(data$rp,data$ns)},
            "nw"={data$sh<-rw2n(data$rs,0.8,result$design$sReplTails)},
            "ci1"={data$sh<-r2ci(data$rs,data$ns,-1)},
            "ci2"={data$sh<-r2ci(data$rs,data$ns,+1)},
            "e1"={data$sh<-data$ps},
            "e2"={data$sh<-data$ps}
    )
    if (logScale) {
      data$sh<-log10(data$sh)
    }  
  }    
  g<-start_plot(orientation=orientation)
  sigOnly<-result$evidence$sigOnly

  # make theory
  if (!all(is.na(result$rIV))) { theoryAlpha<-0.5} else {theoryAlpha<-1}
  for (i in 1:length(xoff)){
    if (showTheory) {
      if (!result$effect$world$worldOn) {
        result$effect$world$populationPDF<-"Single"
        result$effect$world$populationRZ<-"r"
        result$effect$world$populationPDFk<-result$effect$rIV
        result$effect$world$populationNullp<-0
      }
      if (orientation=="horz") {
        distMax<-0.8
      } else {
        distMax<-0.5
      }
      if (is.element(expType,c("p","e1","e2","p1"))) {
        if (logScale) {
          yv<-seq(0,log10(min_p),length.out=201)
          yvUse<-10^yv
        }else{
          yv<-seq(1,0,length.out=201)
          yvUse<-yv
        }
        xd<-fullRSamplingDist(yvUse,result$effect$world,result$design,"p",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
        xdsig<-fullRSamplingDist(yvUse,result$effect$world,result$design,"p",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
      } else {
        npt<-101
      switch(expType,
             "r"={
               if (RZ=="z") {
                 zvals<-seq(-1,1,length.out=npt*2)*z_range*2
                 rvals<-tanh(zvals)
                 # rvals<-seq(-1,1,length.out=npt)*0.99
                 xd<-fullRSamplingDist(rvals,result$effect$world,result$design,"r",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
                 xdsig<-fullRSamplingDist(rvals,result$effect$world,result$design,"r",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
                 xd<-rdens2zdens(xd,rvals)
                 xdsig<-rdens2zdens(xdsig,rvals)
                 yv<-atanh(rvals)
                 use<-abs(zvals)<=z_range
                 yv<-yv[use]
                 xd<-xd[use]
                 xdsig<-xdsig[use]
               } else {
                 rvals<-seq(-1,1,length.out=npt)*0.99
                 xd<-fullRSamplingDist(rvals,result$effect$world,result$design,"r",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
                 xdsig<-fullRSamplingDist(rvals,result$effect$world,result$design,"r",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
                 yv<-rvals
               }
             },
             "ra"={
               if (RZ=="z") {
                 yv<-seq(-1,1,length.out=npt)*z_range
                 xd<-fullRSamplingDist(tanh(yv),result$effect$world,result$design,"r",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
                 xdsig<-fullRSamplingDist(tanh(yv),result$effect$world,result$design,"r",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
                 xd<-rdens2zdens(xd,tanh(yv))
                 xdsig<-rdens2zdens(xdsig,tanh(yv))
               } else {
                 yv<-seq(-1,1,length.out=npt)*0.99
                 xd<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
                 xdsig<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
               }
             },
             "ci1"={
               yv<-seq(-1,1,length.out=npt)*0.99
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale,sigOnly=sigOnly)
             },
             "ci2"={
               yv<-seq(-1,1,length.out=npt)*0.99
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"r",logScale=logScale,sigOnly=sigOnly)
             },
             "w"={
               yv<-seq(alphaSig*1.01,1/1.01,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"w",logScale=logScale,sigOnly=sigOnly)
             },
             "log(lrs)"={
               yv<-seq(0,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrs)",logScale=logScale,sigOnly=sigOnly)
             },
             "log(lrd)"={
               yv<-seq(-lrRange,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
             },
             "e1d"={
               yv<-seq(-lrRange,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
             },
             "e2d"={
               yv<-seq(-lrRange,lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
             },
             "nw"={
               if (logScale) {
                 yv<-seq(log10(5),log10(max_nw),length.out=npt)
                 yvUse<-10^yv
               }else{
                 yv<-5+seq(0,max_nw,length.out=npt)
                 yvUse<-yv
               }
               xd<-fullRSamplingDist(yvUse,result$effect$world,result$design,"nw",logScale=logScale,sigOnly=sigOnly)
             },
             "rp"={
               if (RZ=="z") {
                 yv<-seq(-1,1,length.out=npt)*z_range
                 xd<-fullRPopulationDist(tanh(yv),result$effect$world)
                 xd<-rdens2zdens(xd,tanh(yv))
               } else {
                 yv<-seq(-1,1,length.out=npt)*0.99
                 xd<-fullRPopulationDist(yv,result$effect$world)
               }
             },
             "n"={
               ndist<-getNDist(result$design,result$effect$world,logScale=logScale,sigOnly=TRUE)
               yv<-ndist$nvals
               xd<-ndist$ndens
               xdsig<-ndist$ndensSig
             },
             "t"={
               yv<-seq(-5,5,length.out=npt)
               yvUse<-yv
               xd<-yv*0
               xd[yv==0]<-1
               xdsig<-xd
             },
             "wp"={
               yv<-seq(alphaSig*1.01,1/1.01,length.out=npt)
               xd<-fullRSamplingDist(yv,result$effect$world,result$design,"wp",logScale=logScale,sigOnly=sigOnly)
             }
      )
      }
      xd[is.na(xd)]<-0
      theoryGain<-1/max(xd)*distMax
      xd<-xd*theoryGain
      histGain<<-abs(sum(xd*c(0,diff(yv))))
      histGainrange<<-sort(c(yv[1],yv[length(yv)]))
      ptsp<-data.frame(x=c(xd,-rev(xd))+xoff[i],y=c(yv,rev(yv)))
      g<-g+dataPolygon(data=ptsp,colour=NA,fill="white",alpha=theoryAlpha, orientation=orientation)
      if (is.element(expType,c("r","n","p"))) {
        g<-g+dataPolygon(data=ptsp,colour=NA,fill=plotcolours$infer_nsigC,alpha=theoryAlpha, orientation=orientation)
        # xdsig[xdsig==0]<-NA
        ptsp1<-data.frame(x=c(xdsig,-rev(xdsig))*theoryGain+xoff[i],y=c(yv,rev(yv)))
        g<-g+dataPolygon(data=ptsp1,colour=NA,fill=plotcolours$infer_sigC,alpha=theoryAlpha, orientation=orientation)
        g<-g+dataPath(data=ptsp1,colour="black",linewidth=0.5, orientation=orientation)
        g<-g+dataPath(data=ptsp,colour="black",linewidth=0.5, orientation=orientation)
      } else {
        g<-g+dataPath(data=ptsp,colour="black",linewidth=0.5, orientation=orientation)
      }
    } else {
      histGain<<-NA
    }

    # then the samples
  if (!all(is.na(result$rIV))) {
      shvals<-data$sh[,i]
      rvals<-data$rs[,i]
      pvals<-data$ps[,i]
      nvals<-data$ns
      resSig<-isSignificant(STMethod,pvals,rvals,nvals,data$df1,result$evidence)
      if (sigOnly) {
        shvals<-shvals[resSig]
        rvals<-rvals[resSig]
        pvals<-pvals[resSig]
        nvals<-nvals[resSig]
        resSig<-resSig[resSig]
      }
      if (showType=="all") {
        ysc<-1/3
        rvals<-(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1
      }
      if (is.element(expType,c("e1d","e2d"))) {
        d<-res2llr(result,STMethod)
        err<-(d<0 & data$rp[,i]!=0) | (d>0 & data$rp[,i]==0)
        resWSig<-resSig & err
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y3=resWSig,n<-nvals)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,n<-nvals)
      }
      
      # if (RZ=="z" && is.element(expType,c("r","rp"))) {
      #   pts1$y1<-atanh(pts1$y1)
      # }
      g<-expected_plot(g,pts,expType,result,IV,DV,i,orientation=orientation)
    
    if (is.element(expType,c("p","e1","e2","e1d","e2d"))) {
      if (result$effect$world$worldOn && is.element(expType,c("e1","e2","e1d","e2d"))) {
        n<-length(pvals)
        if (!is.null(otherresult)) n<-n+length(otherresult$pIV)
        switch (expType,
                "e1"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s<-sum(resSig,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    sstr<-paste0("(",format(s),"/",format(n),")")
                  } else {
                    nstr<-""
                    sstr<-""
                  }
                  labelPt1<-paste0("p(ns correct) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(sig error) = ",format(s/n*100,digits=2),"% ",sstr)
                },
                "e2"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s<-sum(resSig,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    sstr<-paste0("(",format(s),"/",format(n),")")
                  } else {
                    nstr<-""
                    sstr<-""
                  }
                  labelPt1<-paste0("p(ns miss) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(sig correct) = ",format(s/n*100,digits=2),"% ",sstr)
                },
                "e1d"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s2<-sum(resSig & shvals<0,na.rm=TRUE)
                  s1<-sum(resSig & shvals>0,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    s2str<-paste0("(",format(s2),"/",format(n),")")
                    s1str<-paste0("(",format(s1),"/",format(n),")")
                  } else {
                    nstr<-""
                    s2str<-""
                    s1str<-""
                  }
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(sig correct) = ",format(s2/n*100,digits=2),"% ",s1str)
                  labelPt1<-paste0("p(sig error) = ",format(s1/n*100,digits=2),"% ",s1str)
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"%")
                  labelPt1a<-paste0("p(sig correct) = ",format(s2/n*100,digits=2),"%")
                  labelPt1<-paste0("p(sig error) = ",format(s1/n*100,digits=2),"%")
                },
                "e2d"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s2<-sum(resSig & shvals<0,na.rm=TRUE)
                  s1<-sum(resSig & shvals>0,na.rm=TRUE)
                  if (n<=10000) {
                    nstr<-paste0("(",format(ns),"/",format(n),")")
                    s2str<-paste0("(",format(s2),"/",format(n),")")
                    s1str<-paste0("(",format(s1),"/",format(n),")")
                  } else {
                    nstr<-""
                    s2str<-""
                    s1str<-""
                  }
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"% ",nstr)
                  labelPt1a<-paste0("p(sig error) = ",format(s2/n*100,digits=2),"% ",s2str)
                  labelPt1<-paste0("p(sig correct) = ",format(s1/n*100,digits=2),"% ",s1str)
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"%")
                  labelPt1a<-paste0("p(sig error) = ",format(s2/n*100,digits=2),"%")
                  labelPt1<-paste0("p(sig correct) = ",format(s1/n*100,digits=2),"%")
                }
        )
        lpts1<-data.frame(x = xoff[i]-0.98, y = ylim[2]+diff(ylim)/25,label = labelPt1)
        g<-g+dataLabel(data=lpts1,orientation=orientation)
        lpts1a<-data.frame(x = xoff[i]-0.98, y = ylim[1]-diff(ylim)/25,label = labelPt1a)
        g<-g+dataLabel(data=lpts1a,orientation=orientation)
        if (is.element(expType,c("e1d","e2d"))) {
          lpts1<-data.frame(x = xoff[i]-0.98, y = sum(ylim)/2,label = labelPt1b)
          g<-g+dataLabel(data=lpts1,orientation=orientation)
        }
      } else {
        switch (expType,
                "p"={labelPt1<-paste0("p(sig)"," = ")},
                "e1"={labelPt1<-"p(Type I) = "},
                "e2"={labelPt1<-"p(Type II) = "}
        )
        if (expType=="e2") {
        labelPt2<-paste0(labelPt1,format(mean(!resSig,na.rm=TRUE)*100,digits=graph_precision),"%")
        labelPt3<-paste0(labelPt2,"  (",format(sum(!resSig,na.rm=TRUE)),"/",format(length(pvals)),")")
      } else {
        labelPt2<-paste0(labelPt1,format(mean(resSig,na.rm=TRUE)*100,digits=graph_precision),"%")
        labelPt3<-paste0(labelPt2,"  (",format(sum(resSig,na.rm=TRUE)),"/",format(length(pvals)),")")
      }
      if (length(xoff)>1) {
        lpts1<-data.frame(x = xoff[i]-0.95, y = ylim[2]+diff(ylim)/25,label = labelPt2)
        # lpts2<-data.frame(x = xoff[i]-0.95, y = ylim[1]-diff(ylim)/25,label = labelPt2)
      } else {
        lpts1<-data.frame(x = xoff[i]-0.95, y = ylim[2]+diff(ylim)/25,label = labelPt3)
        # lpts2<-data.frame(x = xoff[i]-0.95, y = ylim[1]-diff(ylim)/25,label = labelPt3)
      }
      g<-g+dataLabel(data=lpts1,orientation=orientation)
      # if (!is.null(lpts2)) {
      # g<-g+geom_label(data=lpts2,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=labelSize)
      # }
      }
    }
    
    if (is.element(expType,c("r","ci1","ci2"))) {
      lpts<-data.frame(x = xoff[i]-0.95, y = ylim[2],label=paste("actual =",format(rActual[i],digits=graph_precision)))
      g<-g+dataLabel(data=lpts,orientation=orientation)
    }
  }
  }
  
  if (length(xoff)>1)
    if (rem(i,3)==1)
      switch (xoff[i]/2+1,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
      )

  if (showType=="all") {
    for (i in 1:3) {
      g<-g+varLine(intercept=(-1+1)*ysc*0.9+(i-1)*ysc*2-1, color="black", linewidth=1,orientation=orientation)
      g<-g+varLine(intercept=(0.0+1)*ysc*0.9+(i-1)*ysc*2-1, linetype="dotted", color="black", linewidth=0.5,orientation=orientation)
      g<-g+varLine(intercept=(1+1)*ysc*0.9+(i-1)*ysc*2-1, color="black", linewidth=1,orientation=orientation)
    }
    g<-g+plotLimits(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16),orientation=orientation)+
      varScale(breaks=(c(-1,0,1,-1,0,1,-1,0,1)+1)*ysc*0.9+(c(1,1,1,2,2,2,3,3,3)-1)*ysc*2-1,labels=c(-1,0,1,-1,0,1,-1,0,1),orientation = orientation)
  } else {
    g<-g+varLine(intercept=0.0, linetype="dotted", color="black", linewidth=0.5,orientation=orientation)+
      plotLimits(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16),orientation=orientation)
  }
  g<-g+varLab(ylabel,orientation)
  g
}

l_plot<-function(result,ptype=NULL,otherresult=NULL,orientation="vert"){
  g<-r_plot(result,ptype,orientation=orientation)
  sAlpha<-log(dnorm(0)/dnorm(qnorm(1-alphaSig/2)))
  g<-g+varLine(intercept=sAlpha, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
  if (ptype=="log(lrd)")
  g<-g+varLine(intercept=-sAlpha, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
  g
}

p_plot<-function(result,ptype="p",otherresult=NULL,PlotScale=pPlotScale,orientation="vert",showType="direct"){

  g<-r_plot(result,ptype,PlotScale=="log10",otherresult,orientation=orientation,showType=showType)
  
  if (is.element(ptype,c("p","p1"))) {
  if (PlotScale=="log10") {
    g<-g+varLine(intercept=log10(1), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varLine(intercept=log10(0.005), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varLine(intercept=log10(0.01), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varLine(intercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varScale(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1),orientation=orientation)
  } else
  {
    g<-g+varLine(intercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
  }
  }
  g
}

w_plot<-function(result,wtype,orientation="vert"){
  g<-r_plot(result,wtype,wPlotScale=="log10",orientation=orientation)
  
  if (wPlotScale=="log10") {
    g<-g+varLine(intercept=log10(alphaSig), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=log10(0.5), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=log10(0.8), linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
  } else {
    g<-g+varLine(intercept=alphaSig, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=0.5, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=0.8, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
  }
  g
}

n_plot<-function(result,ntype,orientation="vert"){
  r_plot(result,ntype,nPlotScale=="log10",orientation=orientation)
}

e2_plot<-function(result,nullresult=NULL,orientation="vert"){
  distr<-tolower(result$effect$world$populationPDF)
  lambda<-format(result$effect$world$populationPDFk,digits=3)
  switch (RZ,
          "r"={
            lab<-bquote(bold("Non-null:  " ~ r["p"] ~ "~" ~ .(distr) (r/.(lambda))))
          },
          "z"={
            lab<-bquote(bold("Non-null:  " ~ z["p"] ~ "~" ~ .(distr) (z/.(lambda))))
          }
  )
  switch (STMethod,
          "NHST"={
            p_plot(result,ptype="e2",otherresult=nullresult,orientation=orientation)+
              ggtitle(lab)
          },
          "sLLR"={
            p_plot(result,ptype="e2",otherresult=nullresult,orientation=orientation)+
              ggtitle(lab)
          },
          "dLLR"={
            g<-p_plot(nullresult,ptype="e2d",otherresult=nullresult,PlotScale="linear",orientation=orientation)
            g<-g+varLine(intercept=alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
            g<-g+varLine(intercept=-alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
            g+ggtitle(lab)
          }
  )
}

e1_plot<-function(nullresult,result=NULL,orientation="vert"){
  switch (RZ,
          "r"={
            lab<-bquote(bold("Null:  " ~ r["p"] == 0))
          },
          "z"={
            lab<-bquote(bold("Null:  " ~ z["p"] == 0))
          }
  )
  switch (STMethod,
          "NHST"={
            p_plot(nullresult,ptype="e1",otherresult=result,orientation=orientation)+
              ggtitle(lab)
          },
          "sLLR"={
            p_plot(nullresult,ptype="e1",otherresult=result,orientation=orientation)+
              ggtitle(lab)
          },
          "dLLR"={
            g<-p_plot(nullresult,ptype="e1d",otherresult=result,PlotScale="linear",orientation=orientation)
            g<-g+varLine(intercept=alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
            g<-g+varLine(intercept=-alphaLLR, linetype="dotted", color=plotcolours$alpha, linewidth=0.5,orientation=orientation)
            g+ggtitle(lab)
          }
  )
}


