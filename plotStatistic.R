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
  geom_label(data=data,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=BrawOpts$labelSize)
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

collectData<-function(analysis,effectType) {
  use<-(!is.na(analysis$rIV))
  ns<-cbind(analysis$nval[use])
  df1<-cbind(analysis$df1[use])
  rp<-cbind(analysis$rpIV[use])
  ro<-cbind(analysis$roIV[use])
  po<-cbind(analysis$poIV[use])
  
  if (all(is.na(analysis$rIV2))){
    rs<-cbind(analysis$rIV[use])
    ra<-cbind(analysis$raIV[use])
    ps<-cbind(analysis$pIV[use])
  } else {
    ra<-cbind(analysis$raIV[use])
    switch (effectType,
            "direct"={
              rs<-rbind(analysis$r$direct[use,])
              ps<-rbind(analysis$p$direct[use,])
            },
            "unique"={
              rs<-rbind(analysis$r$unique[use,])
              ps<-rbind(analysis$p$unique[use,])
            },
            "total"={
              rs<-rbind(analysis$r$total[use,])
              ps<-rbind(analysis$p$total[use,])
            },
            "all"={
              rs<-c()
              ps<-c()
              ysc=1/3
              xoff=c(0,0,0,2,2,2,4,4,4)
              for (jk in 1:ncol(analysis$r$direct)) {
                rs<-cbind(rs,analysis$r$direct[use,jk],analysis$r$unique[use,jk],analysis$r$total[use,jk])
                ps<-cbind(ps,analysis$p$direct[use,jk],analysis$p$unique[use,jk],analysis$p$total[use,jk])
              }
            },
            "coefficients"={
              rs<-rbind(analysis$r$coefficients[use])
              ps<-rbind(analysis$p$direct[use,])
            }
    )
  }
  if (truncate_p) {
    ps[ps<min_p]<-min_p
    po[po<min_p]<-min_p
  }
  out<-list(rs=rs,ps=ps,ns=ns,df1=df1,rp=rp,ra=ra,ro=ro,po=po)
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
  if (is.element(valType,c("ro","ci1","ci2"))) valType<-"r"
  if (is.element(valType,c("e1","e2","po"))) valType<-"p"
  if (is.element(valType,c("wp"))) valType<-"w"
  
  switch (valType,
          "r"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },

          "p"=  { # ns is large
            if (pPlotScale=="log10") {
              target<-log10(BrawOpts$alphaSig)
              bins<-getBins(vals,svals,target,log10(min_p),log10(1))
            } else {
              target<-BrawOpts$alphaSig
              bins<-getBins(vals,svals,target,0,1)
              bins<-c(0,bins[bins>0])
            }
          },
            
          "rp"=  { # ns is small
            target<-0.3
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "log(lrs)"={
            target<-BrawOpts$alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),0,BrawOpts$lrRange)
          },
          
          "e1d"={
            target<-BrawOpts$alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-BrawOpts$lrRange,BrawOpts$lrRange)
          },
          
          "log(lrd)"={
            target<-BrawOpts$alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-BrawOpts$lrRange,BrawOpts$lrRange)
          },
          
          "e2d"={
            target<-BrawOpts$alphaLLR
            bins<-getBins(vals,svals,target*c(-1,1),-BrawOpts$lrRange,BrawOpts$lrRange)
          },
          
          "w"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,log10(min_p),NULL)
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
    use<- (bins>=histGainrange[1]) & (bins<=histGainrange[2])
    gain<-sum(dens[use]*c(0,diff(bins[use])),na.rm=TRUE)
    sdens<-sdens/gain*histGain
    dens<-dens/gain*histGain
  }
  # browser()
  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
  y2<-c(0,as.vector(matrix(c(sdens,sdens),2,byrow=TRUE)),0)
  data.frame(y1=c(-y1,rev(y1)), y2=c(-y2,rev(y2)), x=c(x,rev(x)))
}

start_plot<-function(orientation) {
  g<-ggplot()
  g<-g+theme(legend.position = "none")+BrawOpts$plotTheme
  g<-g+densScale(breaks=NULL,labels=NULL,orientation=orientation)
  g<-g+densLab(NULL,orientation)
}

expected_plot<-function(g,pts,expType=NULL,analysis=NULL,IV=NULL,DV=NULL,i=1,scale=1,col="white",orientation="vert"){
  dotSize<-(BrawOpts$plotTheme$axis.title$size)/3*scale
  
  if (!is.null(expType)) {
    if (useSignificanceCols){
      c1=BrawOpts$plotColours$infer_sigC
      c2=BrawOpts$plotColours$infer_nsigC
    } else {
      c1=BrawOpts$plotColours$descriptionC
      c2=BrawOpts$plotColours$descriptionC
    }
    if (expType=="e1") {
      c1=BrawOpts$plotColours$infer_sigNull
      c2=BrawOpts$plotColours$infer_nsNull
    }
    if (expType=="e2") {
      c1=BrawOpts$plotColours$infer_sigNonNull
      c2=BrawOpts$plotColours$infer_nsNonNull
    }
    if (expType=="e1d") {
      c1=BrawOpts$plotColours$infer_sigNull
      c2=BrawOpts$plotColours$infer_nsdNull
      c3<-BrawOpts$plotColours$infer_isigNull
    }
    if (expType=="e2d") {
      c1=BrawOpts$plotColours$infer_sigNonNull
      c2=BrawOpts$plotColours$infer_nsdNonNull
      c3<-BrawOpts$plotColours$infer_isigNonNull
    }
  } else {
    c1=col
    c2=col
  }
  
  if (length(pts$y1)<=50) {
    if (!is.null(analysis) && is.element(expType,c("r","p")) && length(pts$y1)==1) {
      switch(i,
             {rCI<-analysis$rIVCI
             pCI<-analysis$pIVCI
             if (isSignificant(BrawOpts$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-analysis$rIV2CI
             pCI<-analysis$pIV2CI
             if (isSignificant(BrawOpts$STMethod,analysis$pIV2,analysis$rIV2,analysis$nval,analysis$df2,analysis$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-analysis$rIVIV2CI
             pCI<-analysis$pIVIV2CI
             if (isSignificant(BrawOpts$STMethod,analysis$pIVIV2DV,analysis$rIVIV2DV,analysis$nval,analysis$df12,analysis$evidence)) {c<-c1} else (c<-c2)
             }
             )
      if (is.null(analysis$hypothesis$IV2)) {
      if (expType=="r" && !is.null(rCI)){
        pts1se<-data.frame(x=pts$x,y=rCI)
        g<-g+dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size,orientation=orientation)
      }
      if (expType=="p" && !is.null(analysis$pIVCI)){
        pts1se<-data.frame(x=pts$x,y=log10(pCI))
        g<-g+dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size,orientation=orientation)
      }
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
    g<-g+dataPoint(data=data.frame(x=pts1$x,y=pts1$y1),shape=BrawOpts$plotShapes$study, colour = co2, fill = c2, size = dotSize,orientation=orientation)
    pts2=pts[pts$y2,]
    g<-g+dataPoint(data=data.frame(x=pts2$x,y=pts2$y1),shape=BrawOpts$plotShapes$study, colour = co1, fill = c1, size = dotSize,orientation=orientation)
    if (!is.null(expType))
      if (is.element(expType,c("e1d","e2d"))) {
        pts3=pts[pts$y3,]
        g<-g+dataPoint(data=data.frame(y=pts3$x,x=pts3$y1),shape=BrawOpts$plotShapes$study, colour = co1, fill = c3, size = dotSize,orientation=orientation)
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


r_plot<-function(analysis,expType="r",logScale=FALSE,otheranalysis=NULL,orientation="vert",effectType="direct",showTheory=TRUE){

  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  evidence<-analysis$evidence
  
  r<-effect$rIV
  if (!is.null(hypothesis$IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
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

  if (all(is.na(analysis$rIVIV2DV)) && is.null(hypothesis$IV2)){
    xoff=0
  } else {
    if (is.na(analysis$rIVIV2DV[1])){
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
          "ra"={
            ylim<-rlims
            if (RZ=="z") ylabel<-bquote(z[a])
            else ylabel<-bquote(r[a])
          },
          "p"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p)
          },
          "rp"={
            ylim<-rlims
            if (RZ=="z") ylabel<-zpLabel
            else ylabel<-rpLabel 
          },
          "ro"={
            ylim<-rlims
            ylabel<-bquote(r[o])
          },
          "po"={
            ylim<-c(min_p, 1)
            ylabel<-bquote(p[o])
          },
          "log(lrs)"={
            ylim<-c(0, BrawOpts$lrRange)
            ylabel<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            ylim<-c(-BrawOpts$lrRange, BrawOpts$lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "e1d"={
            ylim<-c(-BrawOpts$lrRange, BrawOpts$lrRange)
            ylabel<-bquote(log[e](lr[d]))
          },
          "e2d"={
            ylim<-c(-BrawOpts$lrRange, BrawOpts$lrRange)
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
            ylim<-c(1, design$sN*5*1.1)
            ylabel<-"n"
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
  
  if (!all(is.na(analysis$rIV))) {
    data<-collectData(analysis,effectType)
    if (RZ=="z") {
      data$rs<-atanh(data$rs)
      data$rp<-atanh(data$rp)
      data$ro<-atanh(data$ro)
    }
    switch (expType,
            "r"={data$sh<-data$rs},
            "ra"={data$sh<-data$ra},
            "rp"={data$sh<-data$rp},
            "ro"={data$sh<-data$ro},
            "p"={data$sh<-data$ps},
            "po"={data$sh<-data$po},
            "log(lrs)"={data$sh<-cbind(res2llr(analysis,"sLLR"))},
            "log(lrd)"={data$sh<-cbind(res2llr(analysis,"dLLR"))},
            "e1d"={data$sh<-cbind(res2llr(analysis,"dLLR"))},
            "e2d"={data$sh<-cbind(res2llr(analysis,"dLLR"))},
            "n"={data$sh<-data$ns},
            "w"={data$sh<-rn2w(data$rs,data$ns)},
            "wp"={data$sh<-rn2w(data$rp,data$ns)},
            "nw"={data$sh<-rw2n(data$rs,0.8,analysis$design$sReplTails)},
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
  sigOnly<-evidence$sigOnly

  # make theory
  if (!effect$world$worldOn) {
    effect$world$worldOn<-TRUE
    effect$world$populationPDF<-"Single"
    effect$world$populationRZ<-"r"
    effect$world$populationPDFk<-effect$rIV
    effect$world$populationNullp<-0
  }
  
  if (!all(is.na(analysis$rIV))) { theoryAlpha<-0.5} else {theoryAlpha<-1}
  
  for (i in 1:length(xoff)){
    if (showTheory) {
      
      if (is.element(expType,c("p","e1","e2","po"))) {
        npt<-201
        if (logScale) {
          yv<-seq(0,log10(min_p),length.out=npt)
          yvUse<-10^yv
        }else{
          yv<-seq(1,0,length.out=npt)
          yvUse<-yv
        }
        oldEffect<-effect
          if (expType=="e1") effect$world$populationNullp<-1
          if (expType=="e2") effect$world$populationNullp<-0
        xd<-fullRSamplingDist(yvUse,effect$world,design,"p",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
        xdsig<-fullRSamplingDist(yvUse,effect$world,design,"p",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
        effect<-oldEffect
      } 
        
      if (is.element(expType,c("r","ra","ro","ci1","ci2"))) {
        npt<-101
        if (RZ=="z") {
          zvals<-seq(-1,1,length.out=npt*2)*z_range*2
          rvals<-tanh(zvals)
          # rvals<-seq(-1,1,length.out=npt)*0.99
          xd<-fullRSamplingDist(rvals,effect$world,design,"r",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
          xdsig<-fullRSamplingDist(rvals,effect$world,design,"r",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
          xd<-rdens2zdens(xd,rvals)
          xdsig<-rdens2zdens(xdsig,rvals)
          yv<-atanh(rvals)
          use<-abs(zvals)<=z_range
          yv<-yv[use]
          xd<-xd[use]
          xdsig<-xdsig[use]
        } else {
          rvals<-seq(-1,1,length.out=npt)*0.99
          xd<-fullRSamplingDist(rvals,effect$world,design,"r",logScale=logScale,sigOnly=FALSE,HQ=showTheoryHQ)
          xdsig<-fullRSamplingDist(rvals,effect$world,design,"r",logScale=logScale,sigOnly=TRUE,HQ=showTheoryHQ)
          yv<-rvals
        }
      }
      
        npt<-101
        switch(expType,
               "rp"={
                 if (RZ=="z") {
                   yv<-seq(-1,1,length.out=npt)*z_range
                   xd<-fullRPopulationDist(tanh(yv),effect$world)
                   xd<-rdens2zdens(xd,tanh(yv))
                 } else {
                   yv<-seq(-1,1,length.out=npt)*0.99
                   xd<-fullRPopulationDist(yv,effect$world)
                 }
               },
               "n"={
                 ndist<-getNDist(analysis$design,effect$world,logScale=logScale,sigOnly=TRUE)
                 yv<-ndist$nvals
                 xd<-ndist$ndens
                 xdsig<-ndist$ndensSig
               },
               "w"={
                 yv<-seq(BrawOpts$alphaSig*1.01,1/1.01,length.out=npt)
                 xd<-fullRSamplingDist(yv,effect$world,design,"w",logScale=logScale,sigOnly=sigOnly)
               },
               "log(lrs)"={
                 yv<-seq(0,BrawOpts$lrRange,length.out=npt)
                 xd<-fullRSamplingDist(yv,effect$world,design,"log(lrs)",logScale=logScale,sigOnly=sigOnly)
               },
               "log(lrd)"={
                 yv<-seq(-BrawOpts$lrRange,BrawOpts$lrRange,length.out=npt)
                 xd<-fullRSamplingDist(yv,effect$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
               },
               "e1d"={
                 yv<-seq(-BrawOpts$lrRange,BrawOpts$lrRange,length.out=npt)
                 xd<-fullRSamplingDist(yv,effect$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
               },
               "e2d"={
                 yv<-seq(-BrawOpts$lrRange,BrawOpts$lrRange,length.out=npt)
                 xd<-fullRSamplingDist(yv,effect$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
               },
               "nw"={
                 if (logScale) {
                   yv<-seq(log10(5),log10(max_nw),length.out=npt)
                   yvUse<-10^yv
                 }else{
                   yv<-5+seq(0,max_nw,length.out=npt)
                   yvUse<-yv
                 }
                 xd<-fullRSamplingDist(yvUse,effect$world,design,"nw",logScale=logScale,sigOnly=sigOnly)
               },
               "wp"={
                 yv<-seq(BrawOpts$alphaSig*1.01,1/1.01,length.out=npt)
                 xd<-fullRSamplingDist(yv,effect$world,design,"wp",logScale=logScale,sigOnly=sigOnly)
               },
               { } # do nothing
        )

      if (orientation=="horz")  distMax<-0.8
      else distMax<-0.5
      
      xd[is.na(xd)]<-0
      xdsig[is.na(xdsig)]<-0
      theoryGain<-1/max(xd)*distMax
      xd<-xd*theoryGain
      histGain<<-abs(sum(xd*c(0,diff(yv))))
      histGainrange<<-sort(c(yv[1],yv[length(yv)]))
      ptsp<-data.frame(x=c(xd,-rev(xd))+xoff[i],y=c(yv,rev(yv)))
      g<-g+dataPolygon(data=ptsp,colour=NA,fill="white",alpha=theoryAlpha, orientation=orientation)
      if (is.element(expType,c("r","n","p"))) {
        g<-g+dataPolygon(data=ptsp,colour=NA,fill=BrawOpts$plotColours$infer_nsigC,alpha=theoryAlpha, orientation=orientation)
        # xdsig[xdsig==0]<-NA
        ptsp1<-data.frame(x=c(xdsig,-rev(xdsig))*theoryGain+xoff[i],y=c(yv,rev(yv)))
        g<-g+dataPolygon(data=ptsp1,colour=NA,fill=BrawOpts$plotColours$infer_sigC,alpha=theoryAlpha, orientation=orientation)
        # g<-g+dataPath(data=ptsp1,colour="black",linewidth=0.1, orientation=orientation)
        g<-g+dataPath(data=ptsp,colour="black",linewidth=0.1, orientation=orientation)
      } else {
        g<-g+dataPath(data=ptsp,colour="black",linewidth=0.1, orientation=orientation)
      }
    } else {
      histGain<<-NA
    }

    # then the samples
  if (!all(is.na(analysis$rIV))) {
      shvals<-data$sh[,i]
      rvals<-data$rs[,i]
      pvals<-data$ps[,i]
      nvals<-data$ns
      resSig<-isSignificant(BrawOpts$STMethod,pvals,rvals,nvals,data$df1,evidence)
      if (sigOnly) {
        shvals<-shvals[resSig]
        rvals<-rvals[resSig]
        pvals<-pvals[resSig]
        nvals<-nvals[resSig]
        resSig<-resSig[resSig]
      }
      if (effectType=="all") {
        ysc<-1/3
        rvals<-(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1
      }
      if (is.element(expType,c("e1d","e2d"))) {
        d<-res2llr(analysis,BrawOpts$STMethod)
        err<-(d<0 & data$rp[,i]!=0) | (d>0 & data$rp[,i]==0)
        resWSig<-resSig & err
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y3=resWSig,n<-nvals)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,n<-nvals)
      }
      
      # if (RZ=="z" && is.element(expType,c("r","rp"))) {
      #   pts1$y1<-atanh(pts1$y1)
      # }
      g<-expected_plot(g,pts,expType,analysis,IV,DV,i,orientation=orientation)
    
    if (is.element(expType,c("p","e1","e2","e1d","e2d"))) {
      if (effect$world$worldOn && is.element(expType,c("e1","e2","e1d","e2d"))) {
        n<-length(pvals)
        if (!is.null(otheranalysis)) n<-n+length(otheranalysis$pIV)
        switch (expType,
                "e1"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s<-sum(resSig,na.rm=TRUE)
                  labelPt1<-paste0("p(ns correct) = ",format(ns/n*100,digits=2),"% ")
                  labelPt1a<-paste0("p(sig error) = ",format(s/n*100,digits=2),"% ")
                },
                "e2"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s<-sum(resSig,na.rm=TRUE)
                  labelPt1<-paste0("p(ns miss) = ",format(ns/n*100,digits=2),"% ")
                  labelPt1a<-paste0("p(sig correct) = ",format(s/n*100,digits=2),"% ")
                },
                "e1d"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s2<-sum(resSig & shvals<0,na.rm=TRUE)
                  s1<-sum(resSig & shvals>0,na.rm=TRUE)
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"% ")
                  labelPt1a<-paste0("p(sig correct) = ",format(s2/n*100,digits=2),"% ")
                  labelPt1<-paste0("p(sig error) = ",format(s1/n*100,digits=2),"% ")
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"%")
                  labelPt1a<-paste0("p(sig correct) = ",format(s2/n*100,digits=2),"%")
                  labelPt1<-paste0("p(sig error) = ",format(s1/n*100,digits=2),"%")
                },
                "e2d"={
                  ns<-sum(!resSig,na.rm=TRUE)
                  s2<-sum(resSig & shvals<0,na.rm=TRUE)
                  s1<-sum(resSig & shvals>0,na.rm=TRUE)
                  labelPt1b<-paste0("p(ns) = ",format(ns/n*100,digits=2),"% ")
                  labelPt1a<-paste0("p(sig error) = ",format(s2/n*100,digits=2),"% ")
                  labelPt1<-paste0("p(sig correct) = ",format(s1/n*100,digits=2),"% ")
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
      } else {
        labelPt2<-paste0(labelPt1,format(mean(resSig,na.rm=TRUE)*100,digits=graph_precision),"%")
      }
        lpts1<-data.frame(x = xoff[i]-0.95, y = ylim[2]+diff(ylim)/25,label = labelPt2)
      g<-g+dataLabel(data=lpts1,orientation=orientation)
      # if (!is.null(lpts2)) {
      # g<-g+geom_label(data=lpts2,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill="white",size=BrawOpts$labelSize)
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

  if (effectType=="all") {
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

l_plot<-function(analysis,ptype=NULL,otheranalysis=NULL,orientation="vert",showTheory=TRUE){
  g<-r_plot(analysis,ptype,orientation=orientation,showTheory=showTheory)
  sAlpha<-log(dnorm(0)/dnorm(qnorm(1-BrawOpts$alphaSig/2)))
  g<-g+varLine(intercept=sAlpha, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
  if (ptype=="log(lrd)")
  g<-g+varLine(intercept=-sAlpha, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
  g
}

p_plot<-function(analysis,ptype="p",otheranalysis=NULL,PlotScale=pPlotScale,orientation="vert",effectType="direct",showTheory=TRUE){

  g<-r_plot(analysis,ptype,PlotScale=="log10",otheranalysis,orientation=orientation,effectType=effectType,showTheory=showTheory)
  
  if (is.element(ptype,c("p","po"))) {
  if (PlotScale=="log10") {
    g<-g+varLine(intercept=log10(1), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varLine(intercept=log10(0.005), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varLine(intercept=log10(0.01), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varLine(intercept=log10(BrawOpts$alphaSig), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
    g<-g+varScale(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1),orientation=orientation)
  } else
  {
    g<-g+varLine(intercept=log10(BrawOpts$alphaSig), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
  }
  }
  g
}

w_plot<-function(analysis,wtype,orientation="vert",showTheory=TRUE){
  g<-r_plot(analysis,wtype,wPlotScale=="log10",orientation=orientation,showTheory=showTheory)
  
  if (wPlotScale=="log10") {
    g<-g+varLine(intercept=log10(BrawOpts$alphaSig), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=log10(0.5), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=log10(0.8), linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
  } else {
    g<-g+varLine(intercept=BrawOpts$alphaSig, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=0.5, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)+
      varLine(intercept=0.8, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
  }
  g
}

n_plot<-function(analysis,ntype,orientation="vert",showTheory=TRUE){
  r_plot(analysis,ntype,nPlotScale=="log10",orientation=orientation,showTheory=showTheory)
}

e2_plot<-function(analysis,nullanalysis=NULL,orientation="vert",showTheory=TRUE){
  distr<-tolower(analysis$hypothesis$effect$world$populationPDF)
  lambda<-format(analysis$hypothesis$effect$world$populationPDFk,digits=3)
  switch (RZ,
          "r"={
            lab<-bquote(bold(r["p"] ~ "~" ~ .(distr) (r/.(lambda))))
          },
          "z"={
            lab<-bquote(bold(z["p"] ~ "~" ~ .(distr) (z/.(lambda))))
          }
  )

  switch (BrawOpts$STMethod,
          "NHST"={
            g<-p_plot(analysis,ptype="e2",otheranalysis=nullanalysis,orientation=orientation,showTheory=showTheory)
            g<-g+ggtitle(lab)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
          },
          "sLLR"={
            g<-p_plot(analysis,ptype="e2",otheranalysis=nullanalysis,orientation=orientation,showTheory=showTheory)
            g<-g+ggtitle(lab)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
          },
          "dLLR"={
            g<-p_plot(nullanalysis,ptype="e2d",otheranalysis=nullanalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory)
            g<-g+varLine(intercept=BrawOpts$alphaLLR, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
            g<-g+varLine(intercept=-BrawOpts$alphaLLR, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
            g<-g+ggtitle(lab)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
          }
  )
  return(g)
}

e1_plot<-function(nullanalysis,analysis=NULL,orientation="vert",showTheory=TRUE){
  switch (RZ,
          "r"={
            lab<-bquote(bold(r["p"] == 0))
          },
          "z"={
            lab<-bquote(bold(z["p"] == 0))
          }
  )
  
  switch (BrawOpts$STMethod,
          "NHST"={
            g<-p_plot(nullanalysis,ptype="e1",otheranalysis=analysis,orientation=orientation,showTheory=showTheory)
            g<-g+ggtitle(lab)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
          },
          "sLLR"={
            g<-p_plot(nullanalysis,ptype="e1",otheranalysis=analysis,orientation=orientation,showTheory=showTheory)+
              g<-g+ggtitle(lab)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
          },
          "dLLR"={
            g<-p_plot(nullanalysis,ptype="e1d",otheranalysis=analysis,PlotScale="linear",orientation=orientation,showTheory=showTheory)
            g<-g+varLine(intercept=BrawOpts$alphaLLR, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
            g<-g+varLine(intercept=-BrawOpts$alphaLLR, linetype="dotted", color=BrawOpts$plotColours$alpha, linewidth=0.5,orientation=orientation)
            g<-g+ggtitle(lab)+theme(plot.title=element_text(face='bold', size=9, hjust=0))
          }
  )
  return(g)
}


