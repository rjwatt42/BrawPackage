

collectData<-function(analysis,effectType) {
  use<-(!is.na(analysis$rIV))
  ns<-cbind(analysis$nval[use])
  df1<-cbind(analysis$df1[use])
  rp<-cbind(analysis$rpIV[use])
  ro<-cbind(analysis$roIV[use])
  po<-cbind(analysis$poIV[use])
  
  if (all(is.na(analysis$rIV2))){
    rs<-cbind(analysis$rIV[use])
    ps<-cbind(analysis$pIV[use])
  } else {
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
  # if (braw.env$truncate_p) {
  #   ps[ps<braw.env$min_p]<-braw.env$min_p
  #   po[po<braw.env$min_p]<-braw.env$min_p
  # }
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
  maxBins<-251
  
  nv=max(length(nsvals),length(vals))
  nb<-min(round(sqrt(nv)*0.75),maxBins)
  
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
    if (target_low<target_high)   {
      bins<-seq(target_low,target_high,binStep)
      if (target<high_p)  bins<-c(bins,seq(target+binStep,high_p+binStep,binStep))
      if (-target>low_p)  bins<-c(rev(seq(-target-binStep,low_p-binStep,-binStep)),bins)
    } 
    else  {
      bins<-seq(target_high,target_low,binStep)
      if (-target<high_p)  bins<-c(bins,seq(-target+binStep,high_p+binStep,binStep))
      if (target>low_p)  bins<-c(rev(seq(target-binStep,low_p-binStep,-binStep)),bins)
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

expected_hist<-function(vals,svals,valType,ylim,histGain,histGainrange){
  
  if (is.null(valType)) valType<-"rs"
  if (is.element(valType,c("ro","ci1","ci2"))) valType<-"rs"
  if (is.element(valType,c("e1","e2","po"))) valType<-"p"
  if (is.element(valType,c("wp","ws"))) valType<-"ws"
  
  switch (valType,
          "rs"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "re"=  { # ns is small
            target<-get_upperEdge(vals,svals)
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "p"=  { # ns is large
            if (braw.env$pPlotScale=="log10") {
              target<-log10(braw.env$alphaSig)
              bins<-getBins(vals,svals,target,ylim[1],log10(1))
            } else {
              target<-braw.env$alphaSig
              bins<-getBins(vals,svals,target,0,1)
              bins<-c(0,bins[bins>0])
            }
          },
          
          "rp"=  { # ns is small
            target<-0.3
            bins<-getBins(vals,svals,target,NULL,NULL,fixed=TRUE)
          },
          
          "log(lrs)"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),0,braw.env$lrRange)
          },
          
          "e1d"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),-braw.env$lrRange,braw.env$lrRange)
          },
          
          "log(lrd)"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),-braw.env$lrRange,braw.env$lrRange)
          },
          
          "e2d"={
            target<-alphaLLR()
            bins<-getBins(vals,svals,target*c(-1,1),-braw.env$lrRange,braw.env$lrRange)
          },
          
          "ws"=  { # ns is small
            target<-get_upperEdge(abs(vals),abs(svals))
            bins<-getBins(vals,svals,target,log10(braw.env$min_p),NULL)
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
            bins<-getBins(vals,svals,target,NULL,braw.env$max_nw)
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

expected_plot<-function(g,pts,showType=NULL,analysis=NULL,IV=NULL,DV=NULL,
                        i=1,scale=1,col="white",orientation="vert",ylim,histGain=NA,histGainrange=NA,npointsMax=200){
  se_arrow<-0.3
  se_size<-0.75
  
  if (!is.null(showType)) {
    if (braw.env$useSignificanceCols){
      c1=braw.env$plotColours$infer_sigC
      c2=braw.env$plotColours$infer_nsigC
    } else {
      c1=braw.env$plotColours$descriptionC
      c2=braw.env$plotColours$descriptionC
    }
    if (showType=="e1") {
      c1=braw.env$plotColours$infer_sigNull
      c2=braw.env$plotColours$infer_nsigNull
    }
    if (showType=="e2") {
      c1=braw.env$plotColours$infer_sigNonNull
      c2=braw.env$plotColours$infer_nsNonNull
    }
    if (showType=="e1d") {
      c1=braw.env$plotColours$infer_sigNull
      c2=braw.env$plotColours$infer_nsdNull
      c3<-braw.env$plotColours$infer_isigNull
    }
    if (showType=="e2d") {
      c1=braw.env$plotColours$infer_sigNonNull
      c2=braw.env$plotColours$infer_nsdNonNull
      c3<-braw.env$plotColours$infer_isigNonNull
    }
  } else {
    c1=col
    c2=col
  }
  
  if (length(pts$y1)<=npointsMax) {
    if (!is.null(analysis) && is.element(showType,c("rs","p")) && length(pts$y1)==1) {
      switch(i,
             {rCI<-analysis$rIVCI
             pCI<-analysis$pIVCI
             if (isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-analysis$rIV2CI
             pCI<-analysis$pIV2CI
             if (isSignificant(braw.env$STMethod,analysis$pIV2,analysis$rIV2,analysis$nval,analysis$df2,analysis$evidence)) {c<-c1} else (c<-c2)
             },
             {rCI<-analysis$rIVIV2CI
             pCI<-analysis$pIVIV2CI
             if (isSignificant(braw.env$STMethod,analysis$pIVIV2DV,analysis$rIVIV2DV,analysis$nval,analysis$df12,analysis$evidence)) {c<-c1} else (c<-c2)
             }
      )
      if (is.null(analysis$hypothesis$IV2)) {
        if (showType=="rs" && !is.null(rCI)){
          x<-pts$x
          if (length(x)<length(rCI)) x<-rep(x,length(rCI))
          pts1se<-data.frame(y=rCI[1,],x=x)
          g<-g+dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size)
        }
        if (showType=="p" && !is.null(analysis$pIVCI)){
          x<-pts$x
          if (length(x)<length(pCI)) x<-rep(x,length(pCI))
          pts1se<-data.frame(y=log10(pCI[1,]),x=pts$x)
          g<-g+dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size)
        }
      }
    }
    
    xr<-makeFiddle(pts$y1,2/40,orientation)*scale*scale
    xr<-xr/max(c(1,abs(xr)))/1.5
    pts$x<-pts$x+xr
    gain<-7/max(7,sqrt(length(xr)))
    colgain<-1-min(1,sqrt(max(0,(length(xr)-50))/200))
    
    if (scale<1) {
      co1<-c1
      co2<-c2
    } else {
      co1<-"black"
      co2<-"black"
    }
    co1<-darken(c1,off=-colgain)
    co2<-darken(c2,off=-colgain)
    dotSize<-braw.env$dotSize*scale*gain
    pts_ns<-pts[!pts$y2,]
    g<-g+dataPoint(data=data.frame(x=pts_ns$x,y=pts_ns$y1),shape=braw.env$plotShapes$study, colour = co2, fill = c2, size = dotSize)
    pts_sig=pts[pts$y2,]
    g<-g+dataPoint(data=data.frame(x=pts_sig$x,y=pts_sig$y1),shape=braw.env$plotShapes$study, colour = co1, fill = c1, size = dotSize)
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        pts_wsig=pts[pts$y3,]
        g<-g+dataPoint(data=data.frame(x=pts_wsig$x,y=pts_wsig$y1),shape=braw.env$plotShapes$study, colour = co1, fill = c3, size = dotSize)
      }
    
  } else { # more than 250 points
    if (is.logical(pts$y2)) {
      hist1<-expected_hist(pts$y1,pts$y1[pts$y2],showType,ylim,histGain,histGainrange)
    } else {
      hist1<-expected_hist(pts$y1,pts$y2,showType,ylim,histGain,histGainrange)
    }
    xoff<-pts$x[1]
    if (orientation=="vert") {
      simAlpha<-1
    } else {
      simAlpha<-1
    }
    g<-g+
      dataPolygon(data=data.frame(y=hist1$x,x=hist1$y1*scale*scale+xoff),colour=NA, fill = c2,alpha=simAlpha)+
      dataPolygon(data=data.frame(y=hist1$x,x=hist1$y2*scale*scale+xoff),colour=NA, fill = c1,alpha=simAlpha)
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        if (is.logical(pts$y3)) {
          hist1<-expected_hist(pts$y1,pts$y1[pts$y3],showType,ylim)
        }
        g<-g+
          dataPolygon(data=data.frame(y=hist1$x,x=hist1$y2+xoff),colour=NA, fill = c3,alpha=simAlpha)
      }
  }
  g
}

r_plot<-function(analysis,showType="rs",logScale=FALSE,otheranalysis=NULL,orientation="vert",effectType="direct",showTheory=TRUE,g=NULL){
  
  npct<-1
  labelSig<-TRUE
  labelNSig<-TRUE
  
  if (showType=="e1a") {
    showType<-"e1"
    labelSig<-FALSE
    labelNSig<-TRUE
  }
  
  if (showType=="e2a") {
    showType<-"e2"
    labelSig<-FALSE
    labelNSig<-TRUE
  }
  
  if (showType=="e1b") {
    showType<-"e1"
    labelSig<-TRUE
    labelNSig<-FALSE
  }
  
  if (showType=="e2b") {
    showType<-"e2"
    labelSig<-TRUE
    labelNSig<-FALSE
  }
  
  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  evidence<-analysis$evidence
  
  histGain<-NA
  histGainrange<-c(NA,NA)
  
  
  r<-effect$rIV
  if (!is.null(hypothesis$IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }
  rlims<-c(-1,1)

  if (braw.env$RZ=="z") {
    r<-atanh(r)
    rlims<-c(-1,1)*braw.env$z_range
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
  
  switch(orientation,
         "horz"={
           xlim<-c(0,max(xoff))+c(0,1)
           orient<-"vert"
         },
         "vert"={
           xlim<-c(0,max(xoff))+c(-1,1)
           orient<-"horz"
         }
  )
  
  yaxis<-plotAxis(showType,effect)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ylines<-yaxis$lines
  
  if (showType=="p" && braw.env$pPlotScale=="log10" && any(is.numeric(analysis$pIV)) && any(analysis$pIV>0)) 
    while (mean(log10(analysis$pIV)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
  
  if (orient=="vert") ylim[2]<-ylim[2]+diff(ylim)/5
  else                ylim<-ylim+c(-1,1)*diff(ylim)/25
  
  box<-"Y" 
  top<-is.element(showType,c("e1","e2","e1d","e2d"))
  
  g<-startPlot(xlim,ylim,box=box,top=top,orientation=orient,g=g)
  g<-g+yAxisTicks(logScale=yaxis$logScale)+yAxisLabel(ylabel)
  if (!is.null(hypothesis$IV2) && effectType=="direct") 
    g<-g+xAxisTicks(breaks=c(0,2,4),c("Main1","Main2","Interaction"))
  if (!is.null(hypothesis$IV2)) 
    g<-g+dataText(data.frame(x=min(xlim),y=max(ylim)),label=effectType,hjust=-0.1,vjust=1,fontface="bold")
  
  if (!all(is.na(analysis$rIV))) {
    data<-collectData(analysis,effectType)
    if (braw.env$RZ=="z") {
      data$rs<-atanh(data$rs)
      data$rp<-atanh(data$rp)
      data$ro<-atanh(data$ro)
    }
    
    switch (showType,
            "rs"={showVals<-data$rs},
            "rp"={showVals<-data$rp},
            "ro"={showVals<-data$ro},
            "re"={showVals<-data$rs-data$rp},
            "p"={showVals<-data$ps},
            "po"={showVals<-data$po},
            "log(lrs)"={showVals<-cbind(res2llr(analysis,"sLLR"))},
            "log(lrd)"={showVals<-cbind(res2llr(analysis,"dLLR"))},
            "e1d"={showVals<-cbind(res2llr(analysis,"dLLR"))},
            "e2d"={showVals<-cbind(res2llr(analysis,"dLLR"))},
            "n"={showVals<-data$ns},
            "ws"={showVals<-rn2w(data$rs,data$ns)},
            "wp"={showVals<-rn2w(data$rp,data$ns)},
            "nw"={showVals<-rw2n(data$rs,0.8,design$Replication$Tails)},
            "ci1"={showVals<-r2ci(data$rs,data$ns,-1)},
            "ci2"={showVals<-r2ci(data$rs,data$ns,+1)},
            "e1"={showVals<-data$ps},
            "e2"={showVals<-data$ps}
    )
    if (logScale) {
      showVals<-log10(showVals)
    }  
  }    
  sigOnly<-evidence$sigOnly
  
  # make theory
  effectTheory<-effect
  if (!effectTheory$world$worldOn) {
    effectTheory$world$worldOn<-TRUE
    effectTheory$world$populationPDF<-"Single"
    effectTheory$world$populationRZ<-"r"
    effectTheory$world$populationPDFk<-effect$rIV
    effectTheory$world$populationNullp<-0
  }
  
  if (!all(is.na(analysis$rIV))) { theoryAlpha<-0.5} else {theoryAlpha<-1}
  
  for (i in 1:length(xoff)){
    if (showTheory) {
      # theory<-doExpectedTheory(showType,logScale,hypothesis,design,i)
      # yv<-theory$yv
      # xd<-theory$xd
      # xdsig<-theory$xdsig
      
      effectTheory<-effect
      if (!effectTheory$world$worldOn) {
        effectTheory$world$worldOn<-TRUE
        effectTheory$world$populationPDF<-"Single"
        effectTheory$world$populationRZ<-"r"
        switch(i,
               effectTheory$world$populationPDFk<-effect$rIV,
               effectTheory$world$populationPDFk<-effect$rIV2,
               effectTheory$world$populationPDFk<-effect$rIVIV2DV
        )
        effectTheory$world$populationNullp<-0
      }
      
      xdsig<-NULL
      xd<-NULL
      
      if (is.element(showType,c("p","e1","e2","po"))) {
        npt<-201
        if (logScale) {
          yv<-seq(0,ylim[1],length.out=npt)
          yvUse<-10^yv
        }else{
          yv<-seq(1,0,length.out=npt)
          yvUse<-yv
        }
        oldEffect<-effectTheory
        if (showType=="e1") effectTheory$world$populationNullp<-1
        if (showType=="e2") effectTheory$world$populationNullp<-0
        xd<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
        xdsig<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
        effectTheory<-oldEffect
      }
      
      if (is.element(showType,c("rs","re","ro","ci1","ci2"))) {
        npt<-101
        if (showType=="re") rOff<-"re"
        else rOff<-"rs"
        if (braw.env$RZ=="z") {
          zvals<-seq(-1,1,length.out=npt*2)*braw.env$z_range*2
          rvals<-tanh(zvals)
          # rvals<-seq(-1,1,length.out=npt)*0.99
          xd<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
          xdsig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
          xd<-rdens2zdens(xd,rvals)
          xdsig<-rdens2zdens(xdsig,rvals)
          yv<-atanh(rvals)
          use<-abs(zvals)<=braw.env$z_range
          yv<-yv[use]
          xd<-xd[use]
          xdsig<-xdsig[use]
        } else {
          rvals<-seq(-1,1,length.out=npt)*0.99
          xd<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
          xdsig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
          yv<-rvals
        }
      }
      
      npt<-101
      switch(showType,
             "rp"={
               if (braw.env$RZ=="z") {
                 yv<-seq(-1,1,length.out=npt)*braw.env$z_range
                 xd<-rPopulationDist(tanh(yv),effectTheory$world)
                 xd<-rdens2zdens(xd,tanh(yv))
               } else {
                 yv<-seq(-1,1,length.out=npt)*0.99
                 xd<-rPopulationDist(yv,effectTheory$world)
               }
             },
             "n"={
               ndist<-getNDist(analysis$design,effectTheory$world,logScale=logScale,sigOnly=TRUE)
               if (logScale) {
                 yv<-log10(ndist$nvals)
               } else {
                 yv<-ndist$nvals
               }
               xd<-ndist$ndens
               xdsig<-ndist$ndensSig
             },
             "ws"={
               yv<-seq(braw.env$alphaSig*1.01,1/1.01,length.out=npt)
               xd<-fullRSamplingDist(yv,effectTheory$world,design,"ws",logScale=logScale,sigOnly=sigOnly)
             },
             "log(lrs)"={
               yv<-seq(0,braw.env$lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrs)",logScale=logScale,sigOnly=sigOnly)
             },
             "log(lrd)"={
               yv<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
             },
             "e1d"={
               yv<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
             },
             "e2d"={
               yv<-seq(-braw.env$lrRange,braw.env$lrRange,length.out=npt)
               xd<-fullRSamplingDist(yv,effectTheory$world,design,"log(lrd)",logScale=logScale,sigOnly=sigOnly)
             },
             "nw"={
               if (logScale) {
                 yv<-seq(log10(5),log10(braw.env$max_nw),length.out=npt)
                 yvUse<-10^yv
               }else{
                 yv<-5+seq(0,braw.env$max_nw,length.out=npt)
                 yvUse<-yv
               }
               xd<-fullRSamplingDist(yvUse,effectTheory$world,design,"nw",logScale=logScale,sigOnly=sigOnly)
               xd<-abs(xd)
             },
             "wp"={
               yv<-seq(braw.env$alphaSig,1/1.01,length.out=npt)
               xd<-fullRSamplingDist(yv,effectTheory$world,design,"wp",logScale=logScale,sigOnly=sigOnly)
             },
             { } # do nothing
      )
      if (is.element(showType,c("p","e1","e2","po"))) {
        if (!labelNSig) {
          xd<-xd-xdsig
          xdsig<-NA
        }
        if (!labelSig) xd<-xdsig
      }
      
      distMax<-0.8

      xd[is.na(xd)]<-0
      theoryGain<-1/max(xd)*distMax
      xd<-xd*theoryGain
      histGain<-abs(sum(xd*c(0,diff(yv))))
      histGainrange<-sort(c(yv[1],yv[length(yv)]))
      ptsp<-data.frame(y=c(yv,rev(yv)),x=c(xd,-rev(xd))+xoff[i])
      if (is.element(showType,c("rs","n","p","e1","e2"))) {
        xdsig<-xdsig*theoryGain
        xdsig[is.na(xdsig)]<-0
        if (!all(xd==xdsig))
          g<-g+dataPolygon(data=ptsp,colour=NA,fill=braw.env$plotColours$infer_nsigC,alpha=theoryAlpha)
        # xdsig[xdsig==0]<-NA
        i2<-0
        while (i2<length(xdsig)) {
          i1<-i2+min(which(c(xdsig[(i2+1):length(xdsig)],1)>0))
          i2<-(i1-1)+min(which(c(xdsig[i1:length(xdsig)],0)==0))
          use<-i1:(i2-1)
          ptsp1<-data.frame(y=c(yv[use],rev(yv[use])),x=c(xdsig[use],-rev(xdsig[use]))+xoff[i])
          g<-g+dataPolygon(data=ptsp1,colour=NA,fill=braw.env$plotColours$infer_sigC,alpha=theoryAlpha)
        }
        # g<-g+dataPath(data=ptsp1,colour="black",linewidth=0.1, orientation=orientation)
        g<-g+dataPath(data=ptsp,colour="black",linewidth=0.1)
      } else {
        g<-g+dataPolygon(data=ptsp,colour=NA,fill="white",alpha=theoryAlpha)
        g<-g+dataPath(data=ptsp,colour="black",linewidth=0.1)
      }
    } else {
      histGain<-NA
    }
    
    # then the samples
    rvals<-c()
    if (!all(is.na(analysis$rIV))) {
      shvals<-showVals[,i]
      rvals<-data$rs[,i]
      pvals<-data$ps[,i]
      nvals<-data$ns
      resSig<-isSignificant(braw.env$STMethod,pvals,rvals,nvals,data$df1,evidence)
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
      if (is.element(showType,c("e1d","e2d"))) {
        d<-res2llr(analysis,braw.env$STMethod)
        err<-(d<0 & data$rp[,i]!=0) | (d>0 & data$rp[,i]==0)
        resWSig<-resSig & err
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y3=resWSig,n<-nvals)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,n<-nvals)
      }
      
      g<-expected_plot(g,pts,showType,analysis,IV,DV,i,orientation=orientation,
                       ylim=ylim,histGain=histGain,histGainrange=histGainrange,npointsMax=200)
      
      ns<-c()
      s<-c()
      if (length(rvals)>1 && is.element(showType,c("p","e1","e2","e1d","e2d"))) {
        n<-length(pvals)
        if (!is.null(otheranalysis) && effect$world$worldOn) n<-n+length(otheranalysis$pIV)
        ns<-sum(!resSig,na.rm=TRUE)/n
        s<-sum(resSig,na.rm=TRUE)/n
        if (is.element(showType,c("e1d","e2d"))) {
          s2<-sum(resSig & shvals<0,na.rm=TRUE)/n
          s1<-sum(resSig & shvals>0,na.rm=TRUE)/n
        }
      }
    }  else {
      # no simulations
      ns<-c()
      switch (showType,
              "p"={
                s<-fullPSig(hypothesis$effect$world,design)
                ns<-1-s
              },
              "e1"={
                Nullp<-hypothesis$effect$world$populationNullp
                hypothesis$effect$world$populationNullp<-1
                s<-fullPSig(hypothesis$effect$world,design)
                hypothesis$effect$world$populationNullp<-Nullp
                ns<-1-s
                if (hypothesis$effect$world$worldOn) {
                  s<-s*hypothesis$effect$world$populationNullp
                  ns<-ns*hypothesis$effect$world$populationNullp
                }
                if (labelSig != labelNSig) {
                  s<-s/fullPSig(hypothesis$effect$world,design)
                  ns<-ns/(1-fullPSig(hypothesis$effect$world,design))
                }
              },
              "e2"={
                Nullp<-hypothesis$effect$world$populationNullp
                hypothesis$effect$world$populationNullp<-0
                s<-fullPSig(hypothesis$effect$world,design)
                hypothesis$effect$world$populationNullp<-Nullp
                ns<-1-s
                if (hypothesis$effect$world$worldOn) {
                  s<-s*(1-hypothesis$effect$world$populationNullp)
                  ns<-ns*(1-hypothesis$effect$world$populationNullp)
                }
                if (labelSig != labelNSig) {
                  s<-s/fullPSig(hypothesis$effect$world,design)
                  ns<-ns/(1-fullPSig(hypothesis$effect$world,design))
                }
              },
              "e1d"={
                ns<-sum(!resSig,na.rm=TRUE)
                s2<-sum(resSig & shvals<0,na.rm=TRUE)
                s1<-sum(resSig & shvals>0,na.rm=TRUE)
              },
              "e2d"={
                ns<-sum(!resSig,na.rm=TRUE)
                s2<-sum(resSig & shvals<0,na.rm=TRUE)
                s1<-sum(resSig & shvals>0,na.rm=TRUE)
              }
      )
    }
    
    lineCol<-"black"
    if (is.element(showType,c("p","e1","e2","e1d","e2d"))) lineCol<-"green"
    for (yl in ylines)
      g<-g+horzLine(intercept=yl,linetype="dotted",colour=lineCol)
    
    if (!isempty(ns) && !isempty(s))
    if (is.element(showType,c("p","e1","e2","e1d","e2d"))) {
      switch (showType,
              "p"={
                labelPt1<-paste0("p(ns) = ",brawFormat(ns*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig) = ",brawFormat(s*100,digits=npct),"% ")
              },
              "e1"={
                labelPt1<-paste0("p(ns correct) = ",brawFormat(ns*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig error) = ",brawFormat(s*100,digits=npct),"% ")
              },
              "e2"={
                labelPt1<-paste0("p(ns error) = ",brawFormat(ns*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig correct) = ",brawFormat(s*100,digits=npct),"% ")
              },
              "e1d"={
                labelPt1b<-paste0("p(ns) = ",brawFormat(ns/n*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig correct) = ",brawFormat(s2/n*100,digits=npct),"% ")
                labelPt1<-paste0("p(sig error) = ",brawFormat(s1/n*100,digits=npct),"% ")
                labelPt1b<-paste0("p(ns) = ",brawFormat(ns/n*100,digits=npct),"%")
                labelPt1a<-paste0("p(sig correct) = ",brawFormat(s2/n*100,digits=npct),"%")
                labelPt1<-paste0("p(sig error) = ",brawFormat(s1/n*100,digits=npct),"%")
              },
              "e2d"={
                labelPt1b<-paste0("p(ns) = ",brawFormat(ns/n*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig error) = ",brawFormat(s2/n*100,digits=npct),"% ")
                labelPt1<-paste0("p(sig correct) = ",brawFormat(s1/n*100,digits=npct),"% ")
                labelPt1b<-paste0("p(ns) = ",brawFormat(ns/n*100,digits=npct),"%")
                labelPt1a<-paste0("p(sig error) = ",brawFormat(s2/n*100,digits=npct),"%")
                labelPt1<-paste0("p(sig correct) = ",brawFormat(s1/n*100,digits=npct),"%")
              }
      )
      lpts1<-data.frame(y = ylim[2], x = xoff[i]+xlim[1])
      if (labelSig && is.null(analysis$hypothesis$IV2))
        g<-g+dataLabel(data=lpts1,label = labelPt1,vjust=1,size=0.9,label.size=0)
      lpts1a<-data.frame(y = ylim[1], x = xoff[i]+xlim[1])
      if (labelNSig)
        g<-g+dataLabel(data=lpts1a,label = labelPt1a,vjust=0,size=0.9,label.size=0)
      if (is.element(showType,c("e1d","e2d"))) {
        lpts1<-data.frame(y = mean(ylim), x = xoff[i]+xlim[1])
        g<-g+dataLabel(data=lpts1,label = labelPt1b,vjust=0.5,size=0.9,label.size=0)
      }
    }
  }
  
  if (length(xoff)>1) {
    if (rem(i,3)==1)
      switch (xoff[i]/2+1,
              {g<-g+annotate("text",x=xoff[i],y=xlim[2]-diff(xlim)/20,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=xlim[2]-diff(xlim)/20,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=xlim[2]-diff(xlim)/20,label="Interaction",color="white",size=3)}
      )
    
    if (effectType=="all") {
      for (i in 1:3) {
        g<-g+horzLine(intercept=(-1+1)*ysc*0.9+(i-1)*ysc*2-1, colour="black", linewidth=1)
        g<-g+horzLine(intercept=(0.0+1)*ysc*0.9+(i-1)*ysc*2-1, linetype="dotted", colour="black", linewidth=0.5)
        g<-g+horzLine(intercept=(1+1)*ysc*0.9+(i-1)*ysc*2-1, colour="black", linewidth=1)
      }
      g<-g+xAxisTicks(breaks=(c(-1,0,1,-1,0,1,-1,0,1)+1)*ysc*0.9+(c(1,1,1,2,2,2,3,3,3)-1)*ysc*2-1,labels=c(-1,0,1,-1,0,1,-1,0,1))
    }
  }
  g
}

l_plot<-function(analysis,ptype=NULL,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,ptype,orientation=orientation,showTheory=showTheory)
  g
}

p_plot<-function(analysis,ptype="p",otheranalysis=NULL,PlotScale=braw.env$pPlotScale,orientation="vert",effectType="direct",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,ptype,PlotScale=="log10",otheranalysis,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)
  g
}

w_plot<-function(analysis,wtype,orientation="vert",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,wtype,braw.env$wPlotScale=="log10",orientation=orientation,showTheory=showTheory,g=g)
  g
}

n_plot<-function(analysis,ntype,orientation="vert",showTheory=TRUE,g=NULL){
  r_plot(analysis,ntype,braw.env$nPlotScale=="log10",orientation=orientation,showTheory=showTheory,g=g)
}

e2_plot<-function(analysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  if (!analysis$hypothesis$effect$world$worldOn) {
    lambda<-brawFormat(analysis$hypothesis$effect$rIV,digits=3)
    switch (braw.env$RZ,
            "r"={
              lab<-bquote(bold("Non-null: " ~ r["p"] ~ "=" ~ .(lambda)))
            },
            "z"={
              lab<-bquote(bold("Non-null: " ~ z["p"] ~ "=" ~ .(lambda)))
            }
    )
  } else {
  distr<-tolower(analysis$hypothesis$effect$world$populationPDF)
  rz<-analysis$hypothesis$effect$world$populationRZ
  lambda<-brawFormat(analysis$hypothesis$effect$world$populationPDFk,digits=3)
  switch (braw.env$RZ,
          "r"={
            lab<-bquote(bold("Non-null: " ~ r["p"] ~ "~" ~ .(distr) (.(rz)/.(lambda))))
          },
          "z"={
            lab<-bquote(bold("Non-null: " ~ z["p"] ~ "~" ~ .(distr) (.(rz)/.(lambda))))
          }
  )
  
  }
  
  switch (braw.env$STMethod,
          "NHST"={
            g<-p_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
            g<-g+plotTitle(lab)
          },
          "sLLR"={
            g<-p_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
            g<-g+plotTitle(lab)
          },
          "dLLR"={
            g<-p_plot(nullanalysis,"e2d",otheranalysis=otheranalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory,g=g)
            g<-g+plotTitle(lab)
          }
  )
  return(g)
}

e1_plot<-function(nullanalysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  switch (braw.env$RZ,
          "r"={
            lab<-bquote(bold("Null: " ~ r["p"] == 0))
          },
          "z"={
            lab<-bquote(bold("Null: " ~ z["p"] == 0))
          }
  )
  switch (braw.env$STMethod,
          "NHST"={
            g<-p_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
            g<-g+plotTitle(lab)
          },
          "sLLR"={
            g<-p_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)+
              g<-g+plotTitle(lab)
          },
          "dLLR"={
            g<-p_plot(nullanalysis,"e1d",otheranalysis=otheranalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory,g=g)
            g<-g+plotTitle(lab)
          }
  )
  return(g)
}

ps_plot<-function(analysis,disp,showTheory=TRUE,g=NULL){
  psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence))
  
  if (is.null(analysis$hypothesis$IV2)) {
    g<-startPlot(xlim=c(-1,1),ylim=c(0,1),top=TRUE,orientation="horz",g=g)
    g<-g+dataBar(data=data.frame(x=0,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4)
    g<-g+xAxisTicks(breaks=c(0),labels=c("DV~IV"))
  } else {
    g<-startPlot(xlim=c(-1,3),ylim=c(0,1),top=TRUE,orientation="horz",g=g)
    g<-g+dataBar(data=data.frame(x=0,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4)
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV2,analysis$rIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-g+dataBar(data=data.frame(x=1,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4)
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIVIV2,analysis$rIVIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-g+dataBar(data=data.frame(x=2,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4)
    
    g<-g+xAxisTicks(breaks=c(0,1,2),labels=c("DV~IV","DV~IV2","DV~IVxIV2"),angle=90)
  }
  g<-g+yAxisLabel("p(sig)")+yAxisTicks()
  return(g)
}


