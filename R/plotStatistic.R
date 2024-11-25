

collectData<-function(analysis,whichEffect) {
  min_p<-10e-6
  use<-(!is.na(analysis$rIV))
  ns<-cbind(analysis$nval[use])
  df1<-cbind(analysis$df1[use])
  rp<-cbind(analysis$rpIV[use])
  ro<-cbind(analysis$roIV[use])
  po<-cbind(analysis$poIV[use])
  
  iv.mn<-cbind(analysis$iv.mn[use])
  iv.sd<-cbind(analysis$iv.sd[use])
  iv.sk<-cbind(analysis$iv.sk[use])
  iv.kt<-cbind(analysis$iv.kt[use])
  
  dv.mn<-cbind(analysis$dv.mn[use])
  dv.sd<-cbind(analysis$dv.sd[use])
  dv.sk<-cbind(analysis$dv.sk[use])
  dv.kt<-cbind(analysis$dv.kt[use])
  
  rd.mn<-cbind(analysis$rd.mn[use])
  rd.sd<-cbind(analysis$rd.sd[use])
  rd.sk<-cbind(analysis$rd.sk[use])
  rd.kt<-cbind(analysis$rd.kt[use])
  
  if (all(is.na(analysis$rIV2))){
    rs<-cbind(analysis$rIV[use])
    ps<-cbind(analysis$pIV[use])
  } else {
    switch (whichEffect,
            "Main 1"={
              column<-1
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "Main 2"={
              column<-2
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "Interaction"={
              column<-3
              rs<-rbind(cbind(analysis$r$direct[use,column],analysis$r$unique[use,column],analysis$r$total[use,column]))
              ps<-rbind(cbind(analysis$p$direct[use,column],analysis$p$unique[use,column],analysis$p$total[use,column]))
            },
            "All"={
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
  ps[ps<min_p]<-min_p
  po[po<min_p]<-min_p
  # if (braw.env$truncate_p) {
  #   ps[ps<braw.env$min_p]<-braw.env$min_p
  #   po[po<braw.env$min_p]<-braw.env$min_p
  # }
  out<-list(rs=rs,ps=ps,ns=ns,df1=df1,rp=rp,ro=ro,po=po,
            iv.mn=iv.mn,iv.sd=iv.sd,iv.sk=iv.sk,iv.kt=iv.kt,
            dv.mn=dv.mn,dv.sd=dv.sd,dv.sk=dv.sk,dv.kt=dv.kt,
            rd.mn=rd.mn,rd.sd=rd.sd,rd.sk=rd.sk,rd.kt=rd.kt)
}

makeFiddle<-function(y,yd,orientation="horiz"){
  yz<-y[1]
  xz<-0
  xd<-0.15
  
  d<-0.05
  if (length(y)>1)
  for (i in 2:length(y)){
    this_y<-reRangeY(y[i])
    this_yz<-reRangeY(yz)
    dy2<-(this_yz-this_y)^2
    this_xz<-reRangeX(xz)
    for (possible_x in seq(0,500,by=0.01)) {
      this_x<-reRangeX(possible_x)
      this_xneg<-reRangeX(-possible_x)
      distances1=sqrt(dy2+(this_xz-this_x)^2)
      distances2=sqrt(dy2+(this_xz-this_xneg)^2)
      # distances1=sqrt((yz-y[i])^2+(xz-possible_x)^2)
      # distances2=sqrt((yz-y[i])^2+(xz- (-possible_x))^2)
      use1<-min(distances1)
      use2<-min(distances2)
      if (all(c(use1,use2)>d)) {
        if (use2>use1) possible_x<- -possible_x
        break
      }
    }
    xz<-c(xz,possible_x)
    yz<-c(yz,y[i])
    
    
    # found<-(abs(reRangeY(yz)-reRangeY(y[i]))<d)
    # if (any(found,na.rm=TRUE)) {
    #   xdiff<-sqrt(d^2-(reRangeY(yz[found])-reRangeY(y[i]))^2)
    #   xpos1<-reRangeX(xz[found])+xdiff
    #   xpos2<-reRangeX(xz[found])-xdiff
    #   if (all(xpos1<reRangeX(0))) usex1<-reRangeX(0) else usex1<-max(xpos1)
    #   if (all(xpos2>reRangeX(0))) usex2<-reRangeX(0) else usex2<-min(xpos2)
    #   if (abs(usex1-reRangeX(0))<abs(usex2-reRangeX(0))) usex<-usex1 else usex<-usex2
    #   usex<-re2RangeX(usex)
    #   xz<-c(xz,usex)
    # } else {
    #   xz<-c(xz,0)
    # }
    # yz<-c(yz,y[i])
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

expected_hist<-function(pts,valType,ylim,histGain,histGainrange){
  
  vals<-pts$y1
  svals<-pts$y1[pts$y2]
  
  if (is.null(valType)) valType<-"rs"
  if (is.element(valType,c("ro","ci1","ci2"))) valType<-"rs"
  if (is.element(valType,c("e1p","e2p","po"))) valType<-"p"
  if (is.element(valType,c("wp","ws"))) valType<-"ws"
  if (is.element(valType,c("iv.mn","iv.sd","iv.sk","iv.kt",
                           "dv.mn","dv.sd","dv.sk","dv.kt",
                           "rd.mn","rd.sd","rd.sk","rd.kt"))) valType<-"mn"
  
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
          },
          
          "mn"=  { # ns is small
            bins<-getBins(vals,NULL,NULL,NULL,NULL,fixed=TRUE)
          },
          
          
  )
  
  use<-vals>=bins[1] & vals<bins[length(bins)]
  
  vals<-pts$y1[use]
  sigs<-pts$y2[use]
  nonnulls<-pts$y0[use]
  if (all(is.na(nonnulls))) nonnulls<-rep(TRUE,length(vals))
  
  vals1<-vals[nonnulls & !sigs]
  dens1<-hist(vals1,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens1<-dens1$counts
  
  vals2<-vals[nonnulls & sigs]
  dens2<-hist(vals2,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens2<-dens2$counts
  
  vals3<-vals[!nonnulls & sigs]
  dens3<-hist(vals3,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens3<-dens3$counts
  
  vals4<-vals[!nonnulls & !sigs]
  dens4<-hist(vals4,breaks=bins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens4<-dens4$counts
  
  dens0<-dens1+dens2+dens3+dens4
  
  if (is.na(histGain)) {
    scale<-0.35/max(dens0)
  } else {
    use<- (bins>=histGainrange[1]) & (bins<=histGainrange[2])
    gain<-sum(dens0[use]*c(0,diff(bins[use])),na.rm=TRUE)
    scale<-histGain/gain
  }
  # x<-(bins[1:(length(bins)-1)]+bins[2:(length(bins))])/2
  return(list(x=bins,h1=dens1*scale,h2=dens2*scale,h3=dens3*scale,h4=dens4*scale))
  # # browser()
  # x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
  # y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
  # y2<-c(0,as.vector(matrix(c(sdens,sdens),2,byrow=TRUE)),0)
  # data.frame(y1=c(-y1,rev(y1)), y2=c(-y2,rev(y2)), x=c(x,rev(x)))
}

expected_plot<-function(g,pts,showType=NULL,analysis=NULL,IV=NULL,DV=NULL,
                        i=1,scale=1,width=1,col="white",orientation="vert",ylim,histGain=NA,histGainrange=NA,npointsMax=200){
  se_arrow<-0.3
  se_size<-0.25
  
  if (!is.null(showType)) {
    if (braw.env$useSignificanceCols){
    if (!all(is.na(pts$y0))){
      c1<-braw.env$plotColours$infer_sigNonNull
      c2<-braw.env$plotColours$infer_nsigNull
      c3<-braw.env$plotColours$infer_sigNull
      c4<-braw.env$plotColours$infer_nsigNonNull
    } else {
      c1<-braw.env$plotColours$infer_sigNonNull
      c2<-braw.env$plotColours$infer_nsigNull
      c3<-braw.env$plotColours$infer_sigNonNull
      c4<-braw.env$plotColours$infer_nsigNull
      pts$y0<-rep(TRUE,length(pts$y2))
    }
    
    if (showType=="e1r") {
      c1=braw.env$plotColours$infer_sigNull
      c2=braw.env$plotColours$infer_nsigNull
    }
    if (showType=="e2r") {
      c1=braw.env$plotColours$infer_sigNonNull
      c2=braw.env$plotColours$infer_nsigNonNull
    }
    if (showType=="e1p") {
      c1=braw.env$plotColours$infer_sigNull
      c2=braw.env$plotColours$infer_nsigNull
    }
    if (showType=="e2p") {
      c1=braw.env$plotColours$infer_sigNonNull
      c2=braw.env$plotColours$infer_nsigNonNull
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
  }
  if (length(pts$y1)<=npointsMax) {
    if (is.logical(pts$y2)) {
      p1<-pts$y1[pts$y2]
      p2<-pts$y1[!pts$y2]
    }
    
    if (!is.null(analysis) && is.element(showType,c("rs","p")) && length(pts$y1)==1) {
      # if (is.null(analysis$hypothesis$IV2)) {
        if (showType=="rs"){
          n<-pts$n
          r<-pts$y1
          rCI<-r2ci(r,n)
          if (pts$y2) c<-c1 else c=c2
          x<-pts$x
          if (length(x)<length(rCI)) x<-rep(x,length(rCI))
          pts1se<-data.frame(y=rCI[1,],x=x)
          g<-addG(g,dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour="white",linewidth=se_size))
        }
        # if (showType=="p"){
        #   browser()
        #   n<-pts$n
        #   p<-pts$y1
        #   r<-p2r(p,n,analysis$df[1])
        #   rCI<-r2ci(r,n)
        #   pCI<-r2p(rCI,n,analysis$df[1])
        #   if (pts$y2) c<-c1 else c=c2
        #   
        #   x<-pts$x
        #   if (length(x)<length(pCI)) x<-rep(x,length(pCI))
        #   pts1se<-data.frame(y=log10(pCI[1,]),x=pts$x)
        #   g<-addG(g,dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=c,linewidth=se_size))
        # }
      # }
    }
    
    xr<-makeFiddle(pts$y1,2/40/braw.env$plotArea[4],orientation)
    if (max(abs(xr))>0) xr<-xr/max(abs(xr))*0.4
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
    pts_sigNonNull=pts[pts$y2 & pts$y0,]
    pts_nsNonNull=pts[!pts$y2 & pts$y0,]
    pts_sigNull=pts[pts$y2 & !pts$y0,]
    pts_nsNull<-pts[!pts$y2 & !pts$y0,]
    
    g<-addG(g,dataPoint(data=data.frame(x=pts_sigNonNull$x,y=pts_sigNonNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c1,off=-colgain), fill = c1, size = dotSize))
    g<-addG(g,dataPoint(data=data.frame(x=pts_nsNonNull$x,y=pts_nsNonNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c4,off=-colgain), fill = c4, size = dotSize))
    g<-addG(g,dataPoint(data=data.frame(x=pts_sigNull$x,y=pts_sigNull$y1),shape=braw.env$plotShapes$study, 
                        colour = c3, fill = c3, size = dotSize))
    g<-addG(g,dataPoint(data=data.frame(x=pts_nsNull$x,y=pts_nsNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c2,off=-colgain), fill = c2, size = dotSize))
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        pts_wsig=pts[pts$y3,]
        g<-addG(g,dataPoint(data=data.frame(x=pts_wsig$x,y=pts_wsig$y1),shape=braw.env$plotShapes$study, colour = co1, fill = c3, size = dotSize))
      }
  } else { # more than 250 points
    if (is.logical(pts$y2)) {
      hists<-expected_hist(pts,showType,ylim,histGain,histGainrange)
    } else {
      hists<-expected_hist(pts,showType,ylim,histGain,histGainrange)
    }
    xoff<-pts$x[1]
    if (orientation=="vert") {
      simAlpha<-0.85
    } else {
      simAlpha<-0.85
    }
    dx<-diff(hists$x[1:2])
    # c1<-braw.env$plotColours$infer_nsigNonNull
    # c2<-braw.env$plotColours$infer_sigNonNull
    # c3<-braw.env$plotColours$infer_sigNull
    # c4<-braw.env$plotColours$infer_nsigNull
    # if (1==1) {
    #   c1<-c4
    #   c3<-c2
    # }
    cols<-c(c4,c1,c3,c2)
    
    for (i in 1:length(hists$h1)) {
      dens<-c(hists$h1[i],hists$h2[i],hists$h3[i],hists$h4[i])
      # dens<-cumsum(dens)
      use<-order(dens)
      ystart<-0
      for (j in use) {
        if (dens[j]>0) {
          data<-data.frame(y=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                           x=c(-dens[j],-ystart,-ystart,-dens[j])+xoff)
          g<-addG(g,dataPolygon(data=data,
                                colour=NA, fill = cols[j],alpha=simAlpha))
        data<-data.frame(y=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                         x=c(dens[j],ystart,ystart,dens[j])+xoff)
        g<-addG(g,dataPolygon(data=data,
                              colour=NA, fill = cols[j],alpha=simAlpha))
        ystart<-dens[j]
        }
      }
    }
    # g<-addG(g,
    #   dataPolygon(data=data.frame(y=hist1$x,x=hist1$y1+xoff),colour=NA, fill = c2,alpha=simAlpha),
    #   dataPolygon(data=data.frame(y=hist1$x,x=hist1$y2+xoff),colour=NA, fill = c1,alpha=simAlpha)
    # )
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        if (is.logical(pts$y3)) {
          hist1<-expected_hist(pts,showType,ylim)
        }
        g<-addG(g,
          dataPolygon(data=data.frame(y=hist1$x,x=hist1$y2+xoff),colour=NA, fill = c3,alpha=simAlpha))
      }
  }
  g
}

r_plot<-function(analysis,showType="rs",logScale=FALSE,otheranalysis=NULL,
                 orientation="vert",whichEffect="Main 1",effectType="all",showTheory=TRUE,g=NULL){

  npct<-1
  showSig<-TRUE
  labelSig<-TRUE
  labelNSig<-TRUE
  top<-FALSE
  
  if (is.element(showType,c("iv.mn","iv.sd","iv.sk","iv.kt",
                            "dv.mn","dv.sd","dv.sk","dv.kt",
                            "rd.mn","rd.sd","rd.sk","rd.kt"))) {
    # showSig<-FALSE
    labelSig<-FALSE
    labelNSig<-FALSE
  }

  useWorldCols<-FALSE
  if (showType=="rse") {
    showType<-"rs"
    useWorldCols<-TRUE
  }
  if (showType=="e1p") {
    labelSig<-TRUE
    labelNSig<-TRUE
    top<-TRUE
  }
  
  if (showType=="e2p") {
    labelSig<-TRUE
    labelNSig<-TRUE
    top<-TRUE
  }
  
  if (showType=="e1r") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-TRUE
    top<-TRUE
  }
  
  if (showType=="e2r") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-TRUE
    top<-TRUE
  }
  
  if (showType=="e1a") {
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-TRUE
    top<-TRUE
  }
  
  if (showType=="e2a") {
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-TRUE
    top<-TRUE
  }
  
  if (showType=="e1b") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-FALSE
    top<-TRUE
  }
  
  if (showType=="e2b") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-FALSE
    top<-TRUE
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
  
  xoff=0
  if (!is.null(hypothesis$IV2) && effectType=="all") 
    xoff=c(0,2,4)

  switch(orientation,
         "horz"={
           xlim<-c(0,max(xoff))+c(0,1)
           orient<-"vert"
         },
         "vert"={
           xlim<-c(0,max(xoff))+c(-1,1)/2
           orient<-"horz"
         }
  )
  
  yaxis<-plotAxis(showType,hypothesis,design)
  ylim<-yaxis$lim
  ylabel<-yaxis$label
  ylines<-yaxis$lines
  if ((showType=="rs") && (!is.null(hypothesis$IV2))) 
    switch(whichEffect,"Main 1"=ylabel<-"Main 1",
                       "Main 2"=ylabel<-"Main 2",
                       "Interaction"=ylabel<-"Interaction"
           )
  
  
  if (showType=="p" && braw.env$pPlotScale=="log10" && any(is.numeric(analysis$pIV)) && any(analysis$pIV>0)) 
    while (mean(log10(analysis$pIV)>ylim[1])<0.75) ylim[1]<-ylim[1]-1
  
  if (orient=="vert") ylim[2]<-ylim[2]+diff(ylim)/5
  else                ylim<-ylim+c(-1,1)*diff(ylim)/25
  
  box<-"Y" 
  if (!is.null(hypothesis$IV2) && effectType=="all") 
    xticks<-makeTicks(breaks=c(0,2,4),c("direct","unique","total"))
  else
    xticks<-NULL
  
  g<-startPlot(xlim,ylim,
               xticks=xticks,xmax=TRUE,
               yticks=makeTicks(logScale=yaxis$logScale),ylabel=makeLabel(ylabel),
               box=box,top=top,orientation=orient,g=g)
  # g<-addG(g,yAxisTicks(logScale=yaxis$logScale),yAxisLabel(ylabel))
    # g<-addG(g,xAxisTicks(breaks=c(0,2,4),c("direct","unique","total")))
 
  if (!all(is.na(analysis$rIV))) {
    data<-collectData(analysis,whichEffect)
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
            "e1r"={showVals<-data$rs},
            "e2r"={showVals<-data$rs},
            "e1p"={showVals<-data$ps},
            "e2p"={showVals<-data$ps},
            "iv.mn"=showVals<-data$iv.mn,
            "iv.sd"=showVals<-data$iv.sd,
            "iv.sk"=showVals<-data$iv.sk,
            "iv.kt"=showVals<-data$iv.kt,
            "dv.mn"=showVals<-data$dv.mn,
            "dv.sd"=showVals<-data$dv.sd,
            "dv.sk"=showVals<-data$dv.sk,
            "dv.kt"=showVals<-data$dv.kt,
            "rd.mn"=showVals<-data$rd.mn,
            "rd.sd"=showVals<-data$rd.sd,
            "rd.sk"=showVals<-data$rd.sk,
            "rd.kt"=showVals<-data$rd.kt,
    )
    if (logScale) {
      showVals<-log10(showVals)
    }  
  }    
  sigOnly<-evidence$sigOnly
  
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
        switch(whichEffect,
               "Main 1"=effectTheory$world$populationPDFk<-effect$rIV,
               "Main 2"=effectTheory$world$populationPDFk<-effect$rIV2,
               "Interaction"=effectTheory$world$populationPDFk<-effect$rIVIV2DV
        )
        effectTheory$world$populationNullp<-0
      }
      
      xdsig<-NULL
      xd<-NULL
      histGain<-NA
      
      if (is.element(showType,c("p","e1p","e2p","po"))) {
        npt<-201
        if (logScale) {
          yv<-seq(0,ylim[1],length.out=npt)
          yvUse<-10^yv
        }else{
          yv<-seq(1,0,length.out=npt)
          yvUse<-yv
        }
        oldEffect<-effectTheory
        if (showType=="e1p") effectTheory$world$populationNullp<-1
        if (showType=="e2p") effectTheory$world$populationNullp<-0
        xd<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
        xdsig<-fullRSamplingDist(yvUse,effectTheory$world,design,"p",logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
        effectTheory<-oldEffect
      }
      
      if (is.element(showType,c("rs","re","ro","ci1","ci2","e1r","e2r"))) {
        npt<-101
        if (showType=="e1r") effectTheory$world$populationNullp<-1
        if (showType=="e2r") effectTheory$world$populationNullp<-0
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
          if (!design$sNRand) {
            cr<-tanh(pn2z(braw.env$alphaSig,design$sN))
            inc<-cr/ceiling(cr/(2/npt))
            rvals<-seq(inc,0.99,inc)
            rvals<-c(-rev(rvals),0,rvals)
          } else rvals<-seq(-1,1,length.out=npt)*0.99
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
             "iv.mn"={
               var<-hypothesis$IV
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               xd<-dnorm(yv,var$mu,var$sd/sqrt(n))
             },
             "iv.sd"={
               var<-hypothesis$IV
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               yvuse<-yv/var$sd
               # xd<-exp(-n/2*yvuse^2+(n-2)*log(yv))
               # xd<-exp(-n/2*yvuse^2+n*log(yv)-n*2/n*log(yv))
               # xd<-exp(n*(-1/2*yvuse^2+log(yv)-2/n*log(yv)))
               # xd<-exp(n*(-1/2*yvuse^2+log(yv)-2/n*log(yv)))
               xd1<-exp(-1/2*yvuse^2+log(yv)-2/n*log(yv))
               xd1<-xd1/max(xd1)
               xd<-xd1^n
               if (max(xd)==0) xd<-NULL # for n>1000 because of underflow
             },
             "iv.sk"={
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               sksd<-sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
               xd<-dnorm(yv,0,sksd)
             },
             "iv.kt"={
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               ktsd<-sqrt(24*n*(n-2)*(n-3)/(n+1)^2/(n+3)/(n+5))
               xd<-dnorm(yv,0,ktsd)
             },
             "dv.mn"={
               var<-hypothesis$DV
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               xd<-dnorm(yv,var$mu,var$sd/sqrt(n))
             },
             "dv.sd"={
               var<-hypothesis$DV
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               yvuse<-yv/var$sd
               # xd<-exp(-n/2*yvuse^2+(n-2)*log(yv))
               # xd<-exp(-n/2*yvuse^2+n*log(yv)-n*2/n*log(yv))
               # xd<-exp(n*(-1/2*yvuse^2+log(yv)-2/n*log(yv)))
               # xd<-exp(n*(-1/2*yvuse^2+log(yv)-2/n*log(yv)))
               xd1<-exp(-1/2*yvuse^2+log(yv)-2/n*log(yv))
               xd1<-xd1/max(xd1)
               xd<-xd1^n
               if (max(xd)==0) xd<-NULL # for n>1000 because of underflow
             },
             "dv.sk"={
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               sksd<-sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
               xd<-dnorm(yv,0,sksd)
             },
             "dv.kt"={
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               ktsd<-sqrt(24*n*(n-2)*(n-3)/(n+1)^2/(n+3)/(n+5))
               xd<-dnorm(yv,0,ktsd)
             },
             "rd.mn"={
               var<-hypothesis$DV
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               mnsd<-1/sqrt(n)*var$sd*sqrt(1-hypothesis$effect$rIV^2)
               xd<-dnorm(yv,0,mnsd)
             },
             "rd.sd"={
               var<-hypothesis$DV
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               sdsd<-var$sd*sqrt(1-hypothesis$effect$rIV^2)
               yvuse<-yv/sdsd
               # xd<-exp(-n/2*yvuse^2+(n-2)*log(yv))
               # xd<-exp(-n/2*yvuse^2+n*log(yv)-n*2/n*log(yv))
               # xd<-exp(n*(-1/2*yvuse^2+log(yv)-2/n*log(yv)))
               # xd<-exp(n*(-1/2*yvuse^2+log(yv)-2/n*log(yv)))
               xd1<-exp(-1/2*yvuse^2+log(yv)-2/n*log(yv))
               xd1<-xd1/max(xd1)
               xd<-xd1^n
               if (max(xd)==0) xd<-NULL # for n>1000 because of underflow
             },
             "rd.sk"={
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               sksd<-sqrt(6*n*(n-1)/(n-2)/(n+1)/(n+3))
               xd<-dnorm(yv,0,sksd)
             },
             "rd.kt"={
               n<-design$sN
               yv<-seq(ylim[1],ylim[2],length.out=npt)
               ktsd<-sqrt(24*n*(n-2)*(n-3)/(n+1)^2/(n+3)/(n+5))
               xd<-dnorm(yv,0,ktsd)
             },
             { } # do nothing
      )
      if (is.element(showType,c("p","e1p","e2p","po"))) {
        if (!labelNSig) {
          xd<-xd-xdsig
          xdsig<-NA
        }
        if (!labelSig) xd<-xdsig
      }
      
      if (!is.null(xd)) {
      distMax<-0.8/2

      xd[is.na(xd)]<-0
      theoryGain<-1/max(xd)*distMax
      xd<-xd*theoryGain
      histGain<-abs(sum(xd*c(0,diff(yv))))
      histGainrange<-sort(c(yv[1],yv[length(yv)]))
      ptsp<-data.frame(y=c(yv,rev(yv)),x=c(xd,-rev(xd))+xoff[i])
      if (is.element(showType,c("rs","n","p","e1r","e2r","e1p","e2p"))) {
        xdsig<-xdsig*theoryGain
        xdsig[is.na(xdsig)]<-0
        if (!all(xd==xdsig))
          g<-addG(g,dataPolygon(data=ptsp,colour=NA,fill=braw.env$plotColours$infer_nsigC,alpha=theoryAlpha))
        # xdsig[xdsig==0]<-NA
        i2<-0
        while (i2<length(xdsig)) {
          i1<-i2+min(which(c(xdsig[(i2+1):length(xdsig)],1)>0))
          i2<-(i1-1)+min(which(c(xdsig[i1:length(xdsig)],0)==0))
          use<-i1:(i2-1)
          ptsp1<-data.frame(y=c(yv[use],rev(yv[use])),x=c(xdsig[use],-rev(xdsig[use]))+xoff[i])
          g<-addG(g,dataPolygon(data=ptsp1,colour=NA,fill=braw.env$plotColours$infer_sigC,alpha=theoryAlpha))
        }
        # g<-addG(g,dataPath(data=ptsp1,colour="black",linewidth=0.1, orientation=orientation))
        g<-addG(g,dataPath(data=ptsp,colour="black",linewidth=0.1))
      } else {
        g<-addG(g,dataPolygon(data=ptsp,colour=NA,fill="white",alpha=theoryAlpha))
        g<-addG(g,dataPath(data=ptsp,colour="black",linewidth=0.1))
      }
      } 
    } 
    
    # then the samples
    rvals<-c()
    if (!all(is.na(analysis$rIV))) {
      shvals<-showVals[,i]
      rvals<-data$rs[,i]
      pvals<-data$ps[,i]
      nvals<-data$ns
      if (showSig)
        resSig<-isSignificant(braw.env$STMethod,pvals,rvals,nvals,data$df1,evidence)
      else
        resSig<-rep(FALSE,length(rvals))
      if (useWorldCols && ((hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp>0)
          || (!all(data$rp[,i]!=0) && !all(data$rp[,i]==0))))
        resNull<-data$rp[,i]!=0
      else
        resNull<-rep(NA,length(data$rp[,i]))
      
      if (sigOnly) {
        shvals<-shvals[resSig]
        rvals<-rvals[resSig]
        pvals<-pvals[resSig]
        nvals<-nvals[resSig]
        resSig<-resSig[resSig]
        resNull<-resNull[resSig]
      }
      if (effectType=="all") {
        ysc<-1/3
        rvals<-(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1
      }
      if (is.element(showType,c("e1d","e2d"))) {
        d<-res2llr(analysis,braw.env$STMethod)
        err<-(d<0 & data$rp[,i]!=0) | (d>0 & data$rp[,i]==0)
        resWSig<-resSig & err
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y3=resWSig,y0=resNull,n=nvals)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y0=resNull,n=nvals)
      }
      
      g<-expected_plot(g,pts,showType,analysis,IV,DV,i,orientation=orientation,
                       ylim=ylim,histGain=histGain,histGainrange=histGainrange,npointsMax=200)
      
      ns<-c()
      s<-c()
      if (length(rvals)>1 && is.element(showType,c("rs","p","e1r","e2r","e1p","e2p","e1d","e2d"))) {
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
              "e1p"={
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
              "e2p"={
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
    if (is.element(showType,c("p","e1p","e2p","e1d","e2d"))) lineCol<-"green"
    for (yl in ylines)
      g<-addG(g,horzLine(intercept=yl,linetype="dotted",colour=lineCol))
    
    if (!isempty(ns) && !isempty(s))
    if (is.element(showType,c("rs","p","e1r","e2r","e1p","e2p","e1d","e2d"))) {
      switch (showType,
              "rs"={
                if (!is.null(hypothesis$IV2) && effectType=="all") {
                  labelPt1<-paste0(brawFormat(ns*100,digits=npct),"% ")
                  labelPt1a<-paste0(brawFormat(s*100,digits=npct),"% ")
                } else {
                  labelPt1<-paste0("p(ns) = ",brawFormat(ns*100,digits=npct),"% ")
                  labelPt1a<-paste0("p(sig) = ",brawFormat(s*100,digits=npct),"% ")
                }
              },
              "p"={
                if (!is.null(hypothesis$IV2) && effectType=="all") {
                  labelPt1<-paste0(brawFormat(ns*100,digits=npct),"% ")
                  labelPt1a<-paste0(brawFormat(s*100,digits=npct),"% ")
                } else {
                  labelPt1<-paste0("p(ns) = ",brawFormat(ns*100,digits=npct),"% ")
                  labelPt1a<-paste0("p(sig) = ",brawFormat(s*100,digits=npct),"% ")
                }
              },
              "e1r"={
                labelPt1<-paste0("p(ns correct) = ",brawFormat(ns*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig error) = ",brawFormat(s*100,digits=npct),"% ")
              },
              "e2r"={
                labelPt1<-paste0("p(ns error) = ",brawFormat(ns*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig correct) = ",brawFormat(s*100,digits=npct),"% ")
              },
              "e1p"={
                labelPt1<-paste0("p(ns correct) = ",brawFormat(ns*100,digits=npct),"% ")
                labelPt1a<-paste0("p(sig error) = ",brawFormat(s*100,digits=npct),"% ")
              },
              "e2p"={
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
      lpts1<-data.frame(y = ylim[2]-diff(ylim)/50, x = xoff[i]+xlim[2])
      if (labelSig && is.null(analysis$hypothesis$IV2))
        g<-addG(g,dataLabel(data=lpts1,label = labelPt1,hjust=1,vjust=0.5,size=0.9,label.size=0))
      lpts1a<-data.frame(y = ylim[1]+diff(ylim)/50, x = xoff[i]+xlim[2])
      if (labelNSig)
        g<-addG(g,dataLabel(data=lpts1a,label = labelPt1a,hjust=1,vjust=0.5,size=0.9,label.size=0))
      if (is.element(showType,c("e1d","e2d"))) {
        lpts1<-data.frame(y = mean(ylim), x = xoff[i]+xlim[2])
        g<-addG(g,dataLabel(data=lpts1,label = labelPt1b,hjust=1,vjust=0.5,size=0.9,label.size=0))
      }
    }
  }
  
  g
}

l_plot<-function(analysis,ptype=NULL,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,ptype,orientation=orientation,showTheory=showTheory)
  g
}

p_plot<-function(analysis,ptype="p",otheranalysis=NULL,PlotScale=braw.env$pPlotScale,orientation="vert",
                 whichEffect="Main 1",effectType="all",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,ptype,PlotScale=="log10",otheranalysis,orientation=orientation,
            whichEffect=whichEffect,effectType=effectType,showTheory=showTheory,g=g)
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
              lab<-paste0("NonNull: r[p]~ ",lambda)
            },
            "z"={
              lab<-paste0("NonNull: z[p]~ ",lambda)
            }
    )
  } else {
  distr<-tolower(analysis$hypothesis$effect$world$populationPDF)
  rz<-analysis$hypothesis$effect$world$populationRZ
  lambda<-brawFormat(analysis$hypothesis$effect$world$populationPDFk,digits=3)
  switch (braw.env$RZ,
          "r"={
            lab<-paste0("NonNull: r[p]~ ",distr,"(",rz,"/",lambda,")")
          },
          "z"={
            lab<-paste0("NonNull: z[p]~ ",distr,"(",rz,"/",lambda,")")
          }
  )
  
  }
  
  switch (braw.env$STMethod,
          "NHST"={
            analysis$hypothesis$effect$world$populationNullp<-0
            switch(disp,
                   "e2r"={
                     g<-r_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
                   },
                   "e2p"={
                     g<-p_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
                   }
            )
            g<-addG(g,plotTitle(lab))
          },
          "sLLR"={
            g<-p_plot(analysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
            g<-addG(g,plotTitle(lab))
          },
          "dLLR"={
            g<-p_plot(nullanalysis,"e2d",otheranalysis=otheranalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory,g=g)
            g<-addG(g,plotTitle(lab))
          }
  )
  return(g)
}

e1_plot<-function(nullanalysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  switch (braw.env$RZ,
          "r"={
            lab<-"Null: r[p]= 0"
          },
          "z"={
            lab<-"Null: z[p]= 0"
          }
  )
  switch (braw.env$STMethod,
          "NHST"={
            nullanalysis$hypothesis$effect$world$populationNullp<-1
            # g<-r_plot(nullanalysis,"rs_e1",otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
            switch(disp,
                   "e1r"={
                     g<-r_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
                   },
                   "e1a"={
                     g<-p_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)
                   }
            )
            g<-addG(g,plotTitle(lab))
          },
          "sLLR"={
            g<-p_plot(nullanalysis,disp,otheranalysis=otheranalysis,orientation=orientation,showTheory=showTheory,g=g)+
              g<-addG(g,plotTitle(lab))
          },
          "dLLR"={
            g<-p_plot(nullanalysis,"e1d",otheranalysis=otheranalysis,PlotScale="linear",orientation=orientation,showTheory=showTheory,g=g)
            g<-addG(g,plotTitle(lab))
          }
  )
  return(g)
}

ps_plot<-function(analysis,disp,showTheory=TRUE,g=NULL){
  
  if (is.null(analysis$hypothesis$IV2)) {
    sigs<-isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence)
    nulls<-analysis$rp==0
    if ((all(nulls) || all(!nulls)) && disp=="ps") {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("p(sig)"),
                   top=FALSE,orientation="horz",g=g)
      g<-addG(g,dataBar(data=data.frame(x=0,y=mean(sigs)),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    } else {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("Outcomes"),
                   top=FALSE,orientation="horz",g=g)
      y<-1
      col0<-braw.env$plotColours$infer_nsigNonNull
      col2<-braw.env$plotColours$infer_sigNonNull
      col3<-braw.env$plotColours$infer_sigNull
      col5<-braw.env$plotColours$infer_nsigNull
      lb0<-braw.env$nonNullSig
      lb2<-braw.env$nonNullNS
      lb3<-braw.env$nullNS
      lb5<-braw.env$nullSig
      
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col0,barwidth=0.4))
      g<-addG(g,drawNHSTLabel(lb0,data.frame(x=1,y=y-mean(!nulls & !sigs)/2),0,col0,vjust=0.5))
      y<-y-mean(!nulls & !sigs)
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col2,barwidth=0.4))
      g<-addG(g,drawNHSTLabel(lb2,data.frame(x=1,y=y-mean(!nulls & sigs)/2),0,col2,vjust=0.5))
      y<-y-mean(!nulls & sigs)
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col3,barwidth=0.4))
      g<-addG(g,drawNHSTLabel(lb3,data.frame(x=1,y=y-mean(nulls & sigs)/2),0,col3,vjust=0.5))
      y<-y-mean(nulls & sigs)
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col5,barwidth=0.4))
      g<-addG(g,drawNHSTLabel(lb5,data.frame(x=1,y=mean(nulls & !sigs)/2),0,col5,vjust=0.5))

      
    }
  } else {
    g<-startPlot(xlim=c(-1,3),ylim=c(0,1),
                 xticks=makeTicks(breaks=c(0,1,2),labels=c("DV~IV","DV~IV2","DV~IVxIV2")),
                 yticks=makeTicks(),ylabel=makeLabel("p(sig)"),
                 top=FALSE,orientation="horz",g=g)
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=0,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV2,analysis$rIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=1,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIVIV2,analysis$rIVIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=2,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
  }
  return(g)
}

var_plot<-function(analysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,showType=disp,showTheory=showTheory,g=g)
}
