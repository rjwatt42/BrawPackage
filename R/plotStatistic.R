

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
  # ps[ps<min_p]<-min_p
  # po[po<min_p]<-min_p
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
  d<-0.05
  d2<-d^2
  yG<-(braw.env$plotArea[4]-braw.env$plotLimits$gap[4]-braw.env$plotLimits$gap[2])/diff(braw.env$plotLimits$ysc)
  rY<-function(y) y*yG
  
  xG<-(braw.env$plotArea[3]-braw.env$plotLimits$gap[3]-braw.env$plotLimits$gap[1])/diff(braw.env$plotLimits$xsc)
  rX<-function(x) x*xG
  
  possible_xs<-seq(0,500,by=0.01)
  
  yz<-y[1]
  yzR<-rY(yz)
  xz<-0
  xzR<-rX(xz)
  if (length(y)>1)
  for (i in 2:length(y)){
    this_y<-rY(y[i])
    dy2<-(yzR-this_y)^2
    for (possible_x in possible_xs) {
      this_x<-rX(possible_x)
      this_xneg<- -this_x
      distances1=dy2+(xzR-this_x)^2
      distances2=dy2+(xzR-this_xneg)^2
      use1<-min(distances1)
      use2<-min(distances2)
      if (all(c(use1,use2)>d2)) {
        if (use2>use1) possible_x<- -possible_x
        break
      }
    }
    xz<-c(xz,possible_x)
    xzR<-c(xzR,rX(possible_x))
    yz<-c(yz,y[i])
    yzR<-c(yzR,this_y)
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
  if (is.element(valType,c("rse","rss","ro","ci1","ci2","llknull"))) valType<-"rs"
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
          {bins<-getBins(vals,NULL,NULL,NULL,NULL,fixed=TRUE)}
          
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
                        i=1,scale=1,width=1,col="white",alpha=1,useSignificanceCols=braw.env$useSignificanceCols,
                        histStyle="width",orientation="vert",ylim,histGain=NA,histGainrange=NA,npointsMax=200){
  se_arrow<-0.3
  se_size<-0.25
  
  c1=col
  c2=col
  c3=col
  c4=col
  
  if (!is.null(showType)) {
    if (useSignificanceCols){
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
    }
  }
  if (length(pts$y1)<=npointsMax) {
    if (is.logical(pts$y2)) {
      p1<-pts$y1[pts$y2]
      p2<-pts$y1[!pts$y2]
    }
    
    if (!is.null(analysis) && is.element(showType,c("rs","rse","rss","p")) && length(pts$y1)==1) {
      # if (is.null(analysis$hypothesis$IV2)) {
        if (is.element(showType,c("rs","rse","rss"))){
          n<-pts$n
          r<-pts$y1
          rCI<-r2ci(r,n)
          if (pts$y2) c<-c1 else c=c2
          x<-pts$x
          if (length(x)<length(rCI)) x<-rep(x,length(rCI))
          pts1se<-data.frame(y=rCI[1,],x=x)
          g<-addG(g,dataLine(data=pts1se,arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour="white",linewidth=se_size))
        }
    }
    
    xr<-makeFiddle(pts$y1,2/40/braw.env$plotArea[4],orientation)
    if (max(abs(xr))>0) xr<-xr*0.35/max(abs(xr))
    pts$x<-pts$x+xr*sum(width)*0.3/0.35
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
    if (useSignificanceCols) {
    pts_sigNonNull=pts[pts$y2 & pts$y0,]
    pts_nsNonNull=pts[!pts$y2 & pts$y0,]
    pts_sigNull=pts[pts$y2 & !pts$y0,]
    pts_nsNull<-pts[!pts$y2 & !pts$y0,]
    
    g<-addG(g,dataPoint(data=data.frame(x=pts_sigNonNull$x,y=pts_sigNonNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c1,off=-colgain), alpha=alpha, fill = c1, size = dotSize))
    g<-addG(g,dataPoint(data=data.frame(x=pts_nsNonNull$x,y=pts_nsNonNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c4,off=-colgain), alpha=alpha, fill = c4, size = dotSize))
    g<-addG(g,dataPoint(data=data.frame(x=pts_sigNull$x,y=pts_sigNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c3,off=-colgain), alpha=alpha, fill = c3, size = dotSize))
    g<-addG(g,dataPoint(data=data.frame(x=pts_nsNull$x,y=pts_nsNull$y1),shape=braw.env$plotShapes$study, 
                        colour = darken(c2,off=-colgain), alpha=alpha, fill = c2, size = dotSize))
    } else {
      g<-addG(g,dataPoint(data=data.frame(x=pts$x,y=pts$y1),shape=braw.env$plotShapes$study, 
                          colour = darken(c1,off=-colgain), alpha=alpha, fill = c1, size = dotSize))
    }
    if (!is.null(showType))
      if (is.element(showType,c("e1d","e2d"))) {
        pts_wsig=pts[pts$y3,]
        g<-addG(g,dataPoint(data=data.frame(x=pts_wsig$x,y=pts_wsig$y1),shape=braw.env$plotShapes$study, colour = co1, alpha=alpha, fill = c3, size = dotSize))
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
    
    if (length(width)==1) width=c(width,width)
    for (i in 1:length(hists$h1)) {
      dens<-c(hists$h1[i],hists$h2[i],hists$h3[i],hists$h4[i])
      # dens<-cumsum(dens)
      use<-order(dens)
      use<-rev(1:length(dens))
      ystart<-0
      for (j in use) {
        if (dens[j]>0) {
          if (histStyle=="width") {
            alpha<-simAlpha
            w<-dens[j]
          } else {
            alpha<-(dens[j]/0.35)^0.6
            w<-0.5/0.35
          }
          data<-data.frame(y=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                           x=(c(-w,0,0,-w)-ystart)*width[1]+xoff)
          g<-addG(g,dataPolygon(data=data,
                                colour=NA, fill = cols[j],alpha=alpha))
        data<-data.frame(y=c(hists$x[i],hists$x[i],hists$x[i+1],hists$x[i+1]),
                         x=(c(w,0,0,w)+ystart)*width[2]+xoff)
        g<-addG(g,dataPolygon(data=data,
                              colour=NA, fill = cols[j],alpha=alpha))
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
  top<-1
  if (is.element(showType,c("e1d","e2d"))) top<-1.5
  if (is.element(showType,c("rse","rss"))) top<-1.5
  
  if (is.element(showType,c("iv.mn","iv.sd","iv.sk","iv.kt",
                            "dv.mn","dv.sd","dv.sk","dv.kt",
                            "rd.mn","rd.sd","rd.sk","rd.kt"))) {
    # showSig<-FALSE
    labelSig<-FALSE
    labelNSig<-FALSE
  }

  if (showType=="e1p") {
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e2p") {
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e1r") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e2r") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e1a") {
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e2a") {
    # showType<-"rs"
    labelSig<-FALSE
    labelNSig<-TRUE
    # top<-TRUE
  }
  
  if (showType=="e1b") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-FALSE
    # top<-TRUE
  }
  
  if (showType=="e2b") {
    # showType<-"rs"
    labelSig<-TRUE
    labelNSig<-FALSE
    # top<-TRUE
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

  switch(braw.env$RZ,
         "r"={},
         "z"={
           r<-atanh(r)
           rlims<-c(-1,1)*braw.env$z_range
         })
  rActual<-r
  rActual[is.na(r)]<-0
  
  xoff=0
  if (!is.null(hypothesis$IV2) && effectType=="all") 
    xoff=c(0,1,2)*1.2

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
  logScale<-yaxis$logScale
  if (is.element(showType,c("rs","rse","rss")) && (!is.null(hypothesis$IV2))) 
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
    xticks<-makeTicks(breaks=xoff,c("direct","unique","total"))
  else
    xticks<-NULL
  
  g<-startPlot(xlim,ylim,
               xticks=xticks,xmax=TRUE,
               yticks=makeTicks(logScale=yaxis$logScale),ylabel=makeLabel(ylabel),
               box=box,top=top,orientation=orient,g=g)
  # g<-addG(g,yAxisTicks(logScale=yaxis$logScale),yAxisLabel(ylabel))
    # g<-addG(g,xAxisTicks(breaks=c(0,2,4),c("direct","unique","total")))
  lineCol<-"black"
  if (is.element(showType,c("p","e1p","e2p","e1d","e2d"))) lineCol<-"green"
  for (yl in ylines)
    g<-addG(g,horzLine(intercept=yl,linewidth=0.25,linetype="dashed",colour=lineCol))
  
  if (!all(is.na(analysis$rIV))) {
    data<-collectData(analysis,whichEffect)
    switch(braw.env$RZ,
           "r"={},
           "z"={
             data$rs<-atanh(data$rs)
             data$rp<-atanh(data$rp)
             data$ro<-atanh(data$ro)
           })
    switch (showType,
            "rs"={showVals<-data$rs},
            "rse"={showVals<-data$rs},
            "rss"={showVals<-data$rs},
            "rp"={showVals<-data$rp},
            "ro"={showVals<-data$ro},
            "re"={showVals<-data$rs-data$rp},
            "p"={showVals<-data$ps},
            "po"={showVals<-data$po},
            "llknull"={showVals<-exp(cbind(-0.5*(analysis$aic-analysis$aicNull)))},
            "sLLR"={showVals<-cbind(res2llr(analysis,"sLLR"))},
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
      showVals[showVals<(-10)]<--10
    }  
  }    
  sigOnly<-evidence$sigOnly
  
  if (!all(is.na(analysis$rIV))) { theoryAlpha<-0.5} else {theoryAlpha<-1}
  
  for (i in 1:length(xoff)){
    if (showTheory) {
      # theory<-doMultipleTheory(showType,logScale,hypothesis,design,i)
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
      
      if (is.element(showType,c("rs","rse","rss","re","ro","ci1","ci2","e1r","e2r"))) {
        npt<-101
        if (showType=="e1r") effectTheory$world$populationNullp<-1
        if (showType=="e2r") effectTheory$world$populationNullp<-0
        if (showType=="re") rOff<-"re"
        else rOff<-"rs"
        switch(braw.env$RZ,
               "r"={
                 if (!design$sNRand) {
                   cr<-tanh(pn2z(braw.env$alphaSig,design$sN))
                   inc<-cr/ceiling(cr/(2/npt))
                   rvals<-seq(inc,0.99,inc)
                   rvals<-c(-rev(rvals),0,rvals)
                 } else rvals<-seq(-1,1,length.out=npt)*0.99
                 xd<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=FALSE,HQ=braw.env$showTheoryHQ)
                 xdsig<-fullRSamplingDist(rvals,effectTheory$world,design,rOff,logScale=logScale,sigOnly=TRUE,HQ=braw.env$showTheoryHQ)
                 yv<-rvals
               },
               "z"={
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
               }
               )
      }
      
      npt<-101
      switch(showType,
             "rp"={
               switch(braw.env$RZ,
                      "r"={
                        yv<-seq(-1,1,length.out=npt)*0.99
                        xd<-rPopulationDist(yv,effectTheory$world)
                      },
                      "z"={
                        yv<-seq(-1,1,length.out=npt)*braw.env$z_range
                        xd<-rPopulationDist(tanh(yv),effectTheory$world)
                        xd<-rdens2zdens(xd,tanh(yv))
                      })
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
      if (is.infinite(theoryGain)) theoryGain<-0
      xd<-xd*theoryGain
      histGain<-abs(sum(xd*c(0,diff(yv))))
      histGainrange<-sort(c(yv[1],yv[length(yv)]))
      ptsp<-data.frame(y=c(yv,rev(yv)),x=c(xd,-rev(xd))+xoff[i])
      if (is.element(showType,c("rs","rse","rss","n","p","e1r","e2r","e1p","e2p"))) {
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
        if (showType=="rss") 
          resSig<-(analysis$sem[,8]==2)
      else 
        resSig<-isSignificant(braw.env$STMethod,pvals,rvals,nvals,data$df1,evidence)
      else
        resSig<-rep(FALSE,length(rvals))
      if (is.element(showType,c("rse","rss")) && ((hypothesis$effect$world$worldOn && hypothesis$effect$world$populationNullp>0)
          || (!all(data$rp!=0) && !all(data$rp==0))))
        resNotNull<-data$rp!=0
      else
        resNotNull<-rep(NA,length(data$rp))
      
      # if (sigOnly) {
      #   shvals<-shvals[resSig]
      #   rvals<-rvals[resSig]
      #   pvals<-pvals[resSig]
      #   nvals<-nvals[resSig]
      #   resSig<-resSig[resSig]
      #   resNotNull<-resNotNull[resSig]
      # }
      if (effectType=="all") {
        ysc<-1/3
        rvals<-(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1
      }
      if (is.element(showType,c("e1d","e2d"))) {
        d<-res2llr(analysis,braw.env$STMethod)
        err<-(d<0 & data$rp!=0) | (d>0 & data$rp==0)
        resWSig<-resSig & err
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y3=resWSig,y0=resNotNull,n=nvals)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=shvals,y2=resSig,y0=resNotNull,n=nvals)
      }
      
      g<-expected_plot(g,pts,showType,analysis,IV,DV,i,orientation=orientation,
                       ylim=ylim,histGain=histGain,histGainrange=histGainrange,npointsMax=200)
      
      ns<-c()
      s<-c()
      if (length(rvals)>1 && is.element(showType,c("rs","rse","rss","p","e1r","e2r","e1p","e2p","e1d","e2d"))) {
        n<-length(pvals)
        if (!is.null(otheranalysis) && effect$world$worldOn) n<-n+length(otheranalysis$pIV)
        if (is.element(showType,c("rse","rss"))) {
          sc<-sum((resSig)&(resNotNull))/n
          nse<-sum((!resSig)&(resNotNull))/n
          se<-sum((resSig)&(!resNotNull))/n
          nsc<-sum((!resSig)&(!resNotNull))/n
        } else {
          ns<-sum(!resSig,na.rm=TRUE)/n
          s<-sum(resSig,na.rm=TRUE)/n
        }
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
    
    if (length(rvals)>1)
    if (is.element(showType,c("rs","rse","rss","p","e1r","e2r","e1p","e2p","e1d","e2d"))) {
      lb1<-"p("
      lb2<-") = "
      lb1<-"("
      lb2<-")="
      labels<-c()
      colours<-c()
      npad<-function(a) {if (nchar(a)<2) return(paste0("  ",a)) else return(a)}
      switch (showType,
              "rs"={
                if (!is.null(s)) {
                  labels<-c(labels,paste0(lb1,"sig",lb2,brawFormat(s*100,digits=npct),"%"))
                  colours<-c(colours,braw.env$plotColours$infer_sigC)
                }
                if (!is.null(ns)) {
                  labels<-c(labels,paste0(lb1,"ns",lb2,brawFormat(ns*100,digits=npct),"%"))
                  colours<-c(colours,braw.env$plotColours$infer_nsigC)
                }
              },
              "rse"={
                if (!is.null(nse)) {
                  labels<-c(labels,paste0(npad(brawFormat(nse*100,digits=npct)),"%"," error"))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNonNull)
                }
                if (!is.null(sc)) {
                  labels<-c(labels,paste0(npad(brawFormat(sc*100,digits=npct)),"%"," correct"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNonNull)
                }
                if (!is.null(se)) {
                  labels<-c(labels,paste0(npad(brawFormat(se*100,digits=npct)),"%"," error"))
                  colours<-c(colours,braw.env$plotColours$infer_sigNull)
                }
                if (!is.null(nsc)) {
                  labels<-c(labels,paste0(npad(brawFormat(nsc*100,digits=npct)),"%"," correct"))
                  colours<-c(colours,braw.env$plotColours$infer_nsigNull)
                }
              },
                "rss"={
                  if (!is.null(nse)) {
                    labels<-c(labels,paste0(npad(brawFormat(nse*100,digits=npct)),"%"," error"))
                    colours<-c(colours,braw.env$plotColours$infer_nsigNonNull)
                  }
                  if (!is.null(sc)) {
                    labels<-c(labels,paste0(npad(brawFormat(sc*100,digits=npct)),"%"," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_sigNonNull)
                  }
                  if (!is.null(se)) {
                    labels<-c(labels,paste0(npad(brawFormat(se*100,digits=npct)),"%"," error"))
                    colours<-c(colours,braw.env$plotColours$infer_sigNull)
                  }
                  if (!is.null(nsc)) {
                    labels<-c(labels,paste0(npad(brawFormat(nsc*100,digits=npct)),"%"," correct"))
                    colours<-c(colours,braw.env$plotColours$infer_nsigNull)
                  }
                },
              "p"={
                labels<-c(paste0(lb1,"sig",lb2,brawFormat(s*100,digits=npct),"% "),
                          paste0(lb1,"ns",lb2,brawFormat(ns*100,digits=npct),"% "))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e1r"={
                labels<-c(paste0(lb1,"sig error",lb2,brawFormat(s*100,digits=npct),"% "),
                          paste0(lb1,"ns correct",lb2,brawFormat(ns*100,digits=npct),"% "))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e2r"={
                labels<-c(paste0(lb1,"sig correct",lb2,brawFormat(s*100,digits=npct),"% "),
                          paste0(lb1,"ns error",lb2,brawFormat(ns*100,digits=npct),"% "))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e1p"={
                labels<-c(paste0(lb1,"sig error",lb2,brawFormat(s*100,digits=npct),"% "),
                          paste0(lb1,"ns correct",lb2,brawFormat(ns*100,digits=npct),"% "))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e2p"={
                labels<-c(paste0(lb1,"sig correct",lb2,brawFormat(s*100,digits=npct),"% "),
                          paste0(lb1,"ns error",lb2,brawFormat(ns*100,digits=npct),"% "))
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e1d"={
                labels<-c(paste0(lb1,"sig error",lb2,brawFormat(s1/n*100,digits=npct),"% "),
                          paste0(lb1,"sig correct",lb2,brawFormat(s2/n*100,digits=npct),"%"),
                          paste0(lb1,"ns",lb2,brawFormat(ns/n*100,digits=npct),"% ")
                          )
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              },
              "e2d"={
                labels<-c(paste0(lb1,"sig correct",lb2,brawFormat(s1/n*100,digits=npct),"% "),
                          paste0(lb1,"sig error",lb2,brawFormat(s2/n*100,digits=npct),"%"),
                          paste0(lb1,"ns",lb2,brawFormat(ns/n*100,digits=npct),"% ")
                )
                colours<-c(braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_nsigC)
              }
      )
      if (length(labels)>0) g<-addG(g,dataLegend(data.frame(names=labels,colours=colours),title="",shape=22))
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
                   top=1.5,orientation="horz",g=g)
      g<-addG(g,dataBar(data=data.frame(x=0,y=mean(sigs)),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    } else {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("Outcomes"),
                   top=1.5,orientation="horz",g=g)
      col0<-braw.env$plotColours$infer_nsigNonNull
      col2<-braw.env$plotColours$infer_sigNonNull
      col3<-braw.env$plotColours$infer_sigNull
      col5<-braw.env$plotColours$infer_nsigNull
      lb0<-braw.env$nonNullNS
      lb2<-braw.env$nonNullSig
      lb3<-braw.env$nullSig
      lb5<-braw.env$nullNS
      
      y<-1
      y1<-y-mean(!nulls & !sigs)/2
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col0,barwidth=0.4))
      y<-y-mean(!nulls & !sigs)
      y2<-y-mean(!nulls & sigs)/2
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col2,barwidth=0.4))
      y<-y-mean(!nulls & sigs)
      y3<-y-mean(nulls & sigs)/2
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col3,barwidth=0.4))
      y<-y-mean(nulls & sigs)
      y4<-mean(nulls & !sigs)/2
      g<-addG(g,dataBar(data=data.frame(x=0,y=y),fill=col5,barwidth=0.4))
      
      g<-addG(g,dataLegend(data.frame(colours=c(col0,col2,col3,col5),names=c(lb0,lb2,lb3,lb5)),title="",shape=22))

    }
  } else {
    g<-startPlot(xlim=c(-1,3),ylim=c(0,1),
                 xticks=makeTicks(breaks=c(0,1,2),labels=c("DV~IV","DV~IV2","DV~IVxIV2")),
                 yticks=makeTicks(),ylabel=makeLabel("p(sig)"),
                 top=0,orientation="horz",g=g)
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV,analysis$rIV,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=0,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIV2,analysis$rIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=1,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
    psig<-mean(isSignificant(braw.env$STMethod,analysis$pIVIV2,analysis$rIVIV2,analysis$nval,analysis$df1,analysis$evidence))
    g<-addG(g,dataBar(data=data.frame(x=2,y=psig),fill=braw.env$plotColours$infer_sigC,barwidth=0.4))
    
  }
  return(g)
}

aic_plot<-function(analysis,disp,showTheory=TRUE,g=NULL) {
  # plotting a single result:
  
  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  n<-analysis$result$nval
  DVsd<-sd(analysis$result$dv)
  
  yaxis<-plotAxis("SEM",hypothesis,design)
  cols<-yaxis$cols
  
  if (is.null(analysis$sem)) analysis$sem<-analysis$result$sem
  
  nbar<-sum(!is.na(analysis$sem[1,1:7]))
  if (nbar==2) 
    if (analysis$hypothesis$effect$rIV!=0) correct<-2 else correct<-1
  else {
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2!=0) correct<-7
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2==0) correct<-6
    if (analysis$hypothesis$effect$rIV==0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2!=0) correct<-5
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2==0 && analysis$hypothesis$effect$rIVIV2!=0) correct<-4
    if (analysis$hypothesis$effect$rIV==0 && analysis$hypothesis$effect$rIV2!=0 && analysis$hypothesis$effect$rIVIV2==0) correct<-3
    if (analysis$hypothesis$effect$rIV!=0 && analysis$hypothesis$effect$rIV2==0 && analysis$hypothesis$effect$rIVIV2==0) correct<-2
    if (analysis$hypothesis$effect$rIV==0 && analysis$hypothesis$effect$rIV2==0 && analysis$hypothesis$effect$rIVIV2==0) correct<-1
  }
  
  sem<-analysis$sem1
  if (nrow(sem)==1) {
    range<-(max(sem[,1:nbar])-min(sem[,1:nbar]))
    lowY<-min(min(sem[,1:nbar])-range*0.5 , 1.5*n*DVsd)
    highY<-max(max(sem[,1:nbar])+range*0.25 , 3.5*n*DVsd)
    lowY<-lowY-(highY-lowY)/4
    g<-startPlot(xlim=c(0,nbar+1),ylim=c(lowY,highY),
                 yticks=makeTicks(),ylabel=makeLabel("AIC"),
                 top=FALSE,orientation="horz",g=g)
    startBar<-1
  } else {
    sem<-cbind(sem[,1:7]-sem[,1],sem[,8])
    lowY<- -1.0*analysis$design$sN*hypothesis$DV$sd
    highY<- 0.1*analysis$design$sN*hypothesis$DV$sd
    g<-startPlot(xlim=c(0,nbar),ylim=c(lowY,highY),
                 yticks=makeTicks(),ylabel=makeLabel("diff(AIC)"),
                 top=FALSE,orientation="horz",g=g)
    if (nbar==2) cols<-c(braw.env$plotColours$infer_nsigNull,braw.env$plotColours$infer_sigNonNull)
    startBar<-2
  }
  if (nrow(sem)>1) {
    markSize<-2
    shape<-22
  } else {
    markSize<-6
    shape<-21
  }
  for (ig in startBar:nbar) {
    if (ig==correct) fontface<-"bold" else fontface="plain"
    xvals<-makeFiddle(sem[,ig])
    if (any(xvals!=0))    xvals<-xvals/max(abs(xvals))*0.4
    use<-sem[,ncol(sem)]==ig
    if (startBar==2) {
      use1<-sem[,ncol(sem)]==1
      # g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use1],y=sem[use1,ig]),shape=shape,size=size,fill="grey"))
      g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use1],y=sem[use1,ig]),shape=shape,size=markSize,fill=cols[1]))
    } else {
      # g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[!use],y=sem[!use,ig]),shape=shape,size=size,fill="grey"))
      g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[!use],y=sem[!use,ig]),shape=shape,size=markSize,fill="white"))
    }
    # g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use],y=sem[use,ig]),shape=shape,size=size,fill="grey"))
    g<-addG(g,dataPoint(data.frame(x=ig-startBar+1+xvals[use],y=sem[use,ig]),shape=shape,size=markSize,fill=cols[ig]))
    if (nrow(sem)==1)
    g<-addG(g,dataText(data.frame(x=ig-startBar+1,y=min(sem[,ig])-(highY-lowY)/30),label=colnames(sem)[ig],
                       fontface=fontface,hjust=1,vjust=0.5,angle=90,size=0.6))   
  }
  return(g)
}

sem_plot<-function(analysis,disp,showTheory=TRUE,g=NULL){
  
  hypothesis<-analysis$hypothesis
  effect<-hypothesis$effect
  design<-analysis$design
  yaxis<-plotAxis("SEM",hypothesis,design)
  cols<-yaxis$cols
  
  if (is.null(analysis$sem)) analysis$sem<-analysis$result$sem
  nulls<-analysis$rp==0
  if (all(nulls) || all(!nulls)) {
      g<-startPlot(xlim=c(-2,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("SEM"),
                   top=1.5,orientation="horz",g=g)
      nbars<-sum(!is.na(analysis$sem[1,]))-1
      labels<-colnames(analysis$sem)
      proportions<-hist(analysis$sem[,8],breaks=(0:nbars)+0.5,plot=FALSE)$density
      plots<-cumsum(proportions)
      for (ig in nbars:1) {
        g<-addG(g,dataBar(data=data.frame(x=0,y=plots[ig]),fill=cols[ig],barwidth=0.4))
      if (proportions[ig]>0.02) 
        g<-addG(g,dataText(data=data.frame(x=-0.45,y=plots[ig]-proportions[ig]/2),labels[ig],
                           hjust=1,vjust=0.5,size=0.5,colour='white'))
    }
    } else {
      g<-startPlot(xlim=c(-1,1),ylim=c(0,1),
                   yticks=makeTicks(),ylabel=makeLabel("SEM"),
                   top=1.5,orientation="horz",g=g)
      nbars<-sum(!is.na(analysis$sem[1,]))-1
      labels<-c(
        paste0(braw.env$Null,"('",colnames(analysis$sem)[1:2],"')"),
        paste0(braw.env$nonNull,"('",colnames(analysis$sem)[2:1],"')")
        )
      proportions<-c(
        hist(analysis$sem[nulls,8],breaks=(0:nbars)+0.5,plot=FALSE)$counts,
        rev(hist(analysis$sem[!nulls,8],breaks=(0:nbars)+0.5,plot=FALSE)$counts)
        )
      proportions<-proportions/sum(proportions)
      plots<-cumsum(proportions)
      
      cols<-c(
        braw.env$plotColours$infer_nsigNull,
        braw.env$plotColours$infer_sigNull,
        braw.env$plotColours$infer_sigNonNull,
        braw.env$plotColours$infer_nsigNonNull
      )
      for (ig in length(plots):1) {
        g<-addG(g,dataBar(data=data.frame(x=0,y=plots[ig]),fill=cols[ig],barwidth=0.4))
      }
      g<-addG(g,dataLegend(data.frame(colours=rev(cols),names=rev(labels)),title="",shape=22))
    }
  return(g)
}

var_plot<-function(analysis,disp,otheranalysis=NULL,orientation="vert",showTheory=TRUE,g=NULL){
  g<-r_plot(analysis,showType=disp,showTheory=showTheory,g=g)
}
