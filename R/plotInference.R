
trimanalysis<-function(analysis) {
  
  use<-(!is.na(analysis$rIV))
  
  analysis$rIV=analysis$rIV[use]
  analysis$pIV=analysis$pIV[use]
  analysis$rpIV=analysis$rpIV[use]
  analysis$raIV=analysis$raIV[use]
  analysis$roIV=analysis$roIV[use]
  analysis$poIV=analysis$poIV[use]
  analysis$nval=analysis$nval[use]
  analysis$df1=analysis$df1[use]
  
  if (!is.null(analysis$hypothesis$IV2)) {
    analysis$rIV2=analysis$rIV2[use]
    analysis$pIV2=analysis$pIV2[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$r$direct=matrix(analysis$r$direct[use,],nrow=sum(use))
    analysis$r$unique=matrix(analysis$r$unique[use,],nrow=sum(use))
    analysis$r$total=matrix(analysis$r$total[use,],nrow=sum(use))
    analysis$p$direct=matrix(analysis$p$direct[use,],nrow=sum(use))
    analysis$p$unique=matrix(analysis$p$unique[use,],nrow=sum(use))
    analysis$p$total=matrix(analysis$p$total[use,],nrow=sum(use))
  }
  
  analysis
}

plotInference<-function(analysis,disp="r",orientation="vert",effectType="direct",showTheory=TRUE,g=NULL){
  if (length(disp)==2) {
    return(plot2Inference(analysis,disp[1],disp[2]))
  } 
  analysis<-trimanalysis(analysis)
  
  switch (disp,
          "r"= {g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "rp"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ro"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ra"= {g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ci1"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "ci2"={g<-r_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},

          "p"= {g<-p_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          "po"= {g<-p_plot(analysis,disp,orientation=orientation,effectType=effectType,showTheory=showTheory,g=g)},
          
          "log(lrs)"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          "log(lrd)"={g<-l_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          
          "w"= {g<-w_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          "wp"={g<-w_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          
          "nw"={g<-n_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          "n"= {g<-n_plot(analysis,disp,orientation=orientation,showTheory=showTheory,g=g)},
          
          "e1"={g<-e1_plot(analysis,orientation=orientation,showTheory=showTheory,g=g)},
          "e2"={g<-e2_plot(analysis,orientation=orientation,showTheory=showTheory,g=g)}
  )
  return(g)
}


plot2Inference<-function(analysis,disp1,disp2,metaPlot=FALSE){
    
  r<-analysis$hypothesis$rIV
  if (!is.null(analysis$hypothesis$IV2)){
    r<-c(r,analysis$hypothesis$rIV2,analysis$hypothesis$rIVIV2DV)
  }
  
  pvals<-analysis$pIV
  rvals<-analysis$rIV
  nvals<-analysis$nval
  df1vals<-analysis$df1

  rlim<-c(-1,1)
  if (braw.env$RZ=="z")  {
    r<-atanh(r)
    rvals<-atanh(rvals)
    rlim<-c(-1,1)*braw.env$z_range
  }
  xsc<-0
  disp1_use<-disp1
  disp2_use<-disp2
  switch (disp1,
          "r"={
            d1<-analysis$rIV
            if (braw.env$RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-braw.env$zsLabel
            } else {
              disp1_use<-braw.env$rsLabel
            }
            xlim<-rlim
          },
          "p"={
            d1<-analysis$pIV
            if (braw.env$pPlotScale=="log10"){xsc<-1}
            xlim<-c(0,1)
          },
          "rp"={
            d1<-analysis$rpIV
            if (braw.env$RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-braw.env$zpLabel
            } else {
              disp1_use<-braw.env$rpLabel
            }
            xlim<-rlim
          },
          "ra"={
            d1<-analysis$raIV
            disp1_use<-bquote(r[1])
            if (braw.env$RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-bquote(z[a])
            } else{
              disp1_use<-bquote(r[a])
            }
            xlim<-rlim
          },
          "ro"={
            d1<-analysis$roIV
            disp1_use<-bquote(r[1])
            if (braw.env$RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-bquote(z[1])
            }
            xlim<-rlim
          },
          "po"={
            d1<-analysis$poIV
            if (braw.env$pPlotScale=="log10"){xsc<-1}
            xlim<-c(0, 1)
            disp1_use<-bquote(p[1])
          },
          "n"={
            d1<-analysis$nval
            xlim<-c(1, 200*1.1)
          },
          "log(lrs)"={
            d1<-res2llr(analysis,"sLLR")
            xlim<-c(-0.1, braw.env$lrRange)
            disp1_use<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            d1<-res2llr(analysis,"dLLR")
            if (any(d1<0)) {
              ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            } else {
              ylim<-c(-0.1, braw.env$lrRange)
            }
            disp1_use<-bquote(log[e](lr[d]))
          },
          "w"={
            d1<-analysis$rIV
            d1<-rn2w(d1,analysis$nval)
            if (braw.env$wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "wp"={
            d1<-analysis$rp
            d1<-rn2w(d1,analysis$nval)
            if (braw.env$wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "nw"={
            d1<-rw2n(analysis$rIV,0.8,analysis$design$ReplTails)
            xlim<-c(1, max(d1)*1.1)
          }
  )
  
  ysc<-0
  switch (disp2,
          "r"={
            d2<-analysis$rIV
            if (braw.env$RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-braw.env$zsLabel
            } else {
              disp2_use<-braw.env$rsLabel
            }
            ylim<-rlim
          },
          "p"={
            d2<-analysis$pIV
            if (braw.env$pPlotScale=="log10"){
              ysc<-1
              }
            ylim<-c(0,1)
          },
          "rp"={
            d2<-analysis$rpIV
            if (braw.env$RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-braw.env$zpLabel
            } else {
              disp2_use<-braw.env$rpLabel
            }
            ylim<-rlim
          },
          "ra"={
            d2<-analysis$raIV
            disp2_use<-bquote(r[1])
            if (braw.env$RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-bquote(z[a])
            } else {
              disp2_use<-bquote(r[a])
            }
            ylim<-rlim
          },
          "ro"={
            d2<-analysis$roIV
            disp2_use<-bquote(r[1])
            if (braw.env$RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-bquote(z[1])
            } else {
              disp2_use<-bquote(r[1])
            }
            ylim<-rlim
          },
          "po"={
            d2<-analysis$poIV
            if (braw.env$pPlotScale=="log10"){
              ysc<-1
            }
            ylim<-c(0, 1)
            disp2_use<-bquote(p[1])
          },
          "n"={
            d2<-analysis$nval
            ylim<-c(1, 200*1.1)
            if (braw.env$nscaleLog) {
              ysc<-2
              ylim<-c(log10(6), log10(200))
            }
          },
          "w"={
            d2<-rn2w(analysis$rIV,analysis$nval)
            if (braw.env$wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "wp"={
            d2<-rn2w(analysis$rp,analysis$nval)
            if (braw.env$wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "log(lrs)"={
            ylim<-c(-0.1, braw.env$lrRange)
            disp2_use<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            d2<-res2llr(analysis,"dLLR")
            if (any(d2<0)) {
              ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            } else {
              ylim<-c(-0.1, braw.env$lrRange)
            }
            disp2_use<-bquote(log[e](lr[d]))
          },
          "nw"={
            d2<-rw2n(analysis$rIV,0.8,analysis$design$ReplTails)
            ylim<-c(1, max(d1)*1.1)
          }
  )

  if (xsc==1) {
    d1<-log10(d1)
    xlim<-c(log10(braw.env$min_p), 0)
    disp1_use<-bquote(bold(log['10'] (.(disp1))))
  }
  if (ysc==1) {
    d2<-log10(d2)
    ylim<-c(log10(braw.env$min_p), 0)
    disp2_use<-bquote(bold(log['10'] (.(disp2))))
  }
  if (ysc==2) {
    d2<-log10(d2)
    disp2_use<-bquote(bold(log['10'] (.(disp2))))
  }
  pts<-data.frame(x=d1,y=d2)
  
  g<-ggplot(pts,aes(x=x, y=y))
  
  if (disp1=="r" && disp2=="p") {
    rs<-seq(-braw.env$r_range,braw.env$r_range,length.out=51)
    ps<-r2p(rs,analysis$nval[1])
    if (braw.env$pPlotScale=="log10")  ps<-log10(ps)
    g<-g+geom_line(data=data.frame(x=rs,y=ps),aes(x=x,y=y),col="white")
  }
  
  if (disp1=="z" && disp2=="za") {
    rs<-c(-braw.env$z_range,braw.env$z_range)
    g<-g+geom_line(data=data.frame(x=rs,y=rs),aes(x=x,y=y),col="white")
    gain<-mean(pts$y/pts$x)
    g<-g+geom_line(data=data.frame(x=rs/gain,y=rs),aes(x=x,y=y),col="white",linetype=3)
    g<-g+geom_label(data=data.frame(x=braw.env$z_range/gain,y=braw.env$z_range,label=format(gain)),aes(x=x,y=y,label=label))
  }
  
  dotSize=min(8,max(3.5,sqrt(400/length(d1))))
  dotSize<-dotSize<-(braw.env$plotTheme$axis.title$size)/3

  if (!metaPlot && braw.env$useSignificanceCols){
    c1=braw.env$plotColours$infer_sigC
    c2=braw.env$plotColours$infer_nsigC
  } else {
    c1=braw.env$plotColours$descriptionC
    c2=braw.env$plotColours$descriptionC
  }
  if (length(d1)<200) {
    use<-!isSignificant(braw.env$STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
    pts1=pts[use,]
    g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=braw.env$plotShapes$study, colour = "black", fill = c2, size = dotSize)
    pts2=pts[!use,]
    g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=braw.env$plotShapes$study, colour = "black", fill = c1, size = dotSize)
  } else {
    if (length(d1)<=10000) {
      use<-!isSignificant(braw.env$STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=braw.env$plotShapes$study, colour = c2, fill = c2, size = dotSize/4)
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=braw.env$plotShapes$study, colour = c1, fill = c1, size = dotSize/4)
    } else {
      use<-d2<=braw.env$maxnPlot
      pts<-data.frame(x=d1[use],y=d2[use])
      nbins<-diff(ylim)/(2*IQR(d2[use])*length(d2[use])^(-0.33))
      g<-g+stat_bin2d(data=pts,aes(x=x,y=y),bins=nbins)+scale_fill_gradientn(colours=c(braw.env$plotColours$graphBack,braw.env$plotColours$descriptionC))
    }
  }
  g<-g+theme(legend.position = "none")+braw.env$plotTheme
  if (xsc==0) {
    g<-g+scale_x_continuous(limits = xlim)
  } else {
    g<-g+scale_x_continuous(limits = xlim,breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  }
  if (ysc==0) {
    g<-g+scale_y_continuous(limits = ylim)
  }
  if (ysc==1) {
    g<-g+scale_y_continuous(limits = ylim,breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  }
  if (ysc==2) {
    g<-g+scale_y_continuous(limits = ylim)
  }
  
  g<-g+xlab(disp1_use)+ylab(disp2_use)
}
