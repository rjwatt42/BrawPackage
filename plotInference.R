nscaleLog=FALSE
maxnPlot=200

trimanalysis<-function(analysis) {
  
  use<-(!is.na(analysis$rIV))
  
  analysis$rpIV=analysis$rpIV[use]
  analysis$rIV=analysis$rIV[use]
  analysis$pIV=analysis$pIV[use]
  analysis$roIV=analysis$roIV[use]
  analysis$poIV=analysis$poIV[use]
  analysis$nval=analysis$nval[use]
  analysis$df1=analysis$df1[use]
  
  if (!is.null(analysis$IV2)) {
    analysis$rIV2=analysis$rIV2[use]
    analysis$pIV2=analysis$pIV2[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$rIVIV2DV=analysis$rIVIV2DV[use]
    analysis$r$direct=analysis$r$direct[use,]
    analysis$r$unique=analysis$r$unique[use,]
    analysis$r$total=analysis$r$total[use,]
    analysis$p$direct=analysis$p$direct[use,]
    analysis$p$unique=analysis$p$unique[use,]
    analysis$p$total=analysis$p$total[use,]
  }
  
  analysis
}

plotInference<-function(analysis,disp="r",orientation="vert",showType="direct"){
  if (length(disp)==2) {
    return(plot2Inference(analysis,disp[1],disp[2]))
  } 
  analysis<-trimanalysis(analysis)
  
  switch (disp,
          "r"= {g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          "rp"={g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          "r1"={g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          "ra"= {g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          "ci1"={g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          "ci2"={g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          "t"= {g<-r_plot(analysis,disp,orientation=orientation,showType=showType)},
          
          "p"= {g<-p_plot(analysis,disp,orientation=orientation,showType=showType)},
          "p1"= {g<-p_plot(analysis,disp,orientation=orientation,showType=showType)},
          
          "log(lrs)"={g<-l_plot(analysis,disp,orientation=orientation)},
          "log(lrd)"={g<-l_plot(analysis,disp,orientation=orientation)},
          
          "w"= {g<-w_plot(analysis,disp,orientation=orientation)},
          "wp"={g<-w_plot(analysis,disp,orientation=orientation)},
          
          "nw"={g<-n_plot(analysis,disp,orientation=orientation)},
          "n"= {g<-n_plot(analysis,disp,orientation=orientation)},
          
          "e1"={g<-e1_plot(analysis,orientation=orientation)},
          "e2"={g<-e2_plot(analysis,orientation=orientation)}
  )
  g+ggtitle(analysis$an_name)
}


plot2Inference<-function(analysis,disp1,disp2,metaPlot=FALSE){
    
  r<-analysis$effect$rIV
  if (!is.null(analysis$IV2)){
    r<-c(r,analysis$effect$rIV2,analysis$effect$rIVIV2DV)
  }
  
  pvals<-analysis$pIV
  rvals<-analysis$rIV
  nvals<-analysis$nval
  df1vals<-analysis$df1

  rlim<-c(-1,1)
  if (RZ=="z")  {
    r<-atanh(r)
    rvals<-atanh(rvals)
    rlim<-c(-1,1)*z_range
  }
  xsc<-0
  disp1_use<-disp1
  disp2_use<-disp2
  switch (disp1,
          "p"={
            d1<-analysis$pIV
            if (pPlotScale=="log10"){xsc<-1}
            xlim<-c(0,1)
          },
          "r"={
            d1<-analysis$rIV
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-zsLabel
            } else {
              disp1_use<-rsLabel
            }
            xlim<-rlim
          },
          "log(lrs)"={
            d1<-res2llr(analysis,"sLLR")
            xlim<-c(-0.1, lrRange)
            disp1_use<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            d1<-res2llr(analysis,"dLLR")
            if (any(d1<0)) {
              ylim<-c(-lrRange, lrRange)
            } else {
              ylim<-c(-0.1, lrRange)
            }
            disp1_use<-bquote(log[e](lr[d]))
          },
          "w"={
            d1<-analysis$rIV
            d1<-rn2w(d1,analysis$nval)
            if (wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "wp"={
            d1<-analysis$rp
            d1<-rn2w(d1,analysis$nval)
            if (wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "nw"={
            d1<-rw2n(analysis$rIV,0.8,analysis$design$sReplTails)
            xlim<-c(1, max(d1)*1.1)
          },
          "rp"={
            d1<-analysis$rpIV
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-zpLabel
            } else {
              disp1_use<-rpLabel
            }
            xlim<-rlim
          },
          "r1"={
            d1<-analysis$roIV
            disp1_use<-bquote(r[1])
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-bquote(z[1])
            }
            xlim<-rlim
          },
          "ra"={
            d1<-analysis$rIVa
            disp1_use<-bquote(r[1])
            if (RZ=="z") {
              d1<-atanh(d1)
              disp1_use<-bquote(z[a])
            } else{
              disp1_use<-bquote(r[a])
            }
            xlim<-rlim
          },
          "p1"={
            d1<-analysis$poIV
            if (pPlotScale=="log10"){xsc<-1}
            xlim<-c(0, 1)
            disp1_use<-bquote(p[1])
          },
          "n"={
            d1<-analysis$nval
            xlim<-c(1, 200*1.1)
          },
          "t"={
            d1<-analysis$tval
            xlim<-c(-5,5)
          }
  )
  
  ysc<-0
  switch (disp2,
          "p"={
            d2<-analysis$pIV
            if (pPlotScale=="log10"){
              ysc<-1
              }
            ylim<-c(0,1)
          },
          "r"={
            d2<-analysis$rIV
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-zsLabel
            } else {
              disp2_use<-rsLabel
            }
            ylim<-rlim
          },
          "ra"={
            d2<-analysis$rIVa
            disp2_use<-bquote(r[1])
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-bquote(z[a])
            } else {
              disp2_use<-bquote(r[a])
            }
            ylim<-rlim
          },
          "r1"={
            d2<-analysis$roIV
            disp2_use<-bquote(r[1])
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-bquote(z[1])
            } else {
              disp2_use<-bquote(r[1])
            }
            ylim<-rlim
          },
          "p1"={
            d2<-analysis$poIV
            if (pPlotScale=="log10"){
              ysc<-1
            }
            ylim<-c(0, 1)
            disp2_use<-bquote(p[1])
          },
          "rp"={
            d2<-analysis$rpIV
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2_use<-zpLabel
            } else {
              disp2_use<-rpLabel
            }
            ylim<-rlim
          },
          "n"={
            d2<-analysis$nval
            ylim<-c(1, 200*1.1)
            if (nscaleLog) {
              ysc<-2
              ylim<-c(log10(6), log10(200))
            }
          },
          "w"={
            d2<-rn2w(analysis$rIV,analysis$nval)
            if (wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "wp"={
            d2<-rn2w(analysis$rp,analysis$nval)
            if (wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "log(lrs)"={
            ylim<-c(-0.1, lrRange)
            disp2_use<-bquote(log[e](lr[s]))
          },
          "log(lrd)"={
            d2<-res2llr(analysis,"dLLR")
            if (any(d2<0)) {
              ylim<-c(-lrRange, lrRange)
            } else {
              ylim<-c(-0.1, lrRange)
            }
            disp2_use<-bquote(log[e](lr[d]))
          },
          "nw"={
            d2<-rw2n(analysis$rIV,0.8,analysis$design$sReplTails)
            ylim<-c(1, max(d1)*1.1)
          },
          "R"={
            d2<-analysis$rpIV
            if (RZ=="z") {
              d2<-atanh(d2)
              disp2<-"Z"
            }
            ylim<-rlim
          },
          "t"={
            d2<-analysis$tval
            ylim<-c(-5,5)
          }
  )

  if (xsc==1) {
    d1<-log10(d1)
    xlim<-c(log10(min_p), 0)
    disp1_use<-bquote(bold(log['10'] (.(disp1))))
  }
  if (ysc==1) {
    d2<-log10(d2)
    ylim<-c(log10(min_p), 0)
    disp2_use<-bquote(bold(log['10'] (.(disp2))))
  }
  if (ysc==2) {
    d2<-log10(d2)
    disp2_use<-bquote(bold(log['10'] (.(disp2))))
  }
  pts<-data.frame(x=d1,y=d2)
  
  g<-ggplot(pts,aes(x=x, y=y))
  
  if (disp1=="r" && disp2=="p") {
    rs<-seq(-r_range,r_range,length.out=51)
    ps<-r2p(rs,analysis$nval[1])
    if (pPlotScale=="log10")  ps<-log10(ps)
    g<-g+geom_line(data=data.frame(x=rs,y=ps),aes(x=x,y=y),col="white")
  }
  
  if (disp1=="z" && disp2=="za") {
    rs<-c(-z_range,z_range)
    g<-g+geom_line(data=data.frame(x=rs,y=rs),aes(x=x,y=y),col="white")
    gain<-mean(pts$y/pts$x)
    g<-g+geom_line(data=data.frame(x=rs/gain,y=rs),aes(x=x,y=y),col="white",linetype=3)
    g<-g+geom_label(data=data.frame(x=z_range/gain,y=z_range,label=format(gain)),aes(x=x,y=y,label=label))
  }
  
  dotSize=min(8,max(3.5,sqrt(400/length(d1))))
  dotSize<-dotSize<-(plotTheme$axis.title$size)/3

  if (!metaPlot && useSignificanceCols){
    c1=plotcolours$infer_sigC
    c2=plotcolours$infer_nsigC
  } else {
    c1=plotcolours$descriptionC
    c2=plotcolours$descriptionC
  }
  if (length(d1)<200) {
    use<-!isSignificant(STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
    pts1=pts[use,]
    g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=shapes$study, colour = "black", fill = c2, size = dotSize)
    pts2=pts[!use,]
    g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=shapes$study, colour = "black", fill = c1, size = dotSize)
  } else {
    if (length(d1)<=10000) {
      use<-!isSignificant(STMethod,pvals,rvals,nvals,df1vals,analysis$evidence)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=shapes$study, colour = c2, fill = c2, size = dotSize/4)
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=shapes$study, colour = c1, fill = c1, size = dotSize/4)
    } else {
      use<-d2<=maxnPlot
      pts<-data.frame(x=d1[use],y=d2[use])
      nbins<-diff(ylim)/(2*IQR(d2[use])*length(d2[use])^(-0.33))
      g<-g+stat_bin2d(data=pts,aes(x=x,y=y),bins=nbins)+scale_fill_gradientn(colours=c(graphcolours$graphBack,plotcolours$descriptionC))
    }
  }
  g<-g+theme(legend.position = "none")+plotTheme
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
  g+ggtitle(analysis$an_name)
}
