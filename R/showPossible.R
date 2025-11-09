drawDistribution<-function(x_use,y_use,z_use,xlim,ylim,zlim,mapping,
                           colUse,lineUse,alphaUse,draw_lower_limit,g) {
  if (is.na(colUse)) lineWidth<-0.5 else lineWidth<-0.25
  z_use[z_use<draw_lower_limit]<-0
  while (length(z_use)>0 && any(z_use>0)) {
    use1<-which(z_use>0)[1]
    z_use<-z_use[use1:length(z_use)]
    y_use<-y_use[use1:length(y_use)]
    x_use<-x_use[use1:length(x_use)]
    use2<-which(c(z_use,0)==0)[1]-1
    z_draw<-z_use[1:use2]
    y_draw<-y_use[1:use2]
    x_draw<-x_use[1:use2]
    # z_draw[z_draw<draw_lower_limit]<-0
    use<-which((y_draw>=ylim[1] & y_draw<=ylim[2]) & (x_draw>=xlim[1] & x_draw<=xlim[2]))
    if (length(use)>0) {
      dL<-data.frame(x = c(x_use[use[1]],x_use[use],x_use[use[length(use)]]), 
                     y = c(y_use[use[1]],y_use[use],y_use[use[length(use)]]), 
                     z = c(zlim[1],z_draw[use],zlim[1]))
      if (x_use[use[1]]-xlim[1] < 0.1 || y_use[use[1]]-ylim[1] < 0.1) 
        dL1<-data.frame(x = x_use[use], 
                        y = y_use[use], 
                        z = z_draw[use])
      else
      dL1<-data.frame(x = c(x_use[use[1]],x_use[use],x_use[use[length(use)]]), 
                      y = c(y_use[use[1]],y_use[use],y_use[use[length(use)]]), 
                      z = c(zlim[1],z_draw[use],zlim[1]))
      if (!is.na(colUse))
      g<-addG(g,
              dataPolygon(rotate3D(dL,mapping),colour=NA,fill=colUse,alpha=alphaUse))
      g<-addG(g,
              dataPath(rotate3D(dL1,mapping),colour=lineUse,alpha=alphaUse,linewidth=lineWidth))
    }
    if (use2==length(z_use)) break
    z_use<-z_use[(use2+1):length(z_use)]
    y_use<-y_use[(use2+1):length(y_use)]
    x_use<-x_use[(use2+1):length(x_use)]
  }
  return(g)
}

densityFunctionStats<-function(dens_r,rp){
  if (is.matrix(dens_r)) dens_r<-dens_r[1,]
  
  use<-!is.na(dens_r)
  cum_dens_r<-cumsum(dens_r[use])/sum(dens_r[use])
  cum_rp<-rp[use]
  if (length(unique(cum_dens_r))<5) {
    ci<-c(-1,1)
  } else {
    if (any(cum_dens_r==0)) {
      use1<-max(which(cum_dens_r==0))
    } else {use1<-1}
    if (any(cum_dens_r==1)) {
      use2<-min(which(cum_dens_r==1))
    } else {use2<-length(cum_rp)}
    keep<-use1:use2
    use<-match(unique(cum_dens_r[keep]),cum_dens_r[keep])
    if (length(keep[use])>=2) {
      ci<-approx(cum_dens_r[keep[use]],cum_rp[keep[use]]+(rp[2]-rp[1])/2,c(0.025,0.975))$y
    } else {
      ci<-c(-1,1)
    }
  }
  
  pk<-which.max(dens_r)
  if (sum(dens_r>0)<=1) 
    peak<-rp[pk]
  else {
    dr<-diff(dens_r[(pk-3):(pk+3)])
    peak<-approx(dr,rp[(pk-3):(pk+2)]+diff(rp[1:2])/2,0)$y
  }
  dens_at_peak<-max(dens_r)
  list(
    peak=peak,
    mean=sum(rp*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE),
    sd=sqrt(sum((rp)^2*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE)),
    ci=ci,
    dens_at_peak=dens_at_peak
  )
  
}

describePossibleSamples<-function(possibleResult) {
  
  if (is.null(possibleResult$Sims$r)) return(NULL)
  pRho<-possibleResult$pRho
  if (is.null(pRho)) pRho<-0
  pRhogain<-1
  
  sr_effectR<-matrix(possibleResult$Sims$r,nrow=1)
  sSimDens<-c()
  if (!isempty(sr_effectR)) {
    switch(braw.env$RZ,
           "r"={
             use_effects<-sr_effectR
             hist_range<-braw.env$r_range
           },
           "z"={
             use_effects<-atanh(sr_effectR)
             hist_range<-braw.env$z_range
           }
    )
    binWidth<-2*IQR(use_effects)/length(use_effects)^(1/3)
    nbins=round(2/binWidth)
    sSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
    for (i in 1:nrow(use_effects)) {
      use_data<-abs(use_effects[i,])<=hist_range
      h<-hist(use_effects[i,use_data],sSimBins,plot=FALSE)$counts
      sSimDens<-rbind(sSimDens,h*pRhogain[i]/(1-tanh(pRho[i])^2))
    }

    result<-list(sr_effectR=sr_effectR,
                 sSimBins=sSimBins,sSimDens=sSimDens
                 )
  } else {
    result<-NULL
  }
  
}


describePossiblePopulations<-function(possibleResult,possible) {
  
  sRho<-possibleResult$sRho
  
  pr_effectR<-possibleResult$Sims$r
  pr_effectRP<-possibleResult$Sims$rp
  pr_effectN<-possibleResult$Sims$n
  
  if (!isempty(pr_effectRP)) {

    # do this in z - for symmetry
    keep<-abs(atanh(pr_effectR)-sRho[1])<possible$simSlice
    pr_effectRP_slice<-pr_effectRP[keep]
    
    switch(braw.env$RZ,
           "r"={
      use_effectRP_slice<-pr_effectRP_slice
      use_effectR<-pr_effectR
      use_effectRP<-pr_effectRP
      hist_range<-braw.env$r_range
           },
           "z"={
      use_effectRP_slice<-atanh(pr_effectRP_slice)
      use_effectR<-atanh(pr_effectR)
      use_effectRP<-atanh(pr_effectRP)
      hist_range<-braw.env$z_range
           }
    )
    
    if (possible$prior$PDF=="Single" || possible$prior$PDF=="Double") {
      binWidth<-0.05
    } else {
      binWidth<-max(0.05,2*IQR(use_effectRP_slice,na.rm=TRUE)/length(use_effectRP_slice)^(1/3))
    }
    nbins=max(10,round(2/binWidth))
    pSimBins<-seq(-1,1,length.out=nbins+1)*hist_range

    keep<-abs(use_effectRP_slice)<hist_range
    pSimDens_slice<-hist(use_effectRP_slice[keep],pSimBins,plot=FALSE)$counts
    
    keep<-abs(use_effectRP)<hist_range
    pSimDensRP<-hist(use_effectRP[keep],pSimBins,plot=FALSE)$counts
    
    keep<-abs(use_effectR)<hist_range
    pSimDensR<-hist(use_effectR[keep],pSimBins,plot=FALSE)$counts
    
    result<-list(pr_effectR=pr_effectR,pr_effectRP=pr_effectRP,
                 pSimBins=pSimBins,
                 pSimDens_slice=pSimDens_slice,
                 pSimDensRP=pSimDensRP,pSimDensR=pSimDensR
    )
  } else {
    result<-NULL
  }
  
}

#' show the likelihood theory
#' 
#' @param view        "3D","2D"
#' @return ggplot2 object - and printed
#' @examples
#' showPossible <- function(possibleResult=makePossible(),
#'                        showType="Populations",
#'                        cutaway=FALSE,walls=TRUE,showP=0,
#'                        view="3D",axisScale=1,
#'                        azimuth=30,elevation=5,distance=2)
#' @export
showPossible <- function(possibleResult=NULL,
                         showType="Populations",
                         cutaway=FALSE,walls="both",showP=0,doTextResult=TRUE,showSig=FALSE,
                         view="3D",axisScale=1,showEvLk=FALSE,
                         azimuth=NULL,elevation=15,distance=8){
  
  if (is.null(possibleResult)) possibleResult<-doPossible()
  if (is.numeric(possibleResult)) {
    sRho<-possibleResult
   #  if (showType=="Samples") {
   #    possibleResult$sRho<-sRho
   #    possibleResult<-doPossible()
   # } else {
      possibleResult<-doPossible(makePossible(sRho))
   # }
  }
  if (length(possibleResult)==1 && is.na(possibleResult)) {
    possibleResult<-doPossible()
    possibleResult$sRho<-c()
  } 
  if (is.null(possibleResult$possible)) possibleResult<-doPossible(possible=possibleResult)

  possible<-possibleResult$possible
  world<-possible$hypothesis$effect$world
  design<-possible$design
  
  oldRZ<-braw.env$RZ
  braw.env$RZ<-possible$axisType
  on.exit(setBrawEnv("RZ",oldRZ))
  
  if (is.null(azimuth)) 
    switch(showType,"Samples"={azimuth=35},"Populations"={azimuth=55})
  
  if (is.character(walls)) {
  switch(walls,
         "none"={walls<-0},
         "both"={walls<-3},
         "samples"={walls<-1},
         "populations"={walls<-2}
         )
  }
  if (walls<0) {
    walls<-abs(walls)
    possible$showTheory<-FALSE
  } 
  
  BoxCol<-"#666666"
  BoxColFloor<-darken(BoxCol,off=0.5)
  BoxColSamples<-darken(BoxCol,off=0.35)
  BoxColPopulations<-darken(BoxCol,off=0.4)
  
  colS<-"#FFEE00"
  if (showP>0)  colS<-braw.env$plotColours$infer_nsigC
  colSsum<-"#FFBB88"
  colSsum<-darken(colS,off=-0.45)
  colSdark=darken(colS,off=-0.67)
  colSlight=darken(colS,off=0.2)
  colSsim=darken(colS,off=0.0)
  
  colP="#88AAFF"
  colPsum<-darken(colP,off=-0.45)
  colPdark=darken(colP,off=-0.67)
  colPlight=darken(colP,off=0.25)
  colPsim=darken(colP,off=-0.33)

  colPop="#000000"
  colVline="#000000"
  
  colNullS=darken(braw.env$plotColours$infer_nsigC,off=-0.25)
  colDistS=darken(braw.env$plotColours$infer_sigC,off=-0.25)
  highTransparency=0.25
  sampDensGain=0.65
  sampDensGain<-1
  
  char3D=braw.env$labelSize/3
  xylabelSize=1.1
  zlabelSize=0.7
  tickLabelSize<-0.6
  
  if (showType=="Samples") logZ<-FALSE else logZ<-FALSE 
  if (logZ) wallHeight<-1 else wallHeight<-1
  magRange<-0.5
  
  boxed<-TRUE
  boxedDash<-FALSE
  doBothZAxes<-FALSE
  doConnecting<-TRUE
  doSampleLine<-FALSE
  doPeakLine<-TRUE
  doFloorZeroLines<-FALSE 
  doFloorPeakLines<-TRUE 
  doCILines<-FALSE
  showJointLk<-FALSE
  showNull<-FALSE
  normSampDist<-FALSE
  endFace<-TRUE
  continuousSampling<-2
  
  offRange<-0
  if (length(axisScale)==2) {
    offRange<-mean(axisScale)
    axisScale<-2/diff(axisScale)
  } else {
    if (axisScale>1 && !is.null(possible$targetSample)) offRange<-possible$targetSample  
  }
  switch(braw.env$RZ,
         "r"={
           xlim<-c(-1,1)/axisScale+offRange # population
           ylim<-c(-1,1)/axisScale+offRange
         },
         "z"={
           xlim<-c(min(rp),max(rp))/axisScale+offRange # population
           ylim<-c(min(rs),max(rs))/axisScale+offRange
         }
  )
  
  zlim<-c(0,1)
  
  switch (possible$UseSource,
          "hypothesis"={possible$source<-list(On=TRUE,PDF="Single",PDFk=effect$rIV,RZ="r",pRplus=1)},
          "world"={possible$source<-world},
          "prior"={possible$source<-possible$prior},
          "null"={possible$source<-list(On=TRUE,PDF="Single",PDFk=0,RZ="r",pRplus=1)}
  )
  if (showType=="Samples") possible$UsePrior<-"hypothesis"
  # make the distribution        
  
  switch (showType,
          "Samples"={
            label.z<-"Probability Density"
            col=colP
          },
          "Populations"={
            label.z<-"Likelihood"
            col=colS
          }
  )
  if (logZ) label.z<-paste0("Log ",label.z)
  
  sourceRVals<-possibleResult$sourceRVals
  sRho<-possibleResult$sRho
  if (length(sRho)==0) sRho<-NA
  n<-possibleResult$possible$design$sN
  if (showType=="Populations") n<-possibleResult$possible$targetSampleN
  
  rs<-possibleResult$Theory$rs
  sourceSampDens_r_null<-possibleResult$Theory$sourceSampDens_r_null
  sourceSampDens_r_plus<-possibleResult$Theory$sourceSampDens_r_plus
  sourceSampDens_r_total<-possibleResult$Theory$sourceSampDens_r_total
  if (nrow(sourceSampDens_r_plus)>1) {
  use<-sourceRVals>xlim[1] & sourceRVals<xlim[2]
  sourceSampDens_r_plus<-sourceSampDens_r_plus[use,]
  sourceRVals<-sourceRVals[use]
  }
  
  rp<-possibleResult$Theory$rp
  priorSampDens_r<-possibleResult$Theory$priorSampDens_r
  sampleLikelihood_r<-possibleResult$Theory$sampleLikelihood_r
  sampleLikelihood_r_show<-possibleResult$Theory$sampleLikelihood_r_show
  if (!is.null(sampleLikelihood_r_show)) {
  use<-rowSums(sampleLikelihood_r_show)>0
  sampleLikelihoodTotal_r<-possibleResult$Theory$sampleLikelihoodTotal_r
  # if (nrow(sampleLikelihood_r_show)==1) 
  #   sampleLikelihoodTotal_r<-sampleLikelihood_r_show[use,]*possibleResult$Theory$priorPopDens_r
  # else
  #   sampleLikelihoodTotal_r<-exp(colSums(log(sampleLikelihood_r_show[use,])))*possibleResult$Theory$priorPopDens_r
  }
  priorSampDens_r_null<-possibleResult$Theory$priorSampDens_r_null
  priorSampDens_r_plus<-possibleResult$Theory$priorSampDens_r_plus
  
  # make the histograms
  srAnalysis<-describePossibleSamples(possibleResult)
  sSimBins<-srAnalysis$sSimBins
  sSimDens<-srAnalysis$sSimDens
  prAnalysis<-describePossiblePopulations(possibleResult,possible)
  pSimBins<-prAnalysis$pSimBins
  pSimDens_slice<-prAnalysis$pSimDens_slice
  pSimDensRP<-prAnalysis$pSimDensRP
  pSimDensR<-prAnalysis$pSimDensR
  # switch (showType,
  #         "Samples"={
  #         },
  #         "Populations"={
  #         }
  # )
  
  # make the back wall population distributions
  rpw<-rp
  if (showType=="Samples") {
    rpw_dens<-possibleResult$Theory$sourcePopDens_r
  } else {
    rpw_dens<-possibleResult$Theory$priorPopDens_r
  }
  
  # make the back wall sample distributions
  rsw<-rs
  rsw_dens_plus<-possibleResult$Theory$sourceSampDens_r_plus
  if (!is.null(nrow(rsw_dens_plus)))
  rsw_dens_plus<-colSums(rsw_dens_plus)
  rsw_dens_null<-possibleResult$Theory$sourceSampDens_r_null
  if (is.element(world$PDF,c("Single","Double"))) rsw_dens_plus<-rsw_dens_plus-rsw_dens_null
  rsw_dens<-rsw_dens_plus+rsw_dens_null
  
  switch(braw.env$RZ,
         "r"={
         },
         "z"={
           rp<-atanh(rp)

           for (ci in 1:nrow(sourceSampDens_r_plus)) 
             sourceSampDens_r_plus[ci,]<-rdens2zdens(sourceSampDens_r_plus[ci,],rs)
           sourceRVals<-atanh(sourceRVals)
           
           sourceSampDens_r_null<-rdens2zdens(sourceSampDens_r_null,rs)
           sourceSampDens_r_total<-rdens2zdens(sourceSampDens_r_total,rs)
           rs<-atanh(rs)
           
           rsw_dens_plus<-rdens2zdens(rsw_dens_plus,rsw)
           rsw_dens_null<-rdens2zdens(rsw_dens_null,rsw)
           rsw_dens<-rdens2zdens(rsw_dens,rsw)
           rsw<-atanh(rsw)
           
           priorSampDens_r<-rdens2zdens(priorSampDens_r,rpw)
           rpw_dens<-rdens2zdens(rpw_dens,rpw)
           rpw<-atanh(rpw)
           
           xlim<-c(min(rp),max(rp))/axisScale+offRange # population
           ylim<-c(min(rs),max(rs))/axisScale+offRange
         }
  )

  zlim<-c(0,1)
  
  if (showType=="Samples" && length(sourceRVals)>8) draw_lower_limit=0.01
  else                       draw_lower_limit=0.000001
  if (logZ) {
    if (showType=="Samples") {
      zlim<-c(-2,0)
    } else {
      zlim<-c(-2,0)
    }
    draw_lower_limit<-zlim[1]
  }
  zlim[2]<-zlim[2]+diff(zlim)*0.2
  
  if (!is.null(sampleLikelihood_r)) {
    if (is.matrix(sampleLikelihood_r)) {
      old<-sampleLikelihood_r
      sampleLikelihood_r<-1
      rp_peak_local<-c()
      dens_at_peak_local<-c()
      for (i in 1:nrow(old)) {
        rp_stats<-densityFunctionStats(old[i,],rp)
        rp_peak_local<-c(rp_peak_local,rp_stats$peak)
        dens_at_peak_local<-c(dens_at_peak_local,rp_stats$dens_at_peak)
      }
    }
  rp_stats<-densityFunctionStats(sampleLikelihoodTotal_r,rp)
  rp_peak<-rp_stats$peak
  rp_ci<-rp_stats$ci
  dens_at_peak<-rp_stats$dens_at_peak
  }
  
  rpw_dens[rpw_dens>1 | is.na(rpw_dens)]<-1
    rsw_dens_plus<-rsw_dens_plus/max(rsw_dens,na.rm=TRUE)
    rsw_dens_null<-rsw_dens_null/max(rsw_dens,na.rm=TRUE)
  populationBackwall<-list(rpw=rpw,rpw_dens=rpw_dens,priorSampDens_r=priorSampDens_r,rp=rp)
  sampleBackwall<-list(rsw=rsw,rsw_dens_plus=rsw_dens_plus,rsw_dens_null=rsw_dens_null)
  
  if (view=="flat") {
    azimuth=90
    elevation=90
    label.z<-c()
  }
  if (is.element(view,c("3D","flat"))) {
    
    # make the floor
    braw.env$plotArea<-c(0,0,1,1)
    mapping<-mapping3D(azimuth,elevation,distance,xlim,ylim,zlim)
    g<-startPlot(xlim=c(-1,1),ylim=c(-1,1),
                 box="none",backC=braw.env$plotColours$graphC)
    pts<-rotate3D(data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)],z=c(0,0,0,0)+zlim[1]),mapping)
    g<-addG(g,dataPolygon(pts,colour=BoxColFloor,fill=BoxColFloor))
    # outside box            
    if (boxed)
      g<-addG(g,
              dataPolygon(rotate3D(data.frame(x=xlim[c(1,1,1,1)],
                                              y=ylim[c(1,1,2,2)],
                                              z=zlim[c(1,2,2,1)]
              ),
              mapping),
              colour=BoxColSamples,
              fill=BoxColSamples
              ),
              dataPolygon(rotate3D(data.frame(x=xlim[c(1,1,2,2)],
                                              y=ylim[c(2,2,2,2)],
                                              z=zlim[c(1,2,2,1)]
              ),
              mapping),
              colour=BoxColPopulations,
              fill=BoxColPopulations
              )
      )
    
    if (boxedDash)
      g<-addG(g,
              dataPath(rotate3D(data.frame(x=xlim[c(1,1,2)],
                                           y=ylim[c(1,2,2)],
                                           z=zlim[c(2,2,2)]
              ),
              mapping),
              colour=BoxCol
              ),
              dataPath(rotate3D(data.frame(x=xlim[c(1,1)],
                                           y=ylim[c(1,1)],
                                           z=zlim[c(1,2)]
              ),
              mapping),
              colour=BoxCol
              ),
              dataPath(rotate3D(data.frame(x=xlim[c(1,1)],
                                           y=ylim[c(2,2)],
                                           z=zlim[c(1,2)]
              ),
              mapping),
              colour=BoxCol
              ),
              dataPath(rotate3D(data.frame(x=xlim[c(2,2)],
                                           y=ylim[c(2,2)],
                                           z=zlim[c(1,2)]
              ),
              mapping),
              colour=BoxCol
              )
      )
    
    
    tick_grow<-3
    tick_more<-2
    xtick_length<-0.03*diff(xlim)
    ytick_length<-0.03*diff(ylim)
    
    if (!is.null(label.z)) {
      # z-axis 
      if (doBothZAxes) iaRange<-1:2
      else switch(showType,"Samples"={iaRange<-1},"Populations"={iaRange<-2})
      for (ia in iaRange) {
      if (ia==1) {
        label.z<-"Probability Density"
        zx<-xlim[1]
        zy<-ylim[1]
        zyt<- -1
        zxt<-0
      } else {
        label.z<-"Likelihood"
        zx<-xlim[2]
        zy<-ylim[2]
        zyt<-0
        zxt<- 1
      }
      g<-addG(g,dataPath(rotate3D(data.frame(x=c(zx,zx),
                    y=c(zy,zy),
                    z=zlim),mapping),colour="#000000")
      )
      # short ticks
      if (boxed) {
        plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)
      } else {
        plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)
      }
      tick.z.start <- rotate3D(data.frame(x=zx,y=zy,z=plot_ticks), mapping)
      tick.z.end <- rotate3D(data.frame(x=zx+zxt*xtick_length,y=zy+zyt*ytick_length,z=plot_ticks), mapping)
      for (i in 1:length(tick.z.start$x))
      g<-addG(g,dataPath(data.frame(x=c(tick.z.start$x[i],tick.z.end$x[i]),y=c(tick.z.start$y[i],tick.z.end$y[i]))))
      # long ticks
      long_ticks<-seq(zlim[1],zlim[2],diff(zlim)/2)
      tick.z.start <- rotate3D(data.frame(x=zx,y=zy,z=long_ticks), mapping)
      tick.z.end <- rotate3D(data.frame(x=zx+zxt*xtick_length,y=zy+zyt*ytick_length,z=long_ticks), mapping)
      for (i in 1:length(tick.z.start$x))
        g<-addG(g,dataPath(data.frame(x=c(tick.z.start$x[i],tick.z.end$x[i]),y=c(tick.z.start$y[i],tick.z.end$y[i]))))
      # label
      pos.z<-rotate3D(data.frame(x=zx+zxt*diff(xlim)*0.08,y=zy+zyt*diff(ylim)*0.08,z=mean(zlim)),mapping)
      rotate.z=rotate3D(data.frame(x=c(zx,zx),
                       y=c(zy,zy),
                       z=zlim),mapping)
      rotate.z<- 180-zyt*atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296+zxt*atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296
      g<-addG(g,dataText(data.frame(x=pos.z$x,y=pos.z$y),label=label.z,angle=rotate.z,hjust=0.5,size=0.7,fontface="bold"))
    }
    }
    
    # x ticks
    g<-addG(g,dataPath(rotate3D(data.frame(x=xlim,
                                           y=c(ylim[1],ylim[1]),
                                           z=c(zlim[1],zlim[1])),mapping),colour="#000000")
    )
    plot_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
    long_ticks<-seq(ceil(xlim[1]*2),floor(xlim[2]*2))/2
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*5),floor(xlim[2]*5))/5
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
    
    # short ticks  
    tick.x.start <- rotate3D(data.frame(x=plot_ticks, y=ylim[1], z=zlim[1]), mapping)
    tick.x.end <- rotate3D(data.frame(x=plot_ticks, y=ylim[1]-ytick_length, z=zlim[1]), mapping)
    for (i in 1:length(tick.x.start$x))
      g<-addG(g,dataPath(data.frame(x=c(tick.x.start$x[i],tick.x.end$x[i]),y=c(tick.x.start$y[i],tick.x.end$y[i]))))
    # long ticks
    tick.x.start <- rotate3D(data.frame(x=long_ticks, y=ylim[1], z=zlim[1]), mapping)
    tick.x.end <- rotate3D(data.frame(x=long_ticks, y=ylim[1]-ytick_length*tick_more, z=zlim[1]), mapping)
    for (i in 1:length(tick.x.start$x))
      g<-addG(g,dataPath(data.frame(x=c(tick.x.start$x[i],tick.x.end$x[i]),y=c(tick.x.start$y[i],tick.x.end$y[i]))))
    # tick labels
    ticks.x<-rotate3D(data.frame(x=long_ticks,y=ylim[1]-ytick_length*tick_grow,z=zlim[1]),mapping)
    g<-addG(g,dataText(data.frame(x=ticks.x$x,y=ticks.x$y),long_ticks,hjust=1,vjust=0.5,size=0.7))

    # y ticks
    g<-addG(g,dataPath(rotate3D(data.frame(x=c(xlim[2],xlim[2]),
                                           y=ylim,
                                           z=c(zlim[1],zlim[1])),mapping),colour="#000000")
    )
    plot_ticks<-seq(ceil(ylim[1]*10),floor(ylim[2]*10))/10
    long_ticks<-seq(ceil(xlim[1]*2),floor(xlim[2]*2))/2
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*5),floor(xlim[2]*5))/5
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
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
    
    switch(braw.env$RZ,
           "r"={
             label.x<-"r[p]"
             label.y<-"r[s]"
           },
           "z"={
             label.x<-"z[p]"
             label.y<-"z[s]"
           }
    )

    pos.x<-rotate3D(data.frame(x=sum(xlim)/2,y=ylim[1]-ytick_length*tick_grow*2,z=zlim[1]-0.1),mapping)
    g<-addG(g,dataText(pos.x,label.x,hjust=0.5,vjust=0.5,fontface="bold"))

    pos.y<-rotate3D(data.frame(x=xlim[2]+xtick_length*tick_grow*2,y=sum(ylim)/2,z=zlim[1]-0.1),mapping)
    g<-addG(g,dataText(pos.y,label.y,hjust=0.5,vjust=0.5,fontface="bold"))
  }

    # graph frame
  switch (view,
          "3D"= {
            
            # lines on the floor
            # general 
            if (doFloorZeroLines) {
              g<-addG(g,
                      dataPath(rotate3D(data.frame(x=xlim,y=c(0,0),z=c(0,0)+zlim[1]),mapping),linetype="dotted"),
                      dataPath(rotate3D(data.frame(x=c(0,0),y=ylim,z=c(0,0)+zlim[1]),mapping),linetype="dotted")
              )
            }
            # populations 
            # if (!cutaway && !is.null(possible$targetSample)) {
            #   lines(trans3d(x=xlim,y=c(0,0)+possible$targetSample,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=3)
            # }
            if (showType=="Populations" && !is.null(possible$targetPopulation)) {
              g<-addG(g,
                      dataPath(rotate3D(data.frame(x=c(0,0)+possible$targetPopulation,y=ylim,z=c(0,0)+zlim[1]),mapping),colour=colPop,linetype="dotted")
              )
            }
            if (showType=="Samples" && !is.null(possible$targetSample))
                g<-addG(g,
                        dataPath(rotate3D(data.frame(x=xlim,y=c(0,0)++possible$targetSample,z=c(0,0)+zlim[1]),mapping),colour="#000000",linetype="dotted")
                )
            
            if (showType=="Samples" && !any(is.na(sourceRVals)) && length(sourceRVals)<8) {
              for (s in sourceRVals)
                g<-addG(g,
                        dataPath(rotate3D(data.frame(x=c(0,0)+s,y=ylim,z=c(0,0)+zlim[1]),mapping),colour=colVline,linetype="dotted")
                )
            }
            
            # populations 
            if (showType=="Populations" && !is.null(possible$targetSample)) {
              # show peak and CIs on floor
              if (doFloorPeakLines) {
                if (rp_peak==0 && possible$UsePrior!="none") colHere<-colNullS else colHere<-colDistS
                g<-addG(g,
                        dataPath(rotate3D(data.frame(x=c(rp_peak,rp_peak),y=ylim,z=c(0,0)+zlim[1]),mapping),colour=colHere,linewidth=0.55)
                )
                # lines(trans3d(x=c(0,0),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=3)
                if (doCILines) {
                  g<-addG(g,
                          dataPath(rotate3D(data.frame(x=c(rp_ci[1],rp_ci[1]),y=ylim,z=c(0,0)+zlim[1]),mapping),colour=colHere),
                          dataPath(rotate3D(data.frame(x=c(rp_ci[2],rp_ci[2]),y=ylim,z=c(0,0)+zlim[1]),mapping),colour=colHere)
                  )
                }
              }
              # show rp==rs on floor
              # if (world$PDF!="Single"){
                # lines(trans3d(x=c(sRho[1],sRho[1]),y=ylim,z=c(0,0),pmat=mapping),col=colPdark,lwd=1,lty=3)
              # }
            }
            
            if (walls>0) {
              si=1;
            if (!is.na(sRho)) {
              za<-approx(rs,sampleBackwall$rsw_dens_null,sRho[si])$y
              zb<-approx(rs,sampleBackwall$rsw_dens_plus,sRho[si])$y
              llrNull<-log(za/zb)
            }
            
            if (axisScale<=100) {
              
              if (is.element(walls,c(2,3))) {
              # population wall
              x<-populationBackwall$rpw
              y<-x*0+ylim[2]
              z<-populationBackwall$rpw_dens
              z<-z/max(z)
              if (max(z)==min(z)) z<-z/2
              if (logZ) z<-log10(z)
              g<-drawDistribution(x,y,z,xlim,ylim,zlim,mapping,colPsum,"#000000",1,draw_lower_limit=0,g)
              # use<- which((x>=xlim[1]) & (x<=xlim[2]) & (z>=zlim[1]))
              # g<-addG(g,
              #         dataPolygon(rotate3D(data.frame(x=c(x[use[1]],x[use],x[use[length(use)]]),y=c(y[use[1]],y[use],y[use[length(use)]]),z=c(zlim[1],z[use],zlim[1])*wallHeight),mapping),fill=colP,alpha=0.25)
              # )
              
              if (showType=="Populations" && showJointLk && !any(is.na(populationBackwall$priorSampDens_r))) {
                # show the joint likelihood function
                x<-populationBackwall$rp
                y<-x*0+ylim[2]
                z<-sampleLikelihoodTotal_r
                z<-z/max(z)
                if (logZ) z<-log10(z)
                use<- which((x>=xlim[1]) & (x<=xlim[2]) & (z>=zlim[1]))
                g<-addG(g,
                        dataPolygon(rotate3D(data.frame(x=c(x[use[1]],x[use],x[use[length(use)]]),y=c(y[use[1]],y[use],y[use[length(use)]]),z=c(zlim[1],z[use],zlim[1])*wallHeight),mapping),fill=colP,alpha=1)
                )
              }
              if (showType=="Populations" && !is.null(possible$targetSample)) {
                # show peak likelihood on population back wall
                if (rp_peak==0) colHere<-colNullS else colHere<-"red"
                dens_rp_peak<-approx(populationBackwall$rpw,sampleLikelihoodTotal_r,rp_peak)$y
                if (logZ)  dens_rp_peak<-log10(dens_rp_peak)
                if (showJointLk) yoff<-ylim[2] else yoff<-possible$targetSample
                g<-addG(g,
                        dataPath(rotate3D(data.frame(x=c(0,0)+rp_peak,y=c(0,0)+yoff,z=c(zlim[1],wallHeight)),mapping),
                                 colour=colHere,linewidth=0.55)
                )
                for (i in 1:length(rp_peak_local)) {
                  g<-addG(g,
                          dataPath(rotate3D(data.frame(x=c(0,0)+rp_peak_local[i],
                                                       y=ylim,z=c(0,0)),mapping),
                                   colour=colHere,linewidth=0.15)
                  )
                  dens_rp_peak_local<-approx(populationBackwall$rpw,sampleLikelihoodTotal_r,rp_peak_local[i])$y
                  dens_rp_peak_local<-dens_rp_peak_local/dens_rp_peak*wallHeight
                  if (logZ)  dens_rp_peak_local<-log10(dens_rp_peak_local)
                  g<-addG(g,
                          dataPath(rotate3D(data.frame(x=c(0,0)+rp_peak_local[i],
                                                       y=c(0,0)+yoff,
                                                       z=c(zlim[1],dens_rp_peak_local)),mapping),
                                   colour=colHere,linewidth=0.55)
                  )
                }
                # show population likelihood on population back wall
                if (!is.null(possible$targetPopulation)) {
                dens_target<-approx(populationBackwall$rpw,populationBackwall$rpw_dens,possible$targetPopulation)$y
                if (logZ)  dens_target<-log10(dens_target)
                g<-addG(g,
                        dataPath(rotate3D(data.frame(x=c(0,0)+possible$targetPopulation,y=c(0,0)+ylim[2],z=c(zlim[1],dens_target)*wallHeight),mapping),colour=colPop,linetype="dotted")
                )
                }
              }
              }
              
              if (is.element(walls,c(1,3))) {
              # sample wall
              # sampling distribution
              y<-sampleBackwall$rsw
              x<-y*0+xlim[1]
              ztotal<-sampleBackwall$rsw_dens_plus+sampleBackwall$rsw_dens_null
              if (normSampDist) {
                ztotal<-ztotal/sum(ztotal,na.rm=TRUE)
                zgain<-1/max(ztotal,na.rm=TRUE)
                ztotal<-ztotal*zgain
              }
              if (logZ) {
                ztotal<-log10(ztotal)
                ztotal[ztotal<zlim[1]]<-zlim[1]
              }
              g<-drawDistribution(x,y,ztotal,xlim,ylim,zlim,mapping,colSsum,"#000000",1,draw_lower_limit=0,g)
                # split into 2 parts  
                if (possible$source$On && possible$source$pRplus<1){
                  if (!any(is.na(sampleBackwall$rsw_dens_null))) {
                    znull <- sampleBackwall$rsw_dens_null
                  } else {
                    znull<-0
                  }
                  zplus<-sampleBackwall$rsw_dens_plus
                  if (normSampDist) {
                    znull<-znull/sum(znull,na.rm=TRUE)
                    zplus<-zplus/sum(zplus,na.rm=TRUE)
                    znull<-znull*zgain
                    zplus<-zplus*zgain
                  }
                  if (logZ) {
                    znull<-log10(znull)
                    znull[znull<zlim[1]]<-zlim[1]
                    zplus<-log10(zplus)
                    zplus[zplus<zlim[1]]<-zlim[1]
                  }
                  if (possible$source$pRplus<1) {
                    g<-drawDistribution(x,y,znull*wallHeight,xlim,ylim,zlim,mapping,NA,colNullS,1,draw_lower_limit,g)
                  }
                  g<-drawDistribution(x,y,zplus*wallHeight,xlim,ylim,zlim,mapping,NA,colDistS,1,draw_lower_limit,g)
                }
              # }
              
              # vertical lines
              if (showType=="Samples" && possible$showTheory) {
                # show probability density on sample back wall
                if (!is.na(sRho)){
                  for (si in 1:length(sRho)) {
                    z<-approx(sampleBackwall$rsw,ztotal,sRho[si])$y
                    z[z<0]<-0
                    if (logZ) z<-log10(z)
                    if (z>=zlim[1])
                    g<-addG(g,
                            dataPath(rotate3D(data.frame(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],z)*wallHeight),mapping),colour=colVline)
                    )
                  }
                si=1;
                za<-approx(rs,sampleBackwall$rsw_dens_null,sRho[si])$y
                zb<-approx(rs,sampleBackwall$rsw_dens_plus,sRho[si])$y
                llrNull<-log(za/zb)
                if (logZ) {
                  za<-log10(za)
                  zb<-log10(zb)
                  za[za<zlim[1]]<-zlim[1]
                  zb[zb<zlim[1]]<-zlim[1]
                }
                if (za>=zb) {
                  g<-addG(g,
                          dataPath(rotate3D(data.frame(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],za)*wallHeight),mapping),colour=colNullS,linewidth=0.5),
                          dataPath(rotate3D(data.frame(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight),mapping),colour=colDistS,linewidth=0.5)
                  )
                } else {
                  g<-addG(g,
                          dataPath(rotate3D(data.frame(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight),mapping),colour=colDistS,linewidth=0.5),
                          dataPath(rotate3D(data.frame(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],za)*wallHeight),mapping),colour=colNullS,linewidth=0.5)
                  )
                }
                }
              }
            }
            }
            }
            
            # main distributions            
            if (is.na(sRho)) theoryAlpha=0.6 else theoryAlpha=0.3
            simAlpha<-1
            switch (showType,
                    "Samples"={
                      # draw simulations
                      
                      # sample wall
                      if (!is.null(pSimDensR)) {
                      x<-as.vector(matrix(c(pSimBins,pSimBins),2,byrow=TRUE))
                      gainSim<-sum(pSimDensR)*diff(pSimBins[1:2])
                      gainTheory<-sum(rsw_dens)*(rsw[2]-rsw[1])
                      densS<-pSimDensR/(gainSim/gainTheory)
                      if (max(densS)>1.2) {densS<-densS/max(densS)*1.2}
                      yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                      if (logZ) yS<-log10(yS)
                      g<-addG(g,
                              dataPolygon(rotate3D(data.frame(x=x*0+xlim[1],y=x,z=yS*wallHeight),
                                                   mapping),fill=colSdark,alpha=0.25,colour="none")
                      )
                      }
                      
                      
                      if (length(sourceRVals)>2 && length(sourceRVals)<8) {
                        theoryAlpha<-theoryAlpha*0.6
                        simAlpha<-0.95
                      }
                      if (is.element(world$PDF,c("Single","Double"))) {
                        pgain<-1
                      } else {
                      pgain<-possible$source$pRplus
                      if (length(sourceRVals)==2) {
                        pgain<-max(c(possible$source$pRplus,1-possible$source$pRplus))
                      } 
                      }
                      # prepare simulations first
                      if (!is.null(sSimDens)) {
                        theoryAlpha<-0.25
                        
                        simgain<-mean(sourceSampDens_r_plus)/mean(sSimDens)
                        sSimDens<-sSimDens*simgain*pgain
                        if (!is.na(sRho) && cutaway) {
                          waste<-sum(sSimBins<=min(sRho))
                          use_s<-(waste):length(sSimBins)
                          sSimBins<-sSimBins[use_s]
                          sSimBins[1]<-min(sRho)
                          use_s<-use_s[1:(length(use_s)-1)]
                        } else {
                          if (!is.matrix(sSimDens)) {
                            sSimDens<-t(sSimDens)
                          } 
                          use_s<-(1:ncol(sSimDens))
                        }
                      } 
                      
                      # we interleave simulations and theory (because no hidden line removal)
                      cutZ<-c()
                      if (length(sourceRVals)>10) {
                        useVals<-c(rev(seq(ceiling(length(sourceRVals)/2),continuousSampling+1,-continuousSampling)),
                                       seq(ceiling(length(sourceRVals)/2),length(sourceRVals)-continuousSampling,continuousSampling)
                        )
                        useVals<-sort(unique(useVals))
                      } else useVals<-order(sourceRVals)
                      tgain<-max(sourceSampDens_r_plus[useVals,])
                      sourceSampDens_r_plus<-sourceSampDens_r_plus/tgain
                      for (i in order(sourceRVals)) {
                        # draw simulations
                        if (!is.null(sSimDens)){
                          y1<-as.vector(matrix(c(sSimBins,sSimBins),2,byrow=TRUE))
                          z1<-as.vector(matrix(c(sSimDens[i,use_s],sSimDens[i,use_s]),2,byrow=TRUE))
                          if (logZ) z1<-log10(z1)
                          z1<-c(zlim[1],z1,zlim[1])
                          z1[z1<zlim[1]]<-zlim[1]
                          use<-which(y1>=ylim[1] & y1<=ylim[2])
                          g<-addG(g,
                                  dataPolygon(rotate3D(data.frame(x=rep(sourceRVals[i],length(use)),
                                                                  y=y1[use],
                                                                  z=z1[use]),mapping),
                                              fill=colSsim,alpha=simAlpha)
                          )
                        }
                        
                        # draw theory
                        if (possible$showTheory){
                          if (any(i==useVals)) {
                            z_use<-sourceSampDens_r_plus[i,]*pgain
                            if (!is.na(sRho) && cutaway) {
                              use<-max(which(rs<min(sRho)))
                              z_use[use]<-approx(rs,z_use,min(sRho))$y
                              z_use[1:use-1]<-0
                            } 
                            if (!is.na(sRho)) {
                              use<-max(which(rs<min(sRho)))
                              z_use[use]<-approx(rs,z_use,min(sRho))$y
                              z_use[(use+1):length(z_use)]<-0
                            }
                            if (sourceRVals[i]==min(sourceRVals) || is.na(sRho)) 
                              alpha=1 
                            else 
                              alpha=theoryAlpha
                            if (logZ) z_use<-log10(z_use)
                            col<-colS
                            if (showSig) col<-braw.env$plotColours$infer_nsigC
                            g<-drawDistribution(rep(sourceRVals[i],length(rs)),rs,z_use*sampDensGain,xlim,ylim,zlim,
                                                mapping,col,"#000000",alpha,draw_lower_limit,g)
                            
                          if (!is.na(sRho)) {
                            if (cutaway) alpha<-1 else alpha<-theoryAlpha*2
                            z_use<-sourceSampDens_r_plus[i,]*pgain
                            use<-max(which(rs<max(sRho)))
                            z_use[use]<-approx(rs,z_use,max(sRho))$y
                            z_use[1:use-1]<-0
                            if (logZ) z_use<-log10(z_use)
                            col<-colS
                            if (showSig) col<-braw.env$plotColours$infer_sigC
                            g<-drawDistribution(rep(sourceRVals[i],length(rs)),rs,z_use*sampDensGain,xlim,ylim,zlim,
                                                mapping,col,"#000000",alpha,draw_lower_limit,g)
                            
                          }
                          if (showP>0 && !is.na(sRho)) {
                            for (j in 1:length(sRho)) {
                            rcrit<-sRho[j]
                            if (sRho>0)  use<-which(rs>=rcrit)
                            else         use<-which(rs<=rcrit)
                            g<-addG(g,
                                    dataPolygon(rotate3D(data.frame(x = rep(sourceRVals[i],length(use)+2), 
                                                                    y = rs[use[c(1,1:length(use),length(use))]], 
                                                                    z = c(0,z_use[use],0)),
                                                         mapping),fill=braw.env$plotColours$infer_sigC)
                            )
                            if (showP>1) {
                              if (sRho[j]>0)  use<-which(rs<= -rcrit)
                              else         use<-which(rs>= -rcrit)
                              g<-addG(g,
                                      dataPolygon(rotate3D(data.frame(x = rep(sourceRVals[i],length(use)+2), 
                                                                      y = rs[use[c(1,1:length(use),length(use))]], 
                                                                      z = c(0,z_use[use],0)),
                                                           mapping),fill=braw.env$plotColours$infer_sigC)
                              )
                            }
                            }
                          }
                          }
                        }

                        # vertical lines on main distribution
                        if (!isempty(sRho)){
                          for (si in 1:length(sRho)) {
                            if (any(sampleLikelihood_r_show[si,]!=0)){
                            z<-approx(rs,sourceSampDens_r_plus[i,]*pgain,sRho[si])$y
                            if (logZ) {
                              z<-log10(z)
                              z[z<zlim[1]]<-zlim[1]
                            }
                            cutZ<-c(cutZ,z)
                            if (!cutaway && doConnecting && length(sourceRVals)>5 && i<length(sourceRVals)) {
                              z1<-approx(rs,sourceSampDens_r_plus[i+1,],sRho[si])$y
                              z1<-z1*pgain
                              if (logZ) {
                                z1<-log10(z1)
                                z1[z1<zlim[1]]<-zlim[1]
                              }
                              g<-addG(g,
                                      dataPolygon(rotate3D(data.frame(x=c(sourceRVals[i],sourceRVals[i+1],sourceRVals[i+1],sourceRVals[i]),
                                                                      y=c(sRho[si],sRho[si],sRho[si],sRho[si]),
                                                                      z=c(z,z1,zlim[1],zlim[1])),mapping),fill=colP,colour=colP),
                                      dataPath(rotate3D(data.frame(x=c(sourceRVals[i],sourceRVals[i+1]),y=c(sRho[si],sRho[si]),z=c(z,z1)),mapping),colour=colVline)
                              )
                            }
                          }
                          }
                        }
                      }
                      if (cutaway && endFace) {
                        # main distribution
                        cutZ[cutZ<draw_lower_limit]<-zlim[1]
                        use<-which(cutZ>zlim[1])
                        g<-addG(g,
                                dataPolygon(rotate3D(data.frame(x = c(sourceRVals[1],sourceRVals,sourceRVals[length(sourceRVals)]),
                                                                y = c(0,sourceRVals*0,0)+sRho[1], 
                                                                z = c(zlim[1],cutZ,zlim[1])),mapping),fill=colP,colour=colP,alpha=0.8),
                                dataPath(rotate3D(data.frame(x = c(sourceRVals[use[1]],sourceRVals[use],sourceRVals[use[length(use)]]),
                                                             y = c(0,use*0,0)+sRho[1], 
                                                             z = c(zlim[1],cutZ[use],zlim[1])),mapping),colour="#000000")
                        )
                      }
                    },
                    "Populations"={
                      # draw simulations
                      if (!is.null(prAnalysis)) {
                        x<-as.vector(matrix(c(pSimBins,pSimBins),2,byrow=TRUE))
                        
                        # population wall
                        if (sum(pSimDensRP>0)>1) {
                        gainSim<-sum(pSimDensRP)*diff(pSimBins[1:2])
                        gainTheory<-sum(rpw_dens)*diff(rpw[1:2])
                        densP<-pSimDensRP/(gainSim/gainTheory)
                        if (max(densP)>1.2) {densP<-densP/max(densP)*1.2}
                        yP<-c(0,as.vector(matrix(c(densP,densP),2,byrow=TRUE)),0)
                        if (logZ) yP<-log10(yP)
                        g<-addG(g,
                                dataPolygon(rotate3D(data.frame(x=x,y=x*0+ylim[2],z=yP*wallHeight),
                                                     mapping),fill=colPdark,alpha=0.35,colour="none")
                        )
                        }
                        
                        # sample wall
                        gainSim<-sum(pSimDensR)*diff(pSimBins[1:2])
                        gainTheory<-sum(rsw_dens)*(rsw[2]-rsw[1])
                        densS<-pSimDensR/(gainSim/gainTheory)
                        if (max(densS)>1.2) {densS<-densS/max(densS)*1.2}
                        yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                        if (logZ) yS<-log10(yS)
                        g<-addG(g,
                                dataPolygon(rotate3D(data.frame(x=x*0+xlim[1],y=x,z=yS*wallHeight),
                                                     mapping),fill=colSdark,alpha=0.25,colour="none")
                        )

                        #slice of interest
                        si=1
                        gainSim<-sum(pSimDens_slice)*diff(pSimBins[1:2])
                        gainTheory<-sum(sampleLikelihood_r_show)*diff(rpw[1:2])
                        densRS<-pSimDens_slice/(gainSim/gainTheory)
                        # dens<-dens/max(dens,na.rm=TRUE)
                        # if (max(dens)>1.2) {dens<-dens/max(dens)*1.2}
                        yRS<-c(0,as.vector(matrix(c(densRS,densRS),2,byrow=TRUE)),0)
                        if (logZ) yRS<-log10(yRS)
                        if (sum(pSimDensRP>0)>1) {
                          g<-addG(g,
                                dataPolygon(rotate3D(data.frame(x=x,y=x*0+sRho[si],z=yRS),
                                                     mapping),fill=colPsim,colour=colPsim)
                        )
                        }
                      }
                      # draw theory main distribution & lines
                      if (possible$showTheory){
                        theoryAlpha=0.75/sqrt(length(sRho))
                        if (!is.null(possible$targetSample)) {
                          rd<-sampleLikelihood_r_show
                          rd<-rd/max(rd)
                          rdT<-sampleLikelihoodTotal_r
                          rdT<-rdT/max(rdT)
                          if (logZ) {
                            rd<-log10(rd)
                            rd[rd<zlim[1]]<-zlim[1]
                            rdT<-log10(rdT)
                            rdT[rdT<zlim[1]]<-zlim[1]
                          }
                          if (showNull && possible$UsePrior!="none") {
                            rd<-(rd-zlim[1])
                            rd<-zlim[1]+rd/max(rd)*(zb-zlim[1])
                            rdT<-(rdT-zlim[1])
                            rdT<-zlim[1]+rdT/max(rdT)*(zb-zlim[1])
                          }
                          if (!is.null(sampleLikelihood_r_show)){
                            # main distribution
                            col<-colP
                            # if (possible$sigOnlyCompensate) col<-braw.env$plotColours$infer_sigC
                            for (si in order(-sRho)) {
                              use<-rp>=xlim[1] & rp<=xlim[2] & rd[si,]>draw_lower_limit
                              rp_use<-rp[use]
                              dens_use<-rd[si,use]
                              dens_useT<-rdT[si,use]
                              if (is.null(prAnalysis)) {
                                if (showEvLk)
                                  g<-addG(g,
                                        dataPolygon(rotate3D(data.frame(x = c(rp_use[1],rp_use,rp_use[length(rp_use)]),
                                                                        y = c(0,rp_use*0,0)+sRho[si], 
                                                                        z = c(zlim[1],dens_use,zlim[1])),
                                                             mapping),fill=col,alpha=highTransparency)
                                )
                                g<-addG(g,
                                        dataPolygon(rotate3D(data.frame(x = c(rp_use[1],rp_use,rp_use[length(rp_use)]),
                                                                        y = c(0,rp_use*0,0)+sRho[si], 
                                                                        z = c(zlim[1],dens_useT,zlim[1])),
                                                             mapping),fill=col,alpha=theoryAlpha)
                                )
                              } else {
                                g<-addG(g,
                                        dataPolygon(rotate3D(data.frame(x = c(rp_use[1],rp_use,rp_use[length(rp_use)]), 
                                                                        y = c(0,rp_use*0,0)+sRho[si], 
                                                                        z = c(zlim[1],dens_use,zlim[1])),
                                                             mapping),fill=col,alpha=highTransparency)
                                )
                              }
                              # vertical lines on main distribution
                              if (doPeakLine) {
                                if (showNull && possible$UsePrior!="none") {
                                  znullLk<-zlim[1]+(max(rd[si,])-zlim[1])*(za-zlim[1])/(zb-zlim[1])
                                  g<-addG(g,
                                          dataPath(rotate3D(data.frame(x=c(0,0),y=c(sRho[si],sRho[si]),z=c(zlim[1],znullLk)),
                                                            mapping),colour=colNullS,linewidth=0.5)
                                  )
                                }
                                if (showEvLk)
                                g<-addG(g,
                                        dataPath(rotate3D(data.frame(x=c(rp_peak_local[si],rp_peak_local[si]),y=c(sRho[si],sRho[si]),z=c(zlim[1],dens_at_peak_local[si]-0.01)),
                                                          mapping),colour=colDistS,linewidth=0.25)
                                )
                                g<-addG(g,
                                        dataPath(rotate3D(data.frame(x=c(rp_peak,rp_peak),y=c(sRho[si],sRho[si]),z=c(zlim[1],dens_at_peak_local[si]-0.01)),
                                                          mapping),colour=colDistS,linewidth=0.5)
                                )
                                if (doCILines) {
                                  g<-addG(g,
                                          dataPath(rotate3D(data.frame(x=c(rp_ci[1],rp_ci[1]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_ci[1])$y-0.01)),
                                                            mapping),colour=colP,linewidth=0.5,linetype="dotted"),
                                          dataPath(rotate3D(data.frame(x=c(rp_ci[2],rp_ci[2]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_ci[2])$y-0.01)),
                                                            mapping),colour=colP,linewidth=0.5,linetype="dotted")
                                  )
                                }
                                if (!is.null(possible$targetPopulation)) {
                                  g<-addG(g,
                                          dataPath(rotate3D(data.frame(x=c(possible$targetPopulation,possible$targetPopulation),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],possible$targetPopulation)$y-0.01)),
                                                            mapping),colour=colPop,linetype="dotted")
                                  )
                                }
                                if (doSampleLine && world$PDF!="Single"){
                                  g<-addG(g,
                                          dataPath(rotate3D(data.frame(x=c(sRho[si],sRho[si]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],sRho[si])$y-0.01)),
                                                            mapping),colour=colP,linewidth=0.5,linetype="dotted")
                                  )
                                }
                              }
                            }
                          }
                          
                        }
                      }
                    }
            )
            
            # text annotations
            if (doTextResult && !is.na(sRho)) {
              if (showType=="Samples") position<-c(xlim[1],mean(ylim))
              else                     position<-c(mean(xlim),ylim[2])
              # samples
              zoff<-diff(zlim)*1.2
              if (!is.na(sRho) && length(sRho)<=10) {
                use<-order(sRho)
                  h<-c(paste0(braw.env$RZ,"[s]"),
                       paste0("= ",paste(brawFormat(sRho[use],digits=2),collapse=", "))
                  )
                pos<-rotate3D(data.frame(x=position[1],y=position[2],z=zlim[1]+zoff), mapping)
                for (hi in 1:length(h)) {
                  g<-addG(g,
                          dataText(pos,
                                   h[hi],fontface="bold",
                                   hjust=0,size=0.6,colour=colPdark)
                  )
                  pos$x<-pos$x+diff(xlim)*0.05
                }
                zoff<-zoff-diff(zlim)*0.06
                h<-c(paste0("n"),
                     paste0("=  ",paste(brawFormat(n[use],digits=2),collapse=",    "))
                )
                pos<-rotate3D(data.frame(x=position[1],y=position[2],z=zlim[1]+zoff), mapping)
                for (hi in 1:length(h)) {
                  g<-addG(g,
                          dataText(pos,
                                   h[hi],fontface="bold",
                                   hjust=0,size=0.6,colour=colPdark)
                  )
                  pos$x<-pos$x+diff(xlim)*0.05
                }
              }
              # mle population
              # if (showType=="Populations") {
                h<-c(paste0(braw.env$RZ,"[mle]"),
                     paste0("= ",brawFormat(rp_peak,digits=3))
                )
                zoff<-zoff-diff(zlim)*0.06
                pos<-rotate3D(data.frame(x=position[1],y=position[2],z=zlim[1]+zoff), mapping)
                for (hi in 1:length(h)) {
                  g<-addG(g,
                          dataText(pos,
                                   h[hi],fontface="bold",
                                   hjust=0,size=0.6,colour=colPdark)
                  )
                  pos$x<-pos$x+diff(xlim)*0.05
                }
              # }
              # zoff<-zoff-diff(zlim)*0.085
              # llrA<-dnorm(atanh(sRho),mean=atanh(sRho),sd=1/sqrt(n-3))
              # llr0<-dnorm(0,mean=atanh(sRho),sd=1/sqrt(n-3))
              # llr_0_rs<-log(llrA/llr0)
              # g<-addG(g,
              #         dataText(rotate3D(data.frame(x=xlim[2],y=ylim[2],z=zlim[1]+zoff),
              #                           mapping),
              #                  paste0("sLLR(",braw.env$RZ,"[s]/",braw.env$RZ,"[0])=",brawFormat(llr_0_rs,digits=3)),
              #                  hjust=1,size=0.8,colour=colPdark)
              # )
              #     if (possible$UsePrior!="none") {
              #       zoff<-zoff-diff(zlim)*0.085
              #       # lb<-"prior: "
              #       # switch(possibleResult$prior$PDF,
              #       #        "Uniform"=lb<-paste0(lb,"Uniform(",possibleResult$prior$RZ,")"),
              #       #        "Single"=lb<-paste0(lb,"Single(",possibleResult$prior$RZ,"=",brawFormat(possibleResult$prior$PDFk),")"),
              #       #        "Double"=lb<-paste0(lb,"Double(",possibleResult$prior$RZ,"=",brawFormat(possibleResult$prior$PDFk),")"),
              #       #        "Exp"=lb<-paste0(lb,"Exp(",possibleResult$prior$RZ,"/",brawFormat(possibleResult$prior$PDFk),")"),
              #       #        "Gauss"=lb<-paste0(lb,"Gauss(",possibleResult$prior$RZ,"/",brawFormat(possibleResult$prior$PDFk),")")
              #       # )
              #       # lb<-paste0(lb,";p(null)=",brawFormat(possibleResult$prior$pRplus,digits=3))
              #       # zoff<-zoff-diff(zlim)*0.06*2
              #       # text(trans3d(x=xlim[2],y=ylim[2],z=zlim[1]+zoff,pmat=mapping),
              #       #      labels=lb,
              #       #      col=colPdark,adj=1,cex=0.9)
              #   
              # # llr 
              #       zoff<-zoff-diff(zlim)*0.085
              #       llr_0_mle<-log(approx(rs,colSums(priorSampDens_r_plus),sRho)$y/approx(rs,priorSampDens_r_null,sRho)$y)
              #       g<-addG(g,
              #               dataText(rotate3D(data.frame(x=xlim[2],y=ylim[2],z=zlim[1]+zoff),
              #                                 mapping),
              #                        paste0("dLLR(",braw.env$RZ,"[+]/",braw.env$RZ,"[0])=",brawFormat(llr_0_mle,digits=3)),
              #                        hjust=1,size=0.8,colour=colPdark)
              #       )
              #     }
            }
            
            # finish off plot box
            if (boxedDash){
              g<-addG(g,
                      dataPath(rotate3D(data.frame(x=c(xlim[1], xlim[2], xlim[2]),
                                                   y=c(ylim[1], ylim[1], ylim[2]),
                                                   z=c(1,1,1)*zlim[2]),
                                        mapping),colour=BoxCol),
                      dataPath(rotate3D(data.frame(x=c(xlim[2],xlim[2]),y=c(ylim[1],ylim[1]),z=zlim),
                                        mapping),colour=BoxCol)
              )
            }
            if (braw.env$graphicsType=="HTML" && braw.env$autoShow) {
              showHTML(g)
              return(invisible(g))
            }
            if (braw.env$graphicsType=="ggplot" && braw.env$autoPrint) {
              print(g)
              return(invisible(g))
            }
            return(g)  
          },
          "2D"={

            # show the back wall
            switch (showType,
                    "Populations"={
                      rw<-rpw
                      rw_dens<-rpw_dens
                      switch(braw.env$RZ,
                             "r"={xlabel<-"r[p]"},
                             "z"={xlabel<-"z[p]"}
                      )
                      col<-colP
                    },
                    "Samples"={
                      rw<-rsw
                      rw_dens<-rsw_dens
                      switch(braw.env$RZ,
                             "r"={xlabel<-"r[s]"},
                             "z"={xlabel<-"z[s]"}
                             )
                      col<-colS
                    }
            )
            
            if (showType=="Samples") xlim<-ylim
            
            g<-startPlot(xlim=xlim,ylim=zlim,
                         xlabel=makeLabel(xlabel),ylabel=makeLabel(label.z),
                         xticks=makeTicks(),yticks=NULL,
                         gaps=c(0.5,1,1,1)*4
            )

            theoryAlpha=0.8
            # simulations
            switch (showType,
                    "Populations"={
                      if (!is.null(pSimDens_slice)) {
                        pSimDens_slice<-pSimDens_slice/max(pSimDens_slice)
                        x<-as.vector(matrix(c(pSimBins,pSimBins),2,byrow=TRUE))
                        y1<-c(0,as.vector(matrix(c(pSimDens_slice,pSimDens_slice),2,byrow=TRUE)),0)
                        
                        polygon(x=x,y=y1,col=colP)
                        theoryAlpha=0.25
                      }
                    },
                    "Samples"={
                      if (!is.null(sSimDens)) {
                        dens<-colMeans(sSimDens)
                        dens<-dens/max(dens)
                        x<-as.vector(matrix(c(sSimBins,sSimBins),2,byrow=TRUE))
                        y1<-as.vector(matrix(c(dens,dens),2,byrow=TRUE))
                        y1<-c(0,y1,0)
                        polygon(x=x,y=y1,col=colS)
                        theoryAlpha=0.25
                      }
                    }
            )
            
            if (possible$showTheory){
              switch (showType,
                      "Samples"={
                        if (!all(is.na(sourceSampDens_r_total))){
                          # main distributions
                          # total
                          rsd<-sampleBackwall$rsw_dens_plus+sampleBackwall$rsw_dens_null
                          
                          rp<-populationBackwall$rpw
                          rpd<-populationBackwall$rpw_dens
                          rpd<-rpd/(sum(rpd)/sum(rsd))
                          zgain<-25/sum(rsd)

                          g<-addG(g,dataPolygon(data.frame(x=rp[c(1,1:length(rp),length(rp))],
                                                           y=c(0,rpd,0)*zgain),fill="white",colour=NA))
                          g<-addG(g,dataPolygon(data.frame(x=rs[c(1,1:length(rs),length(rs))],
                                                           y=c(0,rsd,0)*zgain),fill=colSsum,alpha=0.8))
                          g<-addG(g,dataPath(data.frame(x=rs,y=rsd*zgain),colour="#000000",linewidth=0.35))
                          if (world$On) {
                            if (world$pRplus<1) {
                              g<-addG(g,dataPath(data.frame(x=rs,y=sampleBackwall$rsw_dens_null*zgain),colour=colNullS,linewidth=0.5))
                              g<-addG(g,dataPath(data.frame(x=rs,y=sampleBackwall$rsw_dens_plus*zgain),colour=colDistS,linewidth=0.5))
                            }
                          }
                          
                          if (!all(is.na(sRho))) {
                            for (si in 1:length(sRho)) {
                                  z<-approx(sampleBackwall$rsw,rsd,sRho[si])$y
                                  zt<-approx(rs,sourceSampDens_r_total,sRho[si])$y
                                  z[z<0]<-0
                                  if (logZ) z<-log10(z)
                                  if (z>=zlim[1])
                                    g<-addG(g,dataPath(data.frame(x=c(0,0)+sRho[si],y=c(0,z)*zgain),colour=colVline,linewidth=0.75))
                                  g<-addG(g,dataText(data.frame(x=sRho[si],y=z*zgain),paste0("pd(",braw.env$RZ,"[s])=",brawFormat(zt)),size=0.85))
                                }
                          }
                        }
                        return(g)
                      },
                      "Populations"={
                        if (!all(is.na(sampleLikelihoodTotal_r))){
                          # main distribution
                          polygon (x = c(rp[1],rp,rp[length(rp)]), y = c(0,sampleLikelihood_r_show,0), col = addTransparency(colP,theoryAlpha), lwd=1)
                          # vertical lines
                          dens_at_peak<-max(sampleLikelihoodTotal_r)
                          dens_at_zero<-approx(rp,sampleLikelihoodTotal_r,0)$y
                          dens_at_sample<-approx(rp,sampleLikelihoodTotal_r,sRho[1])$y
                          dens_at_ci<-approx(rp,sampleLikelihoodTotal_r,rp_ci)$y
                          # lines(x=c(sRho[1],sRho[1]),y=c(0,dens_at_sample-0.01),col="#000000",lwd=1.5)
                          if (world$PDF!="Uniform_r" && !is.null(rp_peak)){
                            lines(x=c(0,0),y=c(0,dens_at_zero-0.01),col="#000000",lwd=1.5)
                            lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="#000000",lwd=1.5)
                            # lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="#000000",lwd=2.5)
                            # lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="white",lwd=2)
                            # lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="red",lty=3,lwd=2)
                            
                            lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="#000000",lwd=2.5)
                            lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="white",lwd=2)
                            lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="red",lty=3,lwd=2)
                            lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="#000000",lwd=2.5)
                            lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="white",lwd=2)
                            lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="red",lty=3,lwd=2)
                          }
                          text(rp_peak,1.05,labels=paste0(braw.env$RZ,"[mle]=",format(rp_peak,digits=3)),
                                  col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                          text(x=rp_peak,1.15,labels=paste0("llr(",braw.env$RZ,"[mle]/",braw.env$RZ,"[0])=",format(log(1/dens_at_zero),digits=3)),
                               col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                          
                          if (possible$prior$On && possible$prior$pRplus<1) {
                            ln_at_sample<-approx(rs,priorSampDens_r_null,sRho[1])$y
                            ld_at_sample<-approx(rs,colMeans(priorSampDens_r_plus),sRho[1])$y
                            llrNull<-log(ln_at_sample/ld_at_sample)
                            text(xlim[1],1.15,labels=paste0("llr(",braw.env$RZ,"[+]/",braw.env$RZ,"[0])=",format(-llrNull,digits=3)),
                                  col=colPdark,adj=c(0),cex=0.9)
                          }
                        }
                      }
              )
              
            }
          },
          "flat"={
            if (!is.null(possible$targetSample) && !is.null(possible$targetPopulation)) {
              lines(trans3d(x=xlim,y=c(0,0)+possible$targetSample,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1)
              lines(trans3d(x=c(0,0)+possible$targetPopulation,y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=2)
              points(trans3d(x=possible$targetPopulation,
                             y=possible$targetSample,
                             z=c(zlim[1],zlim[1],zlim[1]),pmat=mapping),pch=16,cex=1.5,col=colS)
            }
          }
  )
}
