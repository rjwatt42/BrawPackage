
densityFunctionStats<-function(dens_r,rp){
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
  
  peak<-rp[which.max(dens_r)]
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
  
  pRho<-possibleResult$pRho
  pRhogain<-possibleResult$pRhogain
  
  sr_effectR<-possibleResult$Sims$sSims
  sSimDens<-c()
  if (!isempty(sr_effectR)) {
    if (RZ=="z") {
      use_effects<-atanh(sr_effectR)
      hist_range<-z_range
    } else {
      use_effects<-sr_effectR
      hist_range<-r_range
    }
    binWidth<-2*IQR(use_effects)/length(use_effects)^(1/3)
    nbins=round(2/binWidth)
    sSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
    for (i in 1:nrow(use_effects)) {
      use_data<-abs(use_effects[i,])<=hist_range
      h<-hist(use_effects[i,use_data],sSimBins,plot=FALSE)$counts
      sSimDens<-rbind(sSimDens,h*pRhogain[i]/(1-tanh(pRho[i])^2))
    }
    
    rsSim_ci=quantile(use_effects,c(0.025,0.975))
    rsSim_peak=sSimBins[which.max(sSimDens)]+sSimBins[2]-sSimBins[1]
    rsSim_sd<-sd(use_effects,na.rm=TRUE)
    
    result<-list(sr_effectR=sr_effectR,
                 sSimBins=sSimBins,sSimDens=sSimDens,
                 rsSim_peak=rsSim_peak,rsSim_sd=rsSim_sd,rsSim_ci=rsSim_ci)
  } else {
    result<-NULL
  }
  
}


describePossiblePopulations<-function(possibleResult) {
  
  sRho<-possibleResult$sRho
  
  pr_effectR<-possibleResult$Sims$pSims
  pr_effectRP<-possibleResult$Sims$pSimsP
  pr_effectN<-possibleResult$Sims$pSimsN
  
  if (!isempty(pr_effectRP)) {
    pr_effectW<-rn2w(pr_effectRP,pr_effectN)
    
    # do this in z - for symmetry
    keep<-abs(atanh(pr_effectR)-sRho[1])<possible$simSlice
    pr_effectRP_slice<-pr_effectRP[keep]
    pr_effectW_slice<-pr_effectW[keep]
    
    if (RZ=="z") {
      use_effectRP_slice<-atanh(pr_effectRP_slice)
      use_effectR<-atanh(pr_effectR)
      use_effectRP<-atanh(pr_effectRP)
      hist_range<-z_range
    } else {
      use_effectRP_slice<-pr_effectRP_slice
      use_effectR<-pr_effectR
      use_effectRP<-pr_effectRP
      hist_range<-1
    }
    
    if (possible$prior$populationPDF=="Single" || possible$prior$populationPDF=="Double") {
      binWidth<-0.05
    } else {
      binWidth<-max(0.05,2*IQR(use_effectRP_slice,na.rm=TRUE)/length(use_effectRP_slice)^(1/3))
    }
    nbins=max(10,round(2/binWidth))
    pSimBins<-seq(-1,1,length.out=nbins+1)*hist_range
    pSimBinsW<-seq(w_range[1],w_range[2],length.out=nbins+1)
    
    keep<-abs(use_effectRP_slice)<hist_range
    pSimDens_slice<-hist(use_effectRP_slice[keep],pSimBins,plot=FALSE)$counts
    
    keep<-abs(use_effectRP)<hist_range
    pSimDensRP<-hist(use_effectRP[keep],pSimBins,plot=FALSE)$counts
    
    keep<-abs(use_effectR)<hist_range
    pSimDensR<-hist(use_effectR[keep],pSimBins,plot=FALSE)$counts
    
    keep<-pr_effectW_slice>=w_range[1] & pr_effectW_slice<=w_range[2]
    pSimDensW<-hist(pr_effectW_slice[keep],pSimBinsW,plot=FALSE)$counts
    
    rpSim_ci=quantile(use_effectRP_slice,c(0.025,0.975))
    rpSim_peak=pSimBins[which.max(pSimDens_slice)]+pSimBins[2]-pSimBins[1]
    rpSim_sd<-sd(use_effectRP_slice,na.rm=TRUE)
    rpSimWaste<-sum(!keep)
    wpSim_peak<-pSimBinsW[which.max(pSimDensW)]+pSimBinsW[2]-pSimBinsW[1]
    wpSim_mean<-mean(pr_effectW_slice,na.rm=TRUE)
    wpSimWaste<-sum(!keep)
    
    result<-list(pr_effectR=pr_effectR,pr_effectRP=pr_effectRP,pr_effectN=pr_effectN,pr_effectW=pr_effectW,
                 pSimBins=pSimBins,
                 pSimDens_slice=pSimDens_slice,pSimDensRP=pSimDensRP,pSimDensR=pSimDensR,pSimDensW=pSimDensW,
                 rpSim_ci=rpSim_ci,rpSim_peak=rpSim_peak,rpSim_sd=rpSim_sd,rpSimWaste=rpSimWaste,
                 wpSim_peak=wpSim_peak,wpSim_mean=wpSim_mean,wpSimWaste=wpSimWaste
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
#'                        cutaway=FALSE,walls=TRUE,showP=0,
#'                        view="3D",axisScale=1,
#'                        azimuth=30,elevation=5,distance=2)
#' @export
showPossible <- function(possibleResult=braw.res$possibleResult,
                         cutaway=FALSE,walls=TRUE,showP=0,
                         view="3D",axisScale=1,
                         azimuth=30,elevation=5,distance=2){
  
  if (is.null(possibleResult)) possibleResult<-doPossible()
  if (is.null(possibleResult$possible)) possibleResult<-doPossible(possible=possibleResult)
  
  BoxCol<-"#666666"
  
  colS<-"yellow"
  if (showP>0)  colS<-braw.env$plotColours$infer_nsigC
  colSdark=darken(colS,off=-0.67)
  colSlight=darken(colS,off=0.2)
  colSsim=darken(colS,off=0.0)
  
  colP=braw.env$plotColours$descriptionC
  colPdark=darken(colP,off=-0.67)
  colPlight=darken(colP,off=0.25)
  colPsim=darken(colP,off=-0.33)
  
  colPop="#000000"
  colVline="#000000"
  
  colNullS=darken(braw.env$plotColours$infer_nsigC,off=-0.25)
  colDistS=braw.env$plotColours$descriptionC
  highTransparency=0.25
  
  char3D=braw.env$labelSize/3
  xylabelSize=1.1
  zlabelSize=0.7
  tickLabelSize<-0.6
  wallHeight<-1.2
  logZ<-FALSE
  magRange<-0.5
  
  boxed<-FALSE
  doConnecting<-TRUE
  doSampleLine<-FALSE
  doPeakLine<-TRUE
  doFloorLines<-FALSE 
  doFloorCILines<-TRUE 
  doCILines<-FALSE
  doTextResult<-TRUE
  showJointLk<-FALSE
  showNull<-FALSE
  normSampDist<-FALSE
  endFace<-TRUE
  
  possible<-possibleResult$possible
  world<-possible$hypothesis$effect$world
  design<-possible$design
  
  switch (possible$UseSource,
          "hypothesis"={possible$source<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=effect$rIV,populationRZ="r",populationNullp=0)},
          "world"={possible$source<-world},
          "prior"={possible$source<-possible$prior},
          "null"={possible$source<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=0,populationRZ="r",populationNullp=0)}
  )
  # make the distribution        
  
  switch (possible$type,
          "Samples"={
            label.z<-"Probability Density"
            col=colP
          },
          "Populations"={
            label.z<-"Likelihood"
            col=colS
          }
  )
  
  sourceRVals<-possibleResult$sourceRVals
  sRho<-possibleResult$sRho
  
  rs<-possibleResult$Theory$rs
  sourceSampDens_r<-possibleResult$Theory$sourceSampDens_r
  sourceSampDens_r_null<-possibleResult$Theory$sourceSampDens_r_null
  sourceSampDens_r_plus<-possibleResult$Theory$sourceSampDens_r_plus
  sourceSampDens_r_total<-sourceSampDens_r
  
  rp<-possibleResult$Theory$rp
  priorSampDens_r<-possibleResult$Theory$priorSampDens_r
  sampleLikelihood_r<-possibleResult$Theory$sampleLikelihood_r
  sampleLikelihood_r_show<-possibleResult$Theory$sampleLikelihood_r_show
  priorSampDens_r_null<-possibleResult$Theory$priorSampDens_r_null
  priorSampDens_r_plus<-possibleResult$Theory$priorSampDens_r_plus
  
  # make the histograms
  switch (possible$type,
          "Samples"={
            srAnalysis<-describePossibleSamples(possibleResult)
            sSimBins<-srAnalysis$sSimBins
            sSimDens<-srAnalysis$sSimDens
          },
          "Populations"={
            prAnalysis<-describePossiblePopulations(possibleResult)
            pSimBins<-prAnalysis$pSimBins
            pSimDens_slice<-prAnalysis$pSimDens_slice
            pSimDensRP<-prAnalysis$pSimDensRP
            pSimDensR<-prAnalysis$pSimDensR
            
            pSimBinsW<-prAnalysis$pSimBinsW
            pSimDensW<-prAnalysis$pSimDensW
          }
  )
  
  # make the back wall population distributions
  rpw<-rp
  if (possible$type=="Samples") {
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
  if (is.element(world$populationPDF,c("Single","Double"))) rsw_dens_plus<-rsw_dens_plus-rsw_dens_null
  rsw_dens<-rsw_dens_plus+rsw_dens_null
  
  offRange<-0
  if (axisScale>1 && !is.null(possible$targetSample)) offRange<-possible$targetSample  
  if (braw.env$RZ=="r") {
    xlim<-c(-1,1)/axisScale+offRange # population
    ylim<-c(-1,1)/axisScale+offRange
  } else {
    xlim<-c(min(rp),max(rp))/axisScale+offRange # population
    ylim<-c(min(rs),max(rs))/axisScale+offRange
  }
  zlim<-c(0,1)*wallHeight
  
  if (length(sourceRVals)<8) draw_lower_limit=0.000001
  else                       draw_lower_limit=0.01
  if (logZ) {
    if (possible$type=="Samples") {
      zlim<-c(-2,0)
    } else {
      zlim<-c(-5,0)
    }
    draw_lower_limit<-zlim[1]
  }
  zlim[2]<-zlim[2]+diff(zlim)*0.2
  
  if (!is.null(sampleLikelihood_r)) {
  rp_stats<-densityFunctionStats(sampleLikelihood_r,rp)
  rp_peak<-rp_stats$peak
  rp_ci<-rp_stats$ci
  dens_at_peak<-rp_stats$dens_at_peak
  }
  
  rpw_dens[rpw_dens>1 | is.na(rpw_dens)]<-1
  # if (!(possible$type=="Populations" && possible$UsePrior=="none")) {
    rsw_dens_plus<-rsw_dens_plus/max(rsw_dens,na.rm=TRUE)
    rsw_dens_null<-rsw_dens_null/max(rsw_dens,na.rm=TRUE)
  # }
  populationBackwall<-list(rpw=rpw,rpw_dens=rpw_dens,priorSampDens_r=priorSampDens_r,rp=rp)
  sampleBackwall<-list(rsw=rsw,rsw_dens_plus=rsw_dens_plus,rsw_dens_null=rsw_dens_null)
  
  if (view=="flat") {
    azimuth=90
    elevation=90
    label.z<-c()
  }
  if (is.element(view,c("3D","flat"))) {
    
    # make the floor
    f <- function(x, y) { x*0+y*0 }
    z <- outer(xlim, xlim, f)+zlim[1]
    z[is.na(z)] <- zlim[1]
    par(bg=braw.env$plotColours$graphC,mar=c(0,1,0,0),font.lab=2)
    mapping<-persp(xlim,ylim,z, 
                   xlim=xlim+c(-1,1)*diff(xlim)*0.1,
                   ylim=ylim+c(-1,1)*diff(ylim)*0.1,
                   zlim = zlim, 
                   theta = azimuth, phi = elevation, r=distance, 
                   ticktype = "simple", 
                   box = FALSE,
                   axes = FALSE,
                   expand = 0.5, col = braw.env$plotColours$graphBack,
                   cex.axis=0.6,
                   xlab = "Populations", ylab = "Samples", zlab = label.z
    )
    # outside box            
    if (boxed){
      polygon(trans3d(x=c(xlim[1], xlim[1], xlim[1],xlim[1]),
                      y=c(ylim[1], ylim[1], ylim[2],ylim[2]),
                      z=zlim[c(1, 2, 2,1)],mapping),
              col=braw.env$plotColours$graphBack,border=NA
      )
      polygon(trans3d(x=c(xlim[1], xlim[1], xlim[2],xlim[2]),
                      y=c(ylim[2], ylim[2], ylim[2],ylim[2]),
                      z=zlim[c(1, 2, 2,1)],mapping),
              col=braw.env$plotColours$graphBack,border=NA
      )
      lines(trans3d(x=c(xlim[1], xlim[1], xlim[2]),
                    y=c(ylim[1],ylim[2],ylim[2]),
                    z=c(zlim[2],zlim[2],zlim[2]),pmat=mapping), col=BoxCol)        
      lines(trans3d(x=c(xlim[1],xlim[1]),
                    y=c(ylim[1],ylim[1]),
                    z=zlim,pmat=mapping),col=BoxCol)
      lines(trans3d(x=c(xlim[1],xlim[1]),
                    y=c(ylim[2],ylim[2]),
                    z=zlim,pmat=mapping),col=BoxCol)
      lines(trans3d(x=c(xlim[2],xlim[2]),
                    y=c(ylim[2],ylim[2]),
                    z=zlim,pmat=mapping),col=BoxCol)
    }
    
    tick_grow<-2
    xtick_length<-0.02*diff(xlim)
    ytick_length<-0.02*diff(ylim)
    
    if (!is.null(label.z)) {
      # z-axis
      lines(trans3d(x=c(xlim[1],xlim[1]),
                    y=c(ylim[1],ylim[1]),
                    z=zlim,pmat=mapping),col="black")
      # short ticks
      if (boxed) {
        plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)
      } else {
        plot_ticks<-seq(zlim[1],zlim[2],diff(zlim)/10)
      }
      tick.z.start <- trans3d(xlim[1],ylim[1],plot_ticks, mapping)
      tick.z.end <- trans3d(xlim[1],ylim[1]-ytick_length,plot_ticks, mapping)
      segments(tick.z.start$x, tick.z.start$y, tick.z.end$x, tick.z.end$y)
      # long ticks
      long_ticks<-seq(zlim[1],zlim[2],diff(zlim)/2)
      tick.z.start <- trans3d(xlim[1],ylim[1],long_ticks, mapping)
      tick.z.end <- trans3d(xlim[1],ylim[1]-ytick_length*tick_grow,long_ticks, mapping)
      segments(tick.z.start$x, tick.z.start$y, tick.z.end$x, tick.z.end$y)
      # label
      pos.z<-trans3d(xlim[1],ylim[1]-diff(ylim)*0.08,mean(zlim),mapping)
      rotate.z=trans3d(x=c(xlim[1],xlim[1]),
                       y=c(ylim[1],ylim[1]),
                       z=zlim,pmat=mapping)
      rotate.z<-180+atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296
      text(pos.z$x,pos.z$y,label.z,srt=rotate.z,font=2,cex=char3D*zlabelSize)
    }
    
    # x ticks
    plot_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
    long_ticks<-seq(ceil(xlim[1]*2),floor(xlim[2]*2))/2
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*5),floor(xlim[2]*5))/5
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
    
    # short ticks  
    tick.x.start <- trans3d(plot_ticks, ylim[1], zlim[1], mapping)
    tick.x.end <- trans3d(plot_ticks , ylim[1]-ytick_length, zlim[1], mapping)
    segments(tick.x.start$x, tick.x.start$y, tick.x.end$x, tick.x.end$y)
    # long ticks
    tick.x.start <- trans3d(long_ticks, ylim[1], zlim[1], mapping)
    tick.x.end <- trans3d(long_ticks , ylim[1]-ytick_length*tick_grow, zlim[1], mapping)
    segments(tick.x.start$x, tick.x.start$y, tick.x.end$x, tick.x.end$y)
    # tick labels
    ticks.x<-trans3d(long_ticks+xtick_length,ylim[1]- ytick_length*tick_grow*2.5,zlim[1],mapping)
    text(ticks.x$x,ticks.x$y,long_ticks,cex=char3D*tickLabelSize,adj=c(0.5,0.5))
    
    # y ticks
    plot_ticks<-seq(ceil(ylim[1]*10),floor(ylim[2]*10))/10
    long_ticks<-seq(ceil(xlim[1]*2),floor(xlim[2]*2))/2
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*5),floor(xlim[2]*5))/5
    if (length(long_ticks)==1)
      long_ticks<-seq(ceil(xlim[1]*10),floor(xlim[2]*10))/10
    
    tick.y.start <- trans3d(xlim[2], plot_ticks, zlim[1], mapping)
    tick.y.end <- trans3d(xlim[2]+xtick_length, plot_ticks , zlim[1], mapping)
    segments(tick.y.start$x, tick.y.start$y, tick.y.end$x, tick.y.end$y)
    # long ticks
    tick.y.start <- trans3d(xlim[2], long_ticks, zlim[1], mapping)
    tick.y.end <- trans3d(xlim[2]+xtick_length*tick_grow, long_ticks , zlim[1], mapping)
    segments(tick.y.start$x, tick.y.start$y, tick.y.end$x, tick.y.end$y)
    # tick labels
    ticks.y<-trans3d(xlim[2]+xtick_length*tick_grow*2.5,long_ticks,zlim[1],mapping)
    text(ticks.y$x,ticks.y$y,long_ticks,cex=char3D*tickLabelSize,adj=c(0.5,0.5))
    
    if (braw.env$RZ=="r") {
      label.x<-bquote(bold(r['p']))
      label.y<-bquote(bold(r['s']))
    } else {
      label.x<-bquote(bold(z['p']))
      label.y<-bquote(bold(z['s']))
    }
    
    pos.x<-trans3d(sum(xlim)/2,ylim[1]-ytick_length*tick_grow*5,zlim[1]-0.1,mapping)
    text(pos.x$x,pos.x$y,label.x,adj=c(0.5,0.5),font=2,cex=char3D*xylabelSize)
    
    pos.y<-trans3d(xlim[2]+xtick_length*tick_grow*5,sum(ylim)/2,zlim[1]-0.1,mapping)
    text(pos.y$x,pos.y$y,label.y,adj=c(0.5,0.5),font=2,cex=char3D*xylabelSize)
    
  }
  # graph frame
  switch (view,
          "3D"= {
            
            # lines on the floor
            # general 
            if (doFloorLines) {
              lines(trans3d(x=xlim,y=c(0,0),z=c(0,0)+zlim[1],pmat=mapping),col="black",lty=3)
              lines(trans3d(x=c(0,0),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col="black",lty=3)
            }
            # populations 
            # if (!cutaway && !is.null(possible$targetSample)) {
            #   lines(trans3d(x=xlim,y=c(0,0)+possible$targetSample,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=3)
            # }
            if (possible$type=="Populations" && !is.na(possible$targetPopulation)) {
              lines(trans3d(x=c(0,0)+possible$targetPopulation,y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colPop,lwd=1,lty=3)
            }
            if (possible$type=="Samples" && !any(is.na(sourceRVals)) && length(sourceRVals)<8) {
              for (s in sourceRVals)
              lines(trans3d(x=c(0,0)+s,y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=3)
            }
            
            # populations 
            if (!is.null(possible$targetSample)) {
              # show peak and CIs on floor
              if (doFloorCILines) {
                if (rp_peak==0 && possible$UsePrior!="none") colHere<-colNullS else colHere<-colDistS
                lines(trans3d(x=c(rp_peak,rp_peak),y=c(ylim[1],ylim[2]),z=c(0,0)+zlim[1],pmat=mapping),col=colHere,lwd=1)
                # lines(trans3d(x=c(0,0),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=3)
                if (doCILines) {
                  lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colHere,lty=3,lwd=1)
                  lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colHere,lty=3,lwd=1)
                }
              }
              # show rp==rs on floor
              # if (world$populationPDF!="Single"){
                # lines(trans3d(x=c(sRho[1],sRho[1]),y=ylim,z=c(0,0),pmat=mapping),col=colPdark,lwd=1,lty=3)
              # }
            }
            
            if (walls) {
              si=1;
            # if (possible$UsePrior!="none") {
              za<-approx(rs,sampleBackwall$rsw_dens_null,sRho[si])$y
              zb<-approx(rs,sampleBackwall$rsw_dens_plus,sRho[si])$y
              llrNull<-log(za/zb)
            # }
            
            if (axisScale<=100) {
              
              # population wall
              x<-populationBackwall$rpw
              y<-x*0+ylim[2]
              z<-populationBackwall$rpw_dens
              if (logZ) z<-log10(z)
              use<- which((x>=xlim[1]) & (x<=xlim[2]))
              polygon(trans3d(x=c(x[use[1]],x[use],x[use[length(use)]]),y=c(y[use[1]],y[use],y[use[length(use)]]),z=c(zlim[1],z[use],zlim[1])*wallHeight,pmat=mapping),col=addTransparency(colP,0.25))
              
              if (showJointLk && !any(is.na(populationBackwall$priorSampDens_r))) {
                # show the joint likelihood function
                x<-populationBackwall$rp
                y<-x*0+ylim[2]
                z<-populationBackwall$priorSampDens_r
                if (logZ) z<-log10(z)
                polygon(trans3d(x=c(x[1],x,x[length(x)]),y=c(y[1],y,y[length(y)]),z=c(zlim[1],z,zlim[1])*wallHeight,pmat=mapping),col = addTransparency(colPdark,0.5),border=NA)
              }
              if (possible$type=="Populations" && !is.null(possible$targetSample)) {
                # show peak likelihood on population back wall
                if (rp_peak==0) colHere<-colNullS else colHere<-colDistS
                dens_rp_peak<-approx(populationBackwall$rpw,populationBackwall$rpw_dens,rp_peak)$y
                if (logZ)  dens_rp_peak<-log10(dens_rp_peak)
                lines(trans3d(x=c(0,0)+rp_peak,y=c(0,0)+ylim[2],z=c(zlim[1],dens_rp_peak)*wallHeight,pmat=mapping),col=colHere,lwd=2)
                # show population likelihood on population back wall
                dens_target<-approx(populationBackwall$rpw,populationBackwall$rpw_dens,possible$targetPopulation)$y
                if (logZ)  dens_target<-log10(dens_target)
                lines(trans3d(x=c(0,0)+possible$targetPopulation,y=c(0,0)+ylim[2],z=c(zlim[1],dens_target)*wallHeight,pmat=mapping),col=colPop,lwd=1,lty=3)
              }
              
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
              use<- which((y>=ylim[1]) & (y<=ylim[2]))
              # if (!(possible$type=="Populations" && possible$UsePrior=="none")) {
              polygon(trans3d(x=c(x[use[1]],x[use],x[use[length(use)]]),y=c(y[use[1]],y[use],y[use[length(use)]]),z=c(zlim[1],ztotal[use],zlim[1])*wallHeight,pmat=mapping),col=addTransparency(colS,0.25))
                # split into 2 parts  
                if (possible$source$worldOn && possible$source$populationNullp>0){
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
                  if (possible$source$populationNullp>0 ) {
                    lines(trans3d(x=x[use],y=y[use],z=znull[use]*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                  }
                  lines(trans3d(x=x[use],y=y[use],z=zplus[use]*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                }
              # }
              
              # vertical lines
              if (possible$showTheory) {
                if (possible$type=="Samples") {
                  # show probability density on sample back wall
                  if (!isempty(sRho)){
                    for (si in 1:length(sRho)) {
                      z<-approx(sampleBackwall$rsw,ztotal,sRho[si])$y
                      if (logZ) z<-log10(z)
                      lines(trans3d(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],z)*wallHeight,pmat=mapping),col=colVline,lwd=1)
                    }
                  }
                }
                if (possible$type=="Populations" &&!is.null(possible$targetSample)) {
                  # show likelihood on sample back wall
                  si=1;
                  # if (possible$UsePrior!="none") {
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
                      lines(trans3d(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],za)*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                      lines(trans3d(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                    } else {
                      lines(trans3d(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                      lines(trans3d(x=c(0,0)+xlim[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],za)*wallHeight,pmat=mapping),col=colNullS,lwd=2)
                    }
                    # } else  {
                    # zb<-approx(y,ztotal,sRho[si])$y
                    # lines(trans3d(x=c(0,0)+view_lims[1],y=c(sRho[si],sRho[si]),z=c(zlim[1],zb)*wallHeight,pmat=mapping),col=colDistS,lwd=2)
                  # }
                }
              }
            }
            }
            
            # main distributions            
            theoryAlpha=0.8
            simAlpha<-1
            switch (possible$type,
                    "Samples"={
                      # draw simulations
                      if (length(sourceRVals)>1 && length(sourceRVals)<8) {
                        theoryAlpha<-0.45
                        simAlpha<-0.95
                      }
                      pgain<-(1-possible$source$populationNullp)
                      if (length(sourceRVals)==2) {
                        pgain<-max(c(1-possible$source$populationNullp,possible$source$populationNullp))
                      } 
                      # prepare simulations first
                      if (!is.null(sSimDens)) {
                        theoryAlpha<-0.25
                        
                        simgain<-mean(sourceSampDens_r)/mean(sSimDens)
                        sSimDens<-sSimDens*simgain*pgain
                        if (cutaway) {
                          waste<-sum(sSimBins<=min(sRho))
                          use_s<-(waste):length(sSimBins)
                          simBins<-simBins[use_s]
                          simBins[1]<-min(sRho)
                          use_s<-use_s[1:(length(use_s)-1)]
                        } else {
                          if (!is.matrix(sSimDens)) {
                            sSimDens<-t(sSimDens)
                            sourceSampDens_r<-t(sourceSampDens_r)
                          } 
                          use_s<-(1:ncol(sSimDens))
                        }
                      } 
                      
                      # we interleave simulations and theory (because no hidden line removal)
                      cutZ<-c()
                      if (length(sourceRVals)>10) useVals<-seq(1,length(sourceRVals),5)
                        else useVals<-order(sourceRVals)
                      for (i in order(sourceRVals)) {
                        # draw simulations
                        if (!is.null(sSimDens)){
                          y1<-as.vector(matrix(c(sSimBins,sSimBins),2,byrow=TRUE))
                          z1<-as.vector(matrix(c(sSimDens[i,use_s],sSimDens[i,use_s]),2,byrow=TRUE))
                          if (logZ) z1<-log10(z1)
                          z1<-c(zlim[1],z1,zlim[1])
                          z1[z1<zlim[1]]<-zlim[1]
                          use<-which(y1>=ylim[1] & y1<=ylim[2])
                          polygon(trans3d(x=rep(sourceRVals[i],length(use)),y=y1[use],z=z1[use],pmat=mapping),col=addTransparency(colSsim,simAlpha))
                        }
                        
                        # draw theory
                        if (possible$showTheory){
                          if (any(i==useVals)) {
                            z_use<-sourceSampDens_r_plus[i,]*pgain
                            if (cutaway) {
                              use<-max(which(rs<min(sRho)))
                              z_use[use]<-approx(rs,z_use,min(sRho))$y
                              z_use[1:use-1]<-0
                            } 

                          r_use<-rs
                          while (length(z_use)>0 && any(z_use>0)) {
                            use1<-which(z_use>0)[1]
                            z_use<-z_use[use1:length(z_use)]
                            r_use<-r_use[use1:length(r_use)]
                            use2<-which(c(z_use,0)==0)[1]-1
                            rs_draw<-r_use[1:use2]
                            z_draw<-z_use[1:use2]
                            if (logZ) z_draw<-log10(z_draw)
                            z_draw[z_draw<draw_lower_limit]<-NA
                            use<-rs_draw>=ylim[1] & rs_draw<=ylim[2]
                            rs_use<-rs_draw[use]
                            if (sourceRVals[i]>=xlim[1] & sourceRVals[i]<=xlim[2]) {
                              polygon (trans3d(x = rep(sourceRVals[i],length(rs_use)+2), y = c(rs_use[1],rs_use,rs_use[length(rs_use)]), z = c(zlim[1],z_draw[use],zlim[1]), pmat = mapping), col = addTransparency(colS,theoryAlpha), lty=0, lwd=1)
                              lines (trans3d(x = rep(sourceRVals[i],length(rs_use)+2), y = c(rs_use[1],rs_use,rs_use[length(rs_use)]), z = c(zlim[1],z_draw[use],zlim[1]), pmat = mapping), col = addTransparency("#000000",theoryAlpha), lwd=1)
                            }
                            if (use2==length(z_use)) break
                            z_use<-z_use[(use2+1):length(z_use)]
                            r_use<-r_use[(use2+1):length(r_use)]
                          }
                          if (showP>0 && !is.null(sRho)) {
                            rcrit<-sRho
                            if (sRho>0)  use<-which(rs>=rcrit)
                            else         use<-which(rs<=rcrit)
                            polygon(trans3d(x = rep(sourceRVals[i],length(use)+2), 
                                            y = rs[use[c(1,1:length(use),length(use))]], 
                                            z = c(0,z_use[use],0), pmat = mapping), col = braw.env$plotColours$infer_sigC, lwd=1)
                            if (showP>1) {
                              if (sRho>0)  use<-which(rs<= -rcrit)
                              else         use<-which(rs>= -rcrit)
                            polygon(trans3d(x = rep(sourceRVals[i],length(use)+2), 
                                            y = rs[use[c(1,1:length(use),length(use))]], 
                                            z = c(0,z_use[use],0), pmat = mapping), col = braw.env$plotColours$infer_sigC, lwd=1)
                            }
                          }
                          }
                        }
                        # vertical lines on main distribution
                        if (!isempty(sRho)){
                          for (si in 1:length(sRho)) {
                            z<-approx(rs,sourceSampDens_r_plus[i,],sRho[si])$y
                            z<-z*pgain
                            if (logZ) {
                              z<-log10(z)
                              z[z<zlim[1]]<-zlim[1]
                            }
                            cutZ<-c(cutZ,z)
                          }
                          for (si in 1:length(sRho)) {
                            if (any(i==useVals) && (showP==0) && (cutaway || length(sourceRVals)<8) && z>=draw_lower_limit)  {
                            # lines(trans3d(x=c(sourceRVals[i],sourceRVals[i]),y=c(sRho[si],sRho[si]),z=c(zlim[1],z),pmat=mapping),col="black", lwd=1)
                            }
                            # connecting lines
                            if (!cutaway && doConnecting && length(sourceRVals)>5 && i<length(sourceRVals)) {
                              z1<-approx(rs,sourceSampDens_r_plus[i+1,],sRho[si])$y
                              z1<-z1*pgain
                              if (logZ) {
                                z1<-log10(z1)
                                z1[z1<zlim[1]]<-zlim[1]
                              }
                              polygon(trans3d(x=c(sourceRVals[i],sourceRVals[i+1],sourceRVals[i+1],sourceRVals[i]),
                                              y=c(sRho[si],sRho[si],sRho[si],sRho[si]),
                                              z=c(z,z1,zlim[1],zlim[1]),pmat=mapping),
                                      col=colP,lty=0)
                              lines(trans3d(x=c(sourceRVals[i],sourceRVals[i+1]),y=c(sRho[si],sRho[si]),z=c(z,z1),pmat=mapping),col=colVline, lwd=1)
                            }
                          }
                          }
                      }
                      if (cutaway && endFace) {
                        # main distribution
                        cutZ[cutZ<draw_lower_limit]<-zlim[1]
                        polygon (trans3d(x = c(sourceRVals[1],sourceRVals,sourceRVals[length(sourceRVals)]),
                                         y = c(0,sourceRVals*0,0)+sRho[1], 
                                         z = c(zlim[1],cutZ,zlim[1]), pmat = mapping), col = addTransparency(colP,0.8), lty=0, lwd=1)
                        use<-which(cutZ>zlim[1])
                        lines (trans3d(x = c(sourceRVals[use[1]],sourceRVals[use],sourceRVals[use[length(use)]]),
                                         y = c(0,use*0,0)+sRho[1], 
                                         z = c(zlim[1],cutZ[use],zlim[1]), pmat = mapping), col = "black", lwd=1)
                      }
                    },
                    "Populations"={
                      # draw simulations
                      if (!is.null(prAnalysis)) {
                        x<-as.vector(matrix(c(pSimBins,pSimBins),2,byrow=TRUE))
                        
                        # population wall
                        gainSim<-sum(pSimDensRP)*diff(pSimBins[1:2])
                        gainTheory<-sum(rpw_dens)*diff(rpw[1:2])
                        densP<-pSimDensRP/(gainSim/gainTheory)
                        if (max(densP)>1.2) {densP<-densP/max(densP)*1.2}
                        yP<-c(0,as.vector(matrix(c(densP,densP),2,byrow=TRUE)),0)
                        if (logZ) yP<-log10(yP)
                        polygon(trans3d(x=x,y=x*0+ylim[2],z=yP*wallHeight,pmat=mapping),col = addTransparency(colPdark,0.35),border=NA)
                        
                        # sample wall
                        gainSim<-sum(pSimDensR)*diff(pSimBins[1:2])
                        gainTheory<-sum(rsw_dens)*(rsw[2]-rsw[1])
                        densS<-pSimDensR/(gainSim/gainTheory)
                        if (max(densS)>1.2) {densS<-densS/max(densS)*1.2}
                        yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                        if (logZ) yS<-log10(yS)
                        polygon(trans3d(x=x*0+xlim[1],y=x,z=yS*wallHeight,pmat=mapping),col = addTransparency(colSdark,0.25),border=NA)
                        
                        #slice of interest
                        si=1
                        gainSim<-sum(pSimDens_slice)*diff(pSimBins[1:2])
                        gainTheory<-sum(sampleLikelihood_r_show)*diff(rpw[1:2])
                        densRS<-pSimDens_slice/(gainSim/gainTheory)
                        # dens<-dens/max(dens,na.rm=TRUE)
                        # if (max(dens)>1.2) {dens<-dens/max(dens)*1.2}
                        yRS<-c(0,as.vector(matrix(c(densRS,densRS),2,byrow=TRUE)),0)
                        if (logZ) yRS<-log10(yRS)
                        polygon(trans3d(x=x,y=x*0+sRho[si],z=yRS,pmat=mapping),col=colPsim,border=NA)
                      }
                      
                      # draw theory main distribution & lines
                      if (possible$showTheory){
                        theoryAlpha=0.8
                        if (!is.null(possible$targetSample)) {
                          rd<-sampleLikelihood_r_show
                          rd<-rd/max(rd)*0.8
                          if (logZ) {
                            rd<-log10(rd)
                            rd[rd<zlim[1]]<-zlim[1]
                          }
                          if (!is.null(sampleLikelihood_r_show)){
                            # main distribution
                            for (si in order(-sRho)) {
                              use<-rp>=xlim[1] & rp<=xlim[2]
                              rp_use<-rp[use]
                              dens_use<-rd[si,use]
                              if (is.null(prAnalysis)) {
                                polygon (trans3d(x = c(rp_use[1],rp_use,rp_use[length(rp_use)]),
                                                 y = c(0,rp_use*0,0)+sRho[si], 
                                                 z = c(zlim[1],dens_use,zlim[1]), pmat = mapping), col = addTransparency(colP,theoryAlpha), lwd=1)
                              } else {
                                polygon (trans3d(x = c(rp_use[1],rp_use,rp_use[length(rp_use)]), 
                                                 y = c(0,rp_use*0,0)+sRho[si], 
                                                 z = c(zlim[1],dens_use,zlim[1]), pmat = mapping), col = addTransparency(colP,highTransparency), lwd=1)
                              }
                            }
                          }
                          
                          # vertical lines on main distribution
                          if (doPeakLine && length(sRho)==1) {
                            if (showNull) {
                              lines(trans3d(x=c(0,0),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],0)$y-0.01),pmat=mapping),col=colVline, lwd=1, lty=3)
                            }
                            si<-1
                            if (rp_peak==0 && possible$UsePrior!="none") colHere<-colNullS else colHere<-colDistS
                            lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_peak)$y-0.01),pmat=mapping),col=colHere, lwd=2)
                            if (doCILines) {
                              lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_ci[1])$y-0.01),pmat=mapping),col=colP, lwd=2,lty=3)
                              lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],rp_ci[2])$y-0.01),pmat=mapping),col=colP, lwd=2,lty=3)
                            }
                            lines(trans3d(x=c(possible$targetPopulation,possible$targetPopulation),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],possible$targetPopulation)$y-0.01),pmat=mapping),col=colPop, lwd=1, lty=3)
                            if (doSampleLine && world$populationPDF!="Single"){
                              lines(trans3d(x=c(sRho[si],sRho[si]),y=c(sRho[si],sRho[si]),z=c(zlim[1],approx(rp,rd[si,],sRho[si])$y-0.01),pmat=mapping),col=colP,lty=3,lwd=2)
                            }
                          }
                          # text annotations
                          if (doTextResult && walls) {
                            # llr 
                            if (possible$UsePrior!="none") {
                              text(trans3d(x=xlim[2],y=ylim[2],z=zlim[2]*1.1,pmat=mapping),
                                   labels=bquote(
                                     llr(bold(.(braw.env$RZ)["+"]/.(braw.env$RZ)[0]==.(format(-llrNull,digits=3))))
                                         ),
                                   col=colSdark,adj=c(1,0),cex=0.9)
                            }
                            # mle population
                            llr_0_mle<-log(approx(rp,sampleLikelihood_r,rp_peak)$y/approx(rp,sampleLikelihood_r,0)$y)
                            text(trans3d(x=xlim[2],y=ylim[2],z=zlim[2]*1.0,pmat=mapping),labels=bquote(
                              llr(bold(.(braw.env$RZ)[mle]/.(braw.env$RZ)[0]==.(format(llr_0_mle,digits=3))))
                            ),col=colPdark,adj=c(1,0),cex=0.9)
                          }
                        }
                      }
                    }
            )
            if (doTextResult && walls) {
              # mle population
              text(trans3d(x=xlim[2],y=ylim[2],z=zlim[2]*0.9,pmat=mapping),
                        labels=bquote(bold(.(braw.env$RZ)[mle]== .(format(rp_peak,digits=3)))),
                   col=colPdark,adj=1,cex=0.9)
              text(trans3d(x=xlim[2],y=ylim[2],z=zlim[2]*0.8,pmat=mapping),
                   labels=bquote(bold(.(braw.env$RZ)['s    ']== .(format(sRho[1],digits=3)))),
                   col=colPdark,adj=1,cex=0.9)
            }
            
            
            # finish off plot box
            if (boxed){
              lines(trans3d(x=c(xlim[1], xlim[2], xlim[2]),
                            y=c(ylim[1], ylim[1], ylim[2]),
                            z=c(1,1,1)*zlim[2],pmat=mapping), col=BoxCol, lty=3)        
              lines(trans3d(x=c(xlim[2],xlim[2]),y=c(ylim[1],ylim[1]),z=zlim,pmat=mapping),col=BoxCol,lty=3)
            }
          },
          "2D"={
            par(bg=braw.env$plotColours$graphC,pin=c(1.33,1)*3,mar=c(5,5,1,1))
            
            # show the back wall
            switch (possible$type,
                    "Populations"={
                      rw<-rpw
                      rw_dens<-rpw_dens
                      xlabel<-bquote(bold(r['p']))
                      if (braw.env$RZ=="z") xlabel<-bquote(bold(z['p']))
                      col<-colP
                    },
                    "Samples"={
                      rw<-rsw
                      rw_dens<-rsw_dens
                      xlabel<-bquote(bold(r['s']))
                      if (braw.env$RZ=="z") xlabel<-bquote(bold(z['s']))
                      col<-colS
                    }
            )
            
            rwd<-c(rw[1],rw,rw[length(rw)])
            rwd_dens<-c(0,rw_dens,0)
            if (possible$type=="Samples") xlim<-ylim
            plot(x=rwd,y=rwd_dens,xlab=xlabel,ylab=label.z,type="n",yaxt="n",font.lab=2, cex.lab=char3D,
                 xlim=xlim,ylim=zlim)
            axis(side = 2,  at=0, labels = FALSE, tck=-0.05)
            
            # gray background
            u <- par("usr") # The coordinates of the plot area
            rect(u[1], u[3], u[2], u[4], col=braw.env$plotColours$graphBack, border=NA)
            lines(xlim,c(0,0),col="black")
            
            if (possible$type=="Populations") {
              # make the back wall
              polygon(x=rwd,y=rwd_dens,col=addTransparency(col,0.2))
              lines(x=rw,y=rw_dens,col=colDistS,lwd=2)
              if (possible$source$populationNullp>0) {
                lines(x=c(-1,-1,1,1)*0.02,y=c(0,1,1,0),col=addTransparency(colNullS,0.2),lwd=2)
              }
              
              # zpSample<-approx(rw,rw_dens,sRho[1])$y
              # lines(x=c(0,0)+sRho[1],y=c(0,zpSample),col="red", lwd=3)
            }
            
            theoryAlpha=0.8
            # simulations
            switch (possible$type,
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
              switch (possible$type,
                      "Samples"={
                        if (!all(is.na(sourceSampDens_r_total))){
                          # main distributions
                          # total
                          polygon (x = rs[c(1,1:length(rs),length(rs))], y = c(0,sampleBackwall$rsw_dens_plus+sampleBackwall$rsw_dens_null,0), col = addTransparency(colS,theoryAlpha), lwd=1)
                          if (world$worldOn) {
                            if (world$populationNullp>0)
                              # null
                              lines (x = rs, y = sampleBackwall$rsw_dens_null, col = colNullS, lwd=2)
                            # plus
                            lines (x = rs, y = sampleBackwall$rsw_dens_plus, col = colDistS, lwd=2)
                          }
                          
                          if (!all(is.na(sRho))) {
                            for (i in 1:length(sRho)) {
                              s<-sRho[i]
                              gain<-sum(sourceSampDens_r_total)*diff(rs[1:2])
                              p_at_sample<-(sum(sourceSampDens_r_total[rs>=s])+sum(sourceSampDens_r_total[rs< -s]))/sum(sourceSampDens_r_total)
                              pn_at_sample<-(sum(sourceSampDens_r_null[rs>=s])+sum(sourceSampDens_r_null[rs< -s]))/sum(sourceSampDens_r_total)
                              pd_at_sample<-(sum(sourceSampDens_r_plus[rs>=s])+sum(sourceSampDens_r_plus[rs< -s]))/sum(sourceSampDens_r_total)
                              
                              l_at_sample<-approx(rs,sourceSampDens_r_total,s)$y#/mean(sourceSampDens_r_total)
                              ln_at_sample<-approx(rs,sourceSampDens_r_null,s)$y#/mean(sourceSampDens_r_total)
                              ld_at_sample<-approx(rs,colSums(sourceSampDens_r_plus),s)$y#/mean(sourceSampDens_r_total)
                              
                              lines(x=c(sRho[i],sRho[i]),y=c(0,l_at_sample),col=colVline,lwd=1)
                              points(x=sRho[i],y=l_at_sample,col=colVline,pch=16,cex=1.5)
                              if (world$worldOn) {
                                if (length(sRho)<=10) {
                                  ptext<-bquote(
                                    bolditalic(p)[.(braw.env$RZ)]== bold(.(format(p_at_sample,digits=3))) ~" "~ atop(phantom(bold(.(format(pd_at_sample,digits=3)))),phantom(bold(.(format(pn_at_sample,digits=3)))))
                                  )
                                  ltext<-bquote(
                                    bold(pd(.(braw.env$RZ)[s]))==.(format(l_at_sample/gain,digits=3)) ~" "~ atop(phantom(.(format(ld_at_sample,digits=3))),phantom(.(format(ln_at_sample,digits=3))))
                                  )
                                  # ltext<-format(log(l_at_sample),digits=3)
                                  if (s>0)   {
                                    # text(s,0.95,labels=ptext,col=colPdark,adj=0,cex=0.9)
                                    text(s+0.05,l_at_sample+0.05,labels=ltext,col="black",adj=0,cex=0.9)
                                  } else  {
                                    # text(s,0.95,labels=ptext,col=colPdark,adj=1,cex=0.9)
                                    text(s-0.05,l_at_sample+0.05,labels=ltext,col="black",adj=0.6,cex=0.9)
                                  } 
                                }
                              } else {
                                s<-abs(sRho[i])
                                p_at_sample<-(sum(sourceSampDens_r_total[rs>=s])+sum(sourceSampDens_r_total[rs< -s]))/sum(sourceSampDens_r_total)
                                l_at_sample<-approx(rs,sourceSampDens_r_total,s)$y
                                
                                lines(x=c(sRho[1],sRho[1]),y=c(0,l_at_sample-0.01),col=colSdark,lwd=2)
                                
                                text(0,1.05,labels=bquote(
                                  bolditalic(p)[.(braw.env$RZ)]== bold(.(format(p_at_sample,digits=3)))
                                ),col=colPdark,adj=-0.1,cex=0.9)
                                text(0,1.05,labels=bquote(
                                  bolditalic(l)[.(braw.env$RZ)]==bold(.(format(l_at_sample/gain,digits=3)))
                                ),col=colPdark,adj=1.1,cex=0.9)
                              }
                            }
                            if (length(sRho)>1) {
                              l_at_sample<-sum(log(approx(rs,sourceSampDens_r_total,sRho)$y),na.rm=TRUE)#/mean(sourceSampDens_r_total)
                              ltext<-bquote(
                                bold(log(lk(.(braw.env$RZ)[s])))==.(format(l_at_sample,digits=3)) ~" "~ atop(phantom(.(format(ld_at_sample,digits=3))),phantom(.(format(ln_at_sample,digits=3))))
                              )
                              ltext<-bquote(
                                bold(log(lk(Z)))==.(format(l_at_sample,digits=3)) ~" "~ atop(phantom(.(format(ld_at_sample,digits=3))),phantom(.(format(ln_at_sample,digits=3))))
                              )
                              text(0+0.05,1+0.15,labels=ltext,col="black",adj=0.5,cex=0.9)
                            }
                          }
                        }
                      },
                      "Populations"={
                        if (!all(is.na(sampleLikelihood_r))){
                          # main distribution
                          polygon (x = c(rp[1],rp,rp[length(rp)]), y = c(0,sampleLikelihood_r_show,0), col = addTransparency(colP,theoryAlpha), lwd=1)
                          # vertical lines
                          dens_at_peak<-max(sampleLikelihood_r)
                          dens_at_zero<-approx(rp,sampleLikelihood_r,0)$y
                          dens_at_sample<-approx(rp,sampleLikelihood_r,sRho[1])$y
                          dens_at_ci<-approx(rp,sampleLikelihood_r,rp_ci)$y
                          # lines(x=c(sRho[1],sRho[1]),y=c(0,dens_at_sample-0.01),col="black",lwd=1.5)
                          if (world$populationPDF!="Uniform_r" && !is.null(rp_peak)){
                            lines(x=c(0,0),y=c(0,dens_at_zero-0.01),col="black",lwd=1.5)
                            lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="black",lwd=1.5)
                            # lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="black",lwd=2.5)
                            # lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="white",lwd=2)
                            # lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="red",lty=3,lwd=2)
                            
                            lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="black",lwd=2.5)
                            lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="white",lwd=2)
                            lines(x=c(0,0)+rp_ci[1],y=c(0,dens_at_ci[1]-0.01),col="red",lty=3,lwd=2)
                            lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="black",lwd=2.5)
                            lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="white",lwd=2)
                            lines(x=c(0,0)+rp_ci[2],y=c(0,dens_at_ci[2]-0.01),col="red",lty=3,lwd=2)
                          }
                          text(rp_peak,1.05,labels=bquote(
                            bolditalic(.(braw.env$RZ))[mle]== bold(.(format(rp_peak,digits=3)))
                          ),col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                          text(x=rp_peak,1.15,labels=bquote(
                            bold(llr)(bolditalic(.(braw.env$RZ))[mle]/bolditalic(.(braw.env$RZ))[0])==bold(.(format(log(1/dens_at_zero),digits=3)))
                          ),col=colPdark,adj=(sign(rp_peak)+1)/2,cex=0.9)
                          
                          if (possible$prior$worldOn && possible$prior$populationNullp>0) {
                            ln_at_sample<-approx(rs,priorSampDens_r_null,sRho[1])$y
                            ld_at_sample<-approx(rs,colMeans(priorSampDens_r_plus),sRho[1])$y
                            llrNull<-log(ln_at_sample/ld_at_sample)
                            text(xlim[1],1.15,labels=bquote(
                              bold(llr)(bolditalic(.(braw.env$RZ))["+"]/bolditalic(.(braw.env$RZ))[0])==bold(.(format(-llrNull,digits=3)))),
                              col=colPdark,adj=c(0),cex=0.9)
                          }
                        }
                      }
              )
              
            }
          },
          "flat"={
            if (!is.null(possible$targetSample) && !is.na(possible$targetPopulation)) {
              lines(trans3d(x=xlim,y=c(0,0)+possible$targetSample,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1)
              lines(trans3d(x=c(0,0)+possible$targetPopulation,y=ylim,z=c(0,0)+zlim[1],pmat=mapping),col=colVline,lwd=1,lty=2)
              points(trans3d(x=possible$targetPopulation,
                             y=possible$targetSample,
                             z=c(zlim[1],zlim[1],zlim[1]),pmat=mapping),pch=16,cex=1.5,col=colS)
            }
          }
  )
}
