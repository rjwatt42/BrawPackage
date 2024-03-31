showAxis<-function(showType,effect) {
  
switch(braw.env$RZ,
       "r"={
         rlims<-c(-1,1)
         rlab<-"r"
       },
       "z"={    
         rlims<-c(-1,1)*braw.env$z_range
         rlab<-"z"
       }
       )

  use_cols<-c("#FFFFFF","#DDDDDD","#AAAAAA")
  lines<-c()
  ybreaks<-NULL
  switch (showType,
          "r"={
            ylim<-rlims
            if (braw.env$RZ=="z") ylabel<-braw.env$zsLabel
            else ylabel<-braw.env$rsLabel 
            use_cols<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))
            lines<-c(0,effect$rIV)
          },
          "rp"={
            ylim<-rlims
            if (braw.env$RZ=="z") ylabel<-braw.env$zpLabel
            else ylabel<-braw.env$rpLabel 
            use_cols<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))
            lines<-c(0)
          },
          "ro"={
            ylim<-rlims
            ylabel<-bquote(r[o])
            use_cols<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))
            lines<-c(0,effect$rIV)
          },
          "p"={
            ylim<-c(braw.env$min_p, 1)
            ylabel<-bquote(p)
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
            lines<-c(0.05)
          },
          "po"={
            ylim<-c(braw.env$min_p, 1)
            ylabel<-bquote(p[o])
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "log(lrs)"={
            ylim<-c(0, braw.env$lrRange)
            ylabel<-bquote(log[e](lr[s]))
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "log(lrd)"={
            ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            ylabel<-bquote(log[e](lr[d]))
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "e1d"={
            ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            ylabel<-bquote(log[e](lr[d]))
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "e2d"={
            ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            ylabel<-bquote(log[e](lr[d]))
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "w"={
            ylim<-c(0.01, 1)
            ylabel<-bquote(w)
            use_cols<-c(hsv(0.65,1,1),hsv(0.65+0.075,1,1),hsv(0.65+0.15,1,1))
            lines<-c(0.05,0.8)
          },
          "wn"={
            ylim<-c(10, braw.env$max_nw*10)
            ylabel<-bquote(n[w=80])
          },
          "n"={
            ylim<-c(1, 50*5*1.1)
            ylabel<-"n"
          },
          "wp"={
            ylim<-c(0.01, 1)
            ylabel<-bquote(w)
            use_cols<-c(hsv(0.65,1,1),hsv(0.65+0.075,1,1),hsv(0.65+0.15,1,1))
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
            ylim<-c(braw.env$min_p, 1)
            ylabel<-bquote(p)
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "e2"={
            ylim<-c(braw.env$min_p, 1)
            ylabel<-bquote(p)
            use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))
          },
          "FDR"={
            ylim<-c(0,1)
            ylabel<-"False Discovery"
            use_cols<-braw.env$plotColours$fdr
          },
          "NHST"={
            ylim<-c(0,1)
            ylabel<-"Outcomes"
          },
          "p(sig)"={
            ylim<-c(0,1)
            ylabel<-"p(sig)"
            lines<-c(0.05,0.8)
            use_cols<-c(braw.env$plotColours$infer_sigC,
                        braw.env$plotColours$infer_nsigC,
                        braw.env$plotColours$infer_sigNull,
                        braw.env$plotColours$infer_nsigNull)
            
            },
          "FMR"={
            ylim<-c(0,1)
            ylabel<-"False Miss"
            use_cols<-braw.env$plotColours$fmr
          },
          "k"={
            ylim<-c(-0.01,1.01)
            ylabel<-braw.env$Llabel
          },
          "pNull"={
            ylim<-c(-0.01,1.01)
            ylabel<-braw.env$Plabel
          },
          "PDF"={
            ylim<-c(0,1)
            ylabel<-"p(PDF)"
          },
          "S"={
            ylim<-c(min(result$Ss),max(result$Ss))
            ylabel<-"S"
          }
  )
  
  logScale<-FALSE
  if (is.element(showType,c("w","wp")) && braw.env$wPlotScale=="log10"){
    ylim<-log10(ylim)
    if (!is.null(lines))
      lines<-log10(lines)
    ylabel<-bquote(bold(log['10'](.(ylabel))))
    logScale<-TRUE
  }
  if (is.element(showType,c("n","wn")) && braw.env$nPlotScale=="log10"){
    ylim<-log10(c(5,1000))
    if (!is.null(lines))  lines<-log10(lines)
    ylabel<-bquote(bold(log['10'](.(ylabel))))
    logScale<-TRUE
  }
  if (is.element(showType,c("p","e1","e2","po")) && braw.env$pPlotScale=="log10") {
    ylim<-c(-4,0)
    lines<-log10(c(0.05,0.01,0.005,0.001))
    ylabel<-bquote(bold(log['10'](.(ylabel))))
    logScale<-TRUE
  }
  
  # if (logScale) {
  #     ylim<-log10(ylim)
  #   ylabel<-bquote(bold(log['10'](.(ylabel))))
  # }  else {
  #   ylabel<-bquote(.(ylabel))
  # }
  # 
  return(list(lim=ylim,label=ylabel,cols=use_cols,lines=lines,logScale=logScale))
}
