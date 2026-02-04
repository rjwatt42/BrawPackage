plotAxis<-function(showType,hypothesis,design=NULL,result=NULL) {
  fixedAxis=TRUE
  
  effect<-hypothesis$effect
  explicitLog<-FALSE
  base_hue_r<-0.1
  base_hue_p<-0.15
  base_hue_w<-0.65
  hue_diff<-0.03
  
  logScale<-(is.element(showType,c("p","pe","po","e1p","e2p")) && braw.env$pPlotScale=="log10") ||
    (is.element(showType,c("ws","wp")) && braw.env$wPlotScale=="log10") ||
    (is.element(showType,c("n","no")) && braw.env$nPlotScale=="log10") ||
    (is.element(showType,c("nw","llknull")))
  
  switch(braw.env$RZ,
         "r"={
           rlims<-c(-1,1)
           rslab<-braw.env$rsLabel 
           rplab<-braw.env$rpLabel 
           rticks<-seq(-1,1,0.5)
           rmins<-seq(-1,1,0.1)
         },
         "z"={    
           rlims<-c(-1,1)*braw.env$z_range
           rslab<-braw.env$zsLabel
           rplab<-braw.env$zpLabel
           rticks<-seq(-2.5,2.5,0.5)
           rmins<-seq(-2.5,2.5,0.1)
         },
         "d"={    
           rlims<-c(-1,1)*braw.env$d_range
           rslab<-braw.env$dsLabel
           rplab<-braw.env$dpLabel
           rticks<-seq(-5,5,1)
           rmins<-seq(-5,5,0.5)
         }
  )
  plabel<-"p"
  polabel<-"p[o]"
  switch(braw.env$pPlotScale,
         "log10"={
           plim<-c(log10(braw.env$min_p),0)
           pticks<-seq(log10(braw.env$min_p),0,1)
           pmins<-log10(c(seq(1,10)/10000,seq(1,10)/1000,seq(1,10)/100,seq(1,10)/10))
           plines<-log10(c(0.05,0.01,0.005,0.001))
           if (explicitLog) {
             plabel<-"log[10](p)"
             polabel<-"log[10](p[o])"
           }
         },
         "linear"={
           plim<-c(braw.env$min_p, 1)
           pticks<-seq(0,1,0.2)
           pmins<-seq(0,1,0.1)
           plines<-c(0.05)
         }
         )
  wslabel<-"w[s]"
  wplabel<-"w[p]"
  switch (braw.env$wPlotScale,
          "log10"={
            wlim<-c(-2,0)
            wticks<-seq(-2,0,1)
            wmins<-log10(c(seq(1,10)/100,seq(1,10)/10))
            if (explicitLog) {
              wslabel<-"log[10]w[s]"
              wplabel<-"log[10]w[p]"
            }
            wlines<-log10(c(0.05,0.8))
          },
          "linear"={
            wlim<-c(0, 1)
            wticks<-seq(0,1,0.2)
            wmins<-seq(0,1,0.1)
            wlines<-c(0.05,0.8)
          }
  )
  nlabel<-"n"
  switch(braw.env$nPlotScale,
         "log10"={
           nlim<-log10(c(5,2000))
           nticks<-seq(1,3,1)
           nmins<-log10(c(seq(5,10),seq(1,10)*10,seq(1,10)*100))
           if (explicitLog) {
             nlabel<-"log[10](n)"
           }
         },
         "linear"={
           nlim<-c(1, 50*5*1.1)
           nticks<-seq(0,250,50)
           nmins<-seq(0,250,10)
         }
  )
  
  use_cols<-c("#FFFFFF","#DDDDDD","#AAAAAA")
  ylines<-c()
  ymins<-c()
  yticks<-c()
  
  switch (showType,
          "rs"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "rse"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "sig"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "ns"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "nonnulls"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "nulls"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "rss"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "rp"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rplab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            # ylines<-c(0)
          },
          "re"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-'r[e]'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0)
          },
          "ro"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-'r[o]'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "p"={
            ylim<-plim
            yticks<-pticks
            ymins<-pmins
            ylabel<-plabel
            ylines<-plines
            use_cols<-c(hsv(base_hue_p,1,1),hsv(base_hue_p+hue_diff,1,1),hsv(base_hue_p+hue_diff*2,1,1))
          },
          "pe"={
            ylim<-plim
            yticks<-pticks
            ymins<-pmins
            ylabel<-plabel
            ylines<-plines
            use_cols<-c(hsv(base_hue_p,1,1),hsv(base_hue_p+hue_diff,1,1),hsv(base_hue_p+hue_diff*2,1,1))
          },
          "po"={
            ylim<-plim
            yticks<-pticks
            ymins<-pmins
            ylines<-plines
            ylabel<-polabel
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_p+hue_diff,1,1),hsv(base_hue_p+hue_diff*2,1,1))
          },
          "llknull"={
            ylim<-log10(c(0.1, 100000))
            ylabel<-'llr[+]'
            ylines<-log10(c(1))
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "SEM"={
            ylim<-c(0,1)
            ylabel<-'Outcomes'
            use_cols<-c(rep("white",7))
            if (is.null(hypothesis$IV2)) {
              if (effect$world$On) {
                use_cols[1]<-braw.env$plotColours$infer_nsigNull
                use_cols[2]<-braw.env$plotColours$infer_sigNull
                use_cols[3]<-braw.env$plotColours$infer_sigNonNull
                use_cols[4]<-braw.env$plotColours$infer_nsigNonNull
              } else {
                use_cols[1]<-braw.env$plotColours$infer_nsigNull
                use_cols[2]<-braw.env$plotColours$infer_sigNull
              }
            }
            if (!is.null(hypothesis$IV2)) {
              if (effect$world$On) {
                use_cols[1]<-braw.env$plotColours$infer_nsigNull
                use_cols[2:7]<-braw.env$plotColours$infer_sigNull
                use_cols[8:13]<-braw.env$plotColours$infer_sigNonNull
                use_cols[14]<-braw.env$plotColours$infer_nsigNonNull
              } else {
                if (effect$rIV==0 && effect$rIV2==0 && effect$rIVIV2==0 ) {
                  use_cols[1]<-braw.env$plotColours$infer_nsigNull
                  use_cols[2:7]<-braw.env$plotColours$infer_sigNull
                } else {
                  use_cols[1]<-braw.env$plotColours$infer_nsigNonNull
                  for (i in 2:7) # 2 effects missed
                    use_cols[i]<-darken(
                      blend(braw.env$plotColours$infer_nsigNonNull,
                            braw.env$plotColours$infer_sigNonNull,0.7),
                      off=0.1+(i-1)/14)
                  
                  if (effect$rIV!=0 && effect$rIV2!=0 && effect$rIVIV2!=0 )
                    use_cols[7]<-braw.env$plotColours$infer_sigNonNull
                  if (effect$rIV!=0 && effect$rIV2!=0 && effect$rIVIV2==0 )
                    use_cols[6]<-braw.env$plotColours$infer_sigNonNull
                  if (effect$rIV==0 && effect$rIV2!=0 && effect$rIVIV2!=0 ) {
                      errors<-c(1,0,2,1,3,1,2)
                      for (i in 1:7) # 2 effects missed
                        use_cols[i]<-darken(
                          blend(braw.env$plotColours$infer_nsigNonNull,
                                braw.env$plotColours$infer_sigNonNull,
                                ((3-errors[i])/3)^0.25),
                          off=0)
                          # off=(sum(errors[1:i]==errors[i])/sum(errors==errors[i])-1)/9)
                  }
                  if (effect$rIV!=0 && effect$rIV2==0 && effect$rIVIV2!=0 )
                    use_cols[4]<-braw.env$plotColours$infer_sigNonNull
                  if (effect$rIV==0 && effect$rIV2!=0 && effect$rIVIV2==0 )
                    use_cols[3]<-braw.env$plotColours$infer_sigNonNull
                  if (effect$rIV!=0 && effect$rIV2==0 && effect$rIVIV2==0 )
                    use_cols[2]<-braw.env$plotColours$infer_sigNonNull
                }
              }
            }
          },
          "AIC"={
            ylim<-c(1.5*design$sN,3.5*design$sN)*hypothesis$DV$sd
            ylabel<-'AIC[1]'
            use_cols<-c(rep("white",7))
            if (is.null(hypothesis$IV2)) {
              if (effect$world$On) {
                use_cols[1]<-braw.env$plotColours$infer_nsigNull
                use_cols[2]<-braw.env$plotColours$infer_sigNull
                use_cols[3]<-braw.env$plotColours$infer_sigNonNull
                use_cols[4]<-braw.env$plotColours$infer_nsigNonNull
              } else {
                if (effect$rIV==0) {
                  use_cols[1]<-braw.env$plotColours$infer_nsigNull
                  use_cols[2]<-braw.env$plotColours$infer_sigNull
                } else {
                  use_cols[1]<-braw.env$plotColours$infer_nsigNull
                  use_cols[2]<-braw.env$plotColours$infer_sigNull
                }
              }
              }
            if (!is.null(hypothesis$IV2)) {
              if (effect$world$On) {
                use_cols[1]<-braw.env$plotColours$infer_nsigNull
                use_cols[2:7]<-braw.env$plotColours$infer_sigNull
                use_cols[8:13]<-braw.env$plotColours$infer_sigNonNull
                use_cols[14]<-braw.env$plotColours$infer_nsigNonNull
              } else {
              if (effect$rIV==0 && effect$rIV2==0 && effect$rIVIV2==0 ) {
                use_cols[1]<-braw.env$plotColours$infer_nsigNull
                use_cols[2:7]<-braw.env$plotColours$infer_sigNull
              } else {
                use_cols[1]<-braw.env$plotColours$infer_nsigNonNull
                use_cols[2:7]<-darken(
                  blend(braw.env$plotColours$infer_nsigNonNull,braw.env$plotColours$infer_sigNonNull,0.7),
                  off=0.5)
                if (effect$rIV!=0 && effect$rIV2!=0 && effect$rIVIV2!=0 )
                  use_cols[7]<-braw.env$plotColours$infer_sigNonNull
                if (effect$rIV!=0 && effect$rIV2!=0 && effect$rIVIV2==0 )
                  use_cols[6]<-braw.env$plotColours$infer_sigNonNull
                if (effect$rIV==0 && effect$rIV2!=0 && effect$rIVIV2!=0 )
                  use_cols[5]<-braw.env$plotColours$infer_sigNonNull
                if (effect$rIV!=0 && effect$rIV2==0 && effect$rIVIV2!=0 )
                  use_cols[4]<-braw.env$plotColours$infer_sigNonNull
                if (effect$rIV==0 && effect$rIV2!=0 && effect$rIVIV2==0 )
                  use_cols[3]<-braw.env$plotColours$infer_sigNonNull
                if (effect$rIV!=0 && effect$rIV2==0 && effect$rIVIV2==0 )
                  use_cols[2]<-braw.env$plotColours$infer_sigNonNull
              }
              }
            }
          },
          "sLLR"={
            ylim<-c(0, braw.env$lrRange)
            ylabel<-'log[e](lr[s])'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "log(lrs)"={
            ylim<-c(0, braw.env$lrRange)
            ylabel<-'log[e](lr[s])'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "log(lrd)"={
            ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            ylabel<-'log[e](lr[d])'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "e1d"={
            ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            ylabel<-'log[e](lr[d])'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "e2d"={
            ylim<-c(-braw.env$lrRange, braw.env$lrRange)
            ylabel<-'log[e](lr[d])'
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "ws"={
            ylim<-wlim
            yticks<-wticks
            ymins<-wmins
            ylabel<-wslabel
            ylines<-wlines
            use_cols<-c(hsv(base_hue_w,1,1),hsv(base_hue_w+hue_diff,1,1),hsv(base_hue_w+hue_diff*2,1,1))
          },
          "nw"={
            ylim<-log10(c(5,2000))
            yticks<-seq(0,3,1)
            if (explicitLog)
              ylabel<-'log[10](n[w=80])'
            else
              ylabel<-'n[w=80]'
            logScale<-TRUE
          },
          "n"={
            ylim<-nlim
            yticks<-nticks
            ymins<-nmins
            ylabel<-nlabel
          },
          "no"={
            ylim<-nlim
            yticks<-nticks
            ymins<-nmins
            if (explicitLog)
              ylabel<-"log[10](no)"
            else
              ylabel<-"no"
          },
          "wp"={
            ylim<-wlim
            yticks<-wticks
            ymins<-log10(c(seq(1,10),seq(1,10)*10,seq(1,10)*100))
            ylabel<-wplabel
            ylines<-wlines
            use_cols<-c(hsv(base_hue_w,1,1),hsv(base_hue_w+hue_diff,1,1),hsv(base_hue_w+hue_diff*2,1,1))
          },
          "ci1"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "ci2"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e1r"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e2r"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e1+"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e2+"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e1-"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e2-"={
            ylim<-rlims
            yticks<-rticks
            ymins<-rmins
            ylabel<-rslab
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
            ylines<-c(0,effect$rIV)
          },
          "e1p"={
            ylim<-plim
            yticks<-pticks
            ymins<-pmins
            ylabel<-plabel
            ylines<-plines
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "e2p"={
            ylim<-plim
            yticks<-pticks
            ymins<-pmins
            ylabel<-plabel
            ylines<-plines
            use_cols<-c(hsv(base_hue_r,1,1),hsv(base_hue_r+hue_diff,1,1),hsv(base_hue_r+hue_diff*2,1,1))
          },
          "Hits"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"True Hits"
            use_cols<-braw.env$plotColours$fdr
          },
          "NHST"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"Outcomes"
          },
          "Inference"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            use_cols<-c(braw.env$plotColours$fdr,braw.env$plotColours$fmr)
            ylabel<-"p(correct)"
          },
          "Source"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            use_cols<-c(braw.env$plotColours$fdr,braw.env$plotColours$fmr)
            ylabel<-"p(sig)"
          },
          "p(sig)"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"p(sig)"
            lines<-c(0.05,0.8)
            use_cols<-c(braw.env$plotColours$infer_sigC,
                        braw.env$plotColours$infer_nsigC,
                        braw.env$plotColours$infer_sigNull,
                        braw.env$plotColours$infer_nsigNull)
            
          },
          "p(w80)"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"p(w>0.8)"
            lines<-c()
            use_cols<-c(braw.env$plotColours$infer_sigC,
                        braw.env$plotColours$infer_nsigC,
                        braw.env$plotColours$infer_sigNull,
                        braw.env$plotColours$infer_nsigNull)
            
          },
          "Misses"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"False Miss"
            use_cols<-braw.env$plotColours$fmr
          },
          "n(sig)"={
            ylim<-c(0,10)
            yticks<-0:10
            ymins<-0:10
            ylabel<-"n(sig)"
            use_cols<-braw.env$plotColours$infer_sigNonNull
          },
          "n(fd)"={
            ylim<-c(0,10)
            yticks<-1:10
            ymins<-1:10
            ylabel<-"n(false discoveries)"
            use_cols<-"#FFFFFF"
          },
          "metaRiv"={
            ylim<-c(-1.01,1.01)
            yticks<-seq(-1,1,0.2)
            ymins<-seq(-1,1,0.1)
            ylabel<-"r[m]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaRsd"={
            ylim<-c(-0.01,0.5)
            yticks<-seq(0,0.5,0.1)
            ymins<-seq(0,0.5,0.05)
            ylabel<-"r[sd]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaSpread"={
            ylim<-c(-0.01,0.5)
            yticks<-seq(0,0.5,0.1)
            ymins<-seq(0,0.5,0.05)
            ylabel<-"r[sd]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaShape"={
            ylim<-c(-0.01,5)
            yticks<-seq(0,5,1)
            ymins<-seq(0,0.5,0.05)
            ylabel<-"PDF[sh]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaRvar"={
            ylim<-c(-0.05,0.2)
            yticks<-seq(-0.05,0.2,0.05)
            ymins<-seq(-0.05,0.2,0.01)
            ylabel<-"var(r)[m]"
            lines<-0
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaBias"={
            ylim<-c(-0.05,1.05)
            yticks<-seq(0,0.1,1)
            ymins<-seq(0,0.1,1)
            ylabel<-"bias[m]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaK"={
            ylim<-c(-0.01,1.01)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-braw.env$Llabel
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "metaPRplus"={
            ylim<-c(-0.01,1.01)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-braw.env$Plabel
            if (braw.env$RZ=="z") ylabel<-sub("R","Z",ylabel)
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "PDFk"={
            ylim<-c(-0.01,1.01)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"PDF[k]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "PDFshape"={
            ylim<-c(-0.01,6)
            yticks<-seq(0,5,1)
            ymins<-seq(0,0.5,0.05)
            ylabel<-"PDF[shape]"
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "Lambda"={
            ylim<-c(-0.01,1.01)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-braw.env$Llabel
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "p(R+)"={
            ylim<-c(-0.01,1.01)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-braw.env$Plabel
            if (braw.env$RZ=="z") ylabel<-sub("R","Z",ylabel)
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "mean(R+)"={
            ylim<-c(-0.01,1.01)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-braw.env$Llabel
            # if (braw.env$graphicsType=="ggplot")
            #   ylabel<-sub("[+]","[+]",ylabel)
            if (braw.env$RZ=="z") ylabel<-sub("R","Z",ylabel)
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "PDF"={
            ylim<-c(0,1)
            yticks<-seq(0,1,0.2)
            ymins<-seq(0,1,0.1)
            ylabel<-"p(PDF)"
          },
          "metaSmax"={
            ylabel<-"S[max]"
            if (!is.null(result)) 
              ylim<-c(min(result),max(result))
            else ylim<-c(0,1)
            if (diff(ylim)==0) ylim<-ylim+c(-1,1)
            yticks<-seq(ceil(ylim[1]),ceil(ylim[2]),1)
            use_cols<-braw.env$plotColours$metaMultiple
          },
          "iv.mn"={
            var<-hypothesis$IV
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-5*var$sd*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)+var$mu
            yticks<-seq(-l2,l2,0.5)+var$mu
            ymins<-seq(-l3,l3,0.1)+var$mu
            ylabel<-"mean"
          },
          "iv.sd"={
            var<-hypothesis$IV
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-5*var$sd*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)+var$sd
            yticks<-seq(-l2,l2,0.5)+var$sd
            ymins<-seq(-l2,l3,0.1)+var$sd
            ylabel<-"sd"
          },
          "iv.sk"={
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-2*5*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)
            yticks<-seq(-l2,l2,0.5)
            ymins<-seq(-l3,l3,0.1)
            ylabel<-"skew"
          },
          "iv.kt"={
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-3*5*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)
            yticks<-seq(-l2,l2,0.5)
            ymins<-seq(-l3,l3,0.1)
            ylabel<-"kurtosis"
          },
          "dv.mn"={
            var<-hypothesis$DV
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-5*var$sd*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)+var$mu
            yticks<-seq(-l2,l2,0.5)+var$mu
            ymins<-seq(-l3,l3,0.1)+var$mu
            ylabel<-"mean"
          },
          "dv.sd"={
            var<-hypothesis$DV
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-5*var$sd*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)+var$sd
            yticks<-seq(-l2,l2,0.5)+var$sd
            ymins<-seq(-l2,l3,0.1)+var$sd
            ylabel<-"sd"
          },
          "dv.sk"={
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-2*5*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)
            yticks<-seq(-l2,l2,0.5)
            ymins<-seq(-l3,l3,0.1)
            ylabel<-"skew"
          },
          "dv.kt"={
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-3*5*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)
            yticks<-seq(-l2,l2,0.5)
            ymins<-seq(-l3,l3,0.1)
            ylabel<-"kurtosis"
          },
          "er.mn"={
            var<-hypothesis$DV
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-5*var$sd*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)+0
            yticks<-seq(-l2,l2,0.5)+0
            ymins<-seq(-l3,l3,0.1)+0
            ylabel<-"mean"
          },
          "er.sd"={
            var<-hypothesis$DV
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-5*var$sd*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)+var$sd
            yticks<-seq(-l2,l2,0.5)+var$sd
            ymins<-seq(-l2,l3,0.1)+var$sd
            ylabel<-"sd"
          },
          "er.sk"={
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-2*5*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)
            yticks<-seq(-l2,l2,0.5)
            ymins<-seq(-l3,l3,0.1)
            ylabel<-"skew"
          },
          "er.kt"={
            if (fixedAxis) gain<-1/6.5 else gain<- 1/sqrt(design$sN)
            l1<-3*5*gain
            l2<-floor(l1/0.5)*0.5
            l3<-floor(l1/0.1)*0.1
            ylim<-c(-l1,l1)
            yticks<-seq(-l2,l2,0.5)
            ymins<-seq(-l3,l3,0.1)
            ylabel<-"kurtosis"
          }
  )
  
  return(list(lim=ylim,ticks=yticks,label=ylabel,cols=use_cols,lines=ylines,logScale=logScale))
}
