no_se_multiple<-TRUE
multiOverlap<-FALSE
valsGap<-1.4
doBudget<-1
ErrorsWorld<-"1scale"
# ErrorsWorld<-"2scale"
all_cols<-c()

drawNHSTBar<-function(i,npts,pts1,bwidth,col1) {
  barx<-c(-1,-1,1,1)*bwidth
  bary<-c(i,npts*2-i+1,npts*2-i+1,i)
  
  y1<-pts1$y[bary]
  x1<-pts1$x[i]+barx
  pts<-data.frame(x=x1,y=y1)
  geom_polygon(data=pts,aes(x=x,y=y),fill=col1)
}
drawNHSTLabel<-function(lb1,lb1xy,xoff,col1) {
  mathlabel<-grepl("['^']{1}",lb1) | grepl("['[']{1}",lb1)
  if (any(mathlabel)) {
    lb1<-deparse(lb1)
    mathlabel<-TRUE
  }
  if (sum(col2rgb(col1))>128*3) col<-"black" else col<-"white"
  geom_label(data=lb1xy,aes(x=x+xoff,y=y),label=lb1,
             hjust=-0.2,vjust=0.5,size=labelSize,colour=col,fill=col1,parse=mathlabel)
}

drawExplore<-function(IV,IV2,DV,effect,design,explore,exploreResult){
  oldAlpha<-alphaSig
  
  vals<-exploreResult$result$vals
  if (is.character(vals[1]) || is.element(explore$Explore_type,c("IVcats","IVlevels","DVcats","DVlevels","Repeats","sig_only"))){
    if (is.character(vals[1]))  vals<-((1:length(vals))-1)/(length(vals)-1)
    doLine=FALSE
  } else {doLine=TRUE}
  
  if (explore$Explore_type=="pNull" && pPlus) vals<-1-vals
  
  g<-ggplot()
  ybreaks=c()
  ylabels=c()
  secondY<-NULL
  ylim<-c()
  switch (explore$Explore_show,
          "EffectSize"={
            ylim<-c(-1,1)
            ylabel<-bquote(bold(r['s']))
            if (RZ=="z") {
              ylim<-c(-1,1)*z_range
              ylabel<-bquote(bold(z['s']))
            }
          },
          "EffectSizeA"={
            ylim<-c(-1,1)
            ylabel<-bquote(bold(r['a']))
            if (RZ=="z") {
              ylim<-c(-1,1)*z_range
              ylabel<-bquote(bold(z['a']))
            }
          },
          "EffectSize1"={
            ylim<-c(-1,1)
            ylabel<-bquote(bold(r['a']))
            if (RZ=="z") {
              ylim<-c(-1,1)*z_range
              ylabel<-bquote(bold(z['a']))
            }
          },
          "p"={
            if (pPlotScale=="log10") {
              ylim<-c(-4,0.1)
              ylabel<-bquote(bold(log['10'](p)))
              ybreaks<-c(-4,-3,-2,-1,0)
              ylabels<-c(0.0001,0.001,0.01,0.1,1)
            } else {
              ylim<-c(0,1)
              ylabel<-"p"
              ybreaks<-seq(0,1,0.2)
              ylabels<-ybreaks
            }
            g<-g+scale_y_continuous(breaks=ybreaks,labels=ylabels)
          },
          "w"={
            if (wPlotScale=="log10"){
              ylim<-c(-2,0)
              ylabel<-bquote(bold(log['10'](w['est'])))
              ybreaks=c(-2,-1,0)
              ylabels=c(0.01,0.1,1)
              g<-g+scale_y_continuous(breaks=ybreaks,labels=ylabels)
            } else {
              ylim<-c(0,1)
              ylabel<-bquote(bold(w['est']))
            }
          },
          "p(sig)"={
            doBudget<-FALSE
            ylabel<-pSigLabel
            if (explore$Explore_ylog) {
              ylim<-c(0.001,1)
            } else {
              ylim<-c(0,1)
            }
          },
          "n(sig)"={
            doBudget<-1
            ylabel<-bquote(bold(n[.('sig')]))
            explore$Explore_show<-"p(sig)"
            ylim<-c(0,100)
          },
          "FDR"={
            if (explore$Explore_ylog) {
              ylim<-c(0.001,1)
            } else {
              ylim<-c(0,1)
            }
            ylabel<-"False Discovery"
          },
          "NHSTErrors"={
            ylim<-c(0,1)
            if (ErrorsWorld=="1scale") {
              ylabel<-"Errors"
            } else {
              ylabel<-"Type I"
              secondY<-"Type II"
              g<-g+theme(axis.title.y.left = element_text(color=plotcolours$infer_sigNull),axis.title.y.right = element_text(color=plotcolours$infer_nsNonNull))
            }
          },
          "FDR;FMR"={
            ylim<-c(0,1)
            ylabel<-"False Discovery"
            secondY<-"False Miss"
              g<-g+theme(axis.title.y.left = element_text(color=plotcolours$fdr),axis.title.y.right = element_text(color=plotcolours$fmr))
          },
          "log(lrs)"={
            ylim<-c(-0.1,10)
            ylabel<-bquote(bold(log['e'](lr['s'])))
          },
          "log(lrd)"={
            ylim<-c(-1,1)*lrRange
            ylabel<-bquote(bold(log['e'](lr['d'])))
          },
          "likelihood"={
            ylim<-c(-10,10)
            ylabel<-bquote(bold(log['e'](lr['d'])))
          },
          "k"={
            ylim<-c(-0.01,1.01)
            ylabel<-Llabel
          },
          "SampleSize"={
            ylim<-c(minN,maxRandN*design$sN)
            ylabel<-"n"
          },
          "t"={
            ylim<-c()
            ylabel<-"t"
          },
          "pNull"={
            ylim<-c(-0.01,1.01)
            ylabel<-Plabel
          },
          "PDF"={
            ylim<-c(0,1)
            ylabel<-"p(PDF)"
          },
          "S"={
            ylim<-c(min(exploreResult$result$Ss),max(exploreResult$result$Ss))
            ylabel<-"S"
          },
          "mean(IV)"={ylabel<-"mean(IV)"},
          "sd(IV)"={ylabel<-"sd(IV)"},
          "skew(IV)"={ylabel<-"skew(IV)"},
          "kurtosis(IV)"={ylabel<-"kurtosis(IV)"},
          "mean(DV)"={ylabel<-"mean(DV)"},
          "sd(DV)"={ylabel<-"sd(DV)"},
          "skew(DV)"={ylabel<-"skew(DV)"},
          "kurtosis(DV)"={ylabel<-"kurtosis(DV)"}
  )

  if (!is.null(IV2) && is.element(explore$Explore_show,c("EffectSize","EffectSizeA","p","w","p(sig)"))) {
    switch (explore$Explore_show,
            "EffectSize"={use_cols<<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))},
            "EffectSizeA"={use_cols<<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))},
            "p"=         {use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))},
            "w"=         {use_cols<-c(hsv(0.65,1,1),hsv(0.65+0.075,1,1),hsv(0.65+0.15,1,1))},
            "p(sig)"=    {use_cols<-c("#FFFFFF","#DDDDDD","#AAAAAA")},
    )
    names(use_cols)<-c("direct","unique","total")
    all_cols<<-use_cols
    g<-g+scale_fill_manual(name=explore$Explore_whichShow,values=all_cols)
    use_col_names<-TRUE
  } else {
    all_cols<-c()
    use_col_names<-FALSE
  }
  
  markersize<-5
  ni_max1<-1
  ni_max2<-1
  multi="none"
  if (explore$Explore_typeShow=="all") { # all of direct/unique/total
    markersize<-4
    ni_max1<-3
    multi<-"allTypes"
  } 
  if (explore$Explore_whichShow=="All") { # all of main1 main2 interaction
    markersize<-4
    ni_max2<-3
    multi<-"allEffects"
  } else {
    if (explore$Explore_whichShow=="Mains") { 
      markersize<-6
      ni_max2<-2
      multi<-"mainEffects"
    }
  }
  
  for (ni1 in 1:ni_max1){
    for (ni2 in 1:ni_max2){
      if (ni_max1>1) {
      switch (ni1,
            {explore$Explore_typeShow<-"direct"},
            {explore$Explore_typeShow<-"unique"},
            {explore$Explore_typeShow<-"total"})
      } 
      if (ni_max2>1) {
      switch (ni2,
              {explore$Explore_whichShow<-"Main 1"},
              {explore$Explore_whichShow<-"Main 2"},
              {explore$Explore_whichShow<-"Interaction"})
      }

    extra_y_label<-""
    if (is.null(IV2)){
      rVals<-exploreResult$result$rIVs
      pVals<-exploreResult$result$pIVs
    } else {
      switch (explore$Explore_whichShow,
              "Main 1"={
                rVals<-exploreResult$result$r1[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p1[[explore$Explore_typeShow]]
                extra_y_label<-paste("Main Effect 1:",explore$Explore_typeShow)
              },
              "Main 2"={
                rVals<-exploreResult$result$r2[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p2[[explore$Explore_typeShow]]
                extra_y_label<-paste("Main Effect 2:",explore$Explore_typeShow)
              },
              "Interaction"={
                rVals<-exploreResult$result$r3[[explore$Explore_typeShow]]
                pVals<-exploreResult$result$p3[[explore$Explore_typeShow]]
                extra_y_label<-paste("Interaction:",explore$Explore_typeShow)
              }
      )
    }
    rpVals<-exploreResult$result$rpIVs
    raVals<-exploreResult$result$raIVs
    nVals<-exploreResult$result$nvals
    df1Vals<-exploreResult$result$df1vals
    
    switch (explore$Explore_show,
            "EffectSize"={
              showVals<-rVals
              if (RZ=="z") {showVals<-atanh(rVals)}
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
                if (RZ=="z") {lines<-c(0,atanh(effect$rIV))}
              } else {
                switch (explore$Explore_whichShow,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                          if (RZ=="z") {lines<-c(0,atanh(effect$rIV))}
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                          if (RZ=="z") {lines<-c(0,atanh(effect$rIV2))}
                        },
                        "Interaction"={
                          lines<-c(0,effect$rIVIV2DV)
                          if (RZ=="z") {lines<-c(0,atanh(effect$rIVIV2DV))}
                        }
                )
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "EffectSizeA"={
              showVals<-raVals
              if (RZ=="z") {showVals<-atanh(raVals)}
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
                if (RZ=="z") {lines<-c(0,atanh(effect$rIV))}
              } else {
                switch (explore$Explore_whichShow,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                          if (RZ=="z") {lines<-c(0,atanh(effect$rIV))}
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                          if (RZ=="z") {lines<-c(0,atanh(effect$rIV2))}
                        },
                        "Interaction"={
                          lines<-c(0,effect$rIVIV2DV)
                          if (RZ=="z") {lines<-c(0,atanh(effect$rIVIV2DV))}
                        }
                )
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "p"={
              showVals<-pVals
              lines<-c(0.05)
              if (pPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(IV2)){
                col<-"red"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "t"={
              showVals<-exploreResult$result$tvals
              lines<-c()
              if (is.null(IV2)){
                col<-"orange"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "w"={
              showVals<-rn2w(rVals,exploreResult$result$nvals)
              lines<-c(0.05,0.8)
              if (wPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "SampleSize"={
              showVals<-exploreResult$result$nvals
              lines<-c(design$sN)
              if (is.null(IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "likelihood"={
              showVals<-exploreResult$result$likes
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "log(lrs)"={
              ns<-exploreResult$result$nvals
              df1<-exploreResult$result$df1
              showVals<-r2llr(rVals,ns,df1,"sLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
              
              lines<-c()
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "log(lrd)"={
              ns<-exploreResult$result$nvals
              df1<-exploreResult$result$df1
              showVals<-r2llr(rVals,ns,df1,"dLLR",exploreResult$evidence$llr,exploreResult$evidence$prior)
              
              lines<-c()
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "k"={
              showVals<-exploreResult$result$ks
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "pNull"={
              showVals<-exploreResult$result$pnulls
              if (pPlus) showVals<-1-showVals
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "S"={
              showVals<-exploreResult$result$Ss
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "mean(DV)"={
              showVals<-exploreResult$result$dvMean
              
              lines<-c()
              col<-"yellow"
              colFill<-col
            },
            "sd(DV)"={
              showVals<-exploreResult$result$dvSD
              
              lines<-c()
              col<-"yellow"
              colFill<-col
            },
            "skew(DV)"={
              showVals<-exploreResult$result$dvSkew
              
              lines<-c()
              col<-"yellow"
              colFill<-col
            },
            "kurtosis(DV)"={
              showVals<-exploreResult$result$dvKurtosis
              
              lines<-c()
              col<-"yellow"
              colFill<-col
            },
            
            
            
            "p(sig)"={
              if (explore$Explore_type=="Alpha") {
                alphaSig<-exploreResult$result$vals
              }
              if (doBudget) {
                getStat<-function(x,n) {colMeans(x)*max(n)/colMeans(n)}
              } else {
                getStat<-function(x,n) {colMeans(x)}
              }
              ps<-isSignificant(STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,alphaSig)
              ps_mn<-getStat(abs(ps),nVals)
              ps1<-colMeans(abs(ps))
              p_se<-sqrt(ps1*(1-ps1)/nrow(pVals))*(ps_mn/ps1)
              y50<-ps_mn
              y25<-ps_mn+p_se*qnorm(0.25)
              y38<-ps_mn+p_se*qnorm(0.375)
              y62<-ps_mn+p_se*qnorm(0.625)
              y75<-ps_mn+p_se*qnorm(0.75)
              y50e<-c()
              y50a<-c()
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-plotcolours$infer_sigC
                cole<-plotcolours$infer_sigNull
                cola<-plotcolours$infer_nsNonNull
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
              
              if (effect$world$worldOn && effect$world$populationNullp>0) {
                g<-g+scale_fill_manual(name="outcome",values=c(plotcolours$psig,plotcolours$infer_sigC,plotcolours$infer_sigNull))
                col<-"sig"
                cole<-"sig|Z0"
                cola<-"sig|Z+"
                nulls<-rpVals==0
                ps_mn<-getStat((ps>0 & nulls)|(ps<0 & !nulls),nVals)
                ps1<-colMeans(ps)
                p_se<-sqrt(ps1*(1-ps1)/nrow(pVals))*(ps_mn/ps1)
                y50e<-ps_mn
                y25e<-ps_mn+p_se*qnorm(0.25)
                y38e<-ps_mn+p_se*qnorm(0.375)
                y62e<-ps_mn+p_se*qnorm(0.625)
                y75e<-ps_mn+p_se*qnorm(0.75)
                
                ps_mn<-getStat((ps<0 & nulls)|(ps>0 & !nulls) & !nulls,nVals)
                ps1<-colMeans(ps)
                p_se<-sqrt(ps1*(1-ps1)/nrow(pVals))*(ps_mn/ps1)
                y50a<-ps_mn
                y25a<-ps_mn+p_se*qnorm(0.25)
                y38a<-ps_mn+p_se*qnorm(0.375)
                y62a<-ps_mn+p_se*qnorm(0.625)
                y75a<-ps_mn+p_se*qnorm(0.75)
              }
            },
            "FDR"={
              if (explore$Explore_type=="Alpha") {
                alphaSig<-exploreResult$result$vals
              }
              
              sigs<-isSignificant(STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,alphaSig)
              nulls<-exploreResult$result$rpIVs==0
              if (STMethod=="NHST") {
                p1<-colSums(sigs & nulls)/colSums(sigs)
              } else {
                # d<-r2llr(rVals,nVals,df1Vals,STMethod,world=effect$world)
                p1<-(colSums(sigs & nulls & sigs>0)+colSums(sigs & !nulls & sigs<0))/max(colSums(abs(sigs)),1)
              }
              y50<-p1
              p_se<-sqrt(p1*(1-p1)/nrow(pVals))
              y75<-p1+p_se*qnorm(0.75)
              y25<-p1+p_se*qnorm(0.25)
              y62<-p1+p_se*qnorm(0.625)
              y38<-p1+p_se*qnorm(0.375)
              y50[is.na(y50)]<-0
              y50e<-c()
              
              col<-plotcolours$fdr
              colFill<-col
            },            
            "p(llrs)"={
              ns<-exploreResult$result$nvals
              df1<-exploreResult$result$df1
              if (explore$Explore_type=="Alpha") {
                alphaSig<-exploreResult$result$vals
              }
              p<-mean(isSignificant("sLLR",pvals,rvals,nvals,df1Vals,exploreResult$evidence,alphaSig),na.rm=TRUE)
              p_se<-sqrt(p*(1-p)/nrow(pVals))
              y50<-p
              y75<-p+p_se*qnorm(0.75)
              y25<-p+p_se*qnorm(0.25)
              y62<-p+p_se*qnorm(0.625)
              y38<-p+p_se*qnorm(0.375)
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            "p(llrd)"={
              ns<-exploreResult$result$nvals
              df1<-exploreResult$result$df1
              if (explore$Explore_type=="Alpha") {
                alphaSig<-exploreResult$result$vals
              }
              p<-mean(isSignificant("dLLR",pvals,rvals,nvals,df1Vals,exploreResult$evidence,alphaSig),na.rm=TRUE)
              p_se<-sqrt(p*(1-p)/nrow(pVals))
              y50<-p
              y75<-p+p_se*qnorm(0.75)
              y25<-p+p_se*qnorm(0.25)
              y62<-p+p_se*qnorm(0.625)
              y38<-p+p_se*qnorm(0.375)
              
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_typeShow]]
                colFill<-names(all_cols[explore$Explore_typeShow])
              }
            },
            
            
            
            "NHSTErrors"={
              if (explore$Explore_type=="Alpha") {
                alphaSig<-exploreResult$result$vals
              }
              if (!effect$world$worldOn) {
                pVals<-rbind(pVals,exploreResult$nullresult$pIVs)
                rVals<-rbind(rVals,exploreResult$nullresult$rIVs)
                nVals<-rbind(nVals,exploreResult$nullresult$nvals)
                df1Vals<-rbind(df1Vals,exploreResult$nullresult$df1)
                rpVals<-rbind(rpVals,exploreResult$nullresult$rpIVs)
              }
              sigs<-isSignificant(STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,alphaSig)
              nulls<-rpVals==0
              if (STMethod=="NHST") {
                d<-sigs+1
              } else {
                d<-r2llr(rVals,nVals,df1Vals,STMethod,world=effect$world)
              }
              sigNonNulls<- colSums( sigs & d>0 & !nulls)/nrow(pVals) 
              nsigNonNulls<-colSums(!sigs &       !nulls)/nrow(pVals) 
              isigNonNulls<-colSums( sigs & d<0 & !nulls)/nrow(pVals) 
              isigNulls<-   colSums( sigs & d<0 & nulls)/nrow(pVals) 
              sigNulls<-    colSums( sigs & d>0 & nulls)/nrow(pVals) 
              nsigNulls<-   colSums(!sigs &       nulls)/nrow(pVals) 

              lines<-c(0.05)
            },
            "FDR;FMR"={
              if (explore$Explore_type=="Alpha") {
                alphaSig<-exploreResult$result$vals
              }
              sigs<-isSignificant(STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,alphaSig)
              nulls<-rpVals==0
              
              if (STMethod=="NHST") {
                d<-sigs+1
              } else {
                d<-r2llr(rVals,nVals,df1Vals,STMethod,world=effect$world)
              }
              sumsig<-colSums(sigs)
              sumsig[sumsig==0]<-1
              sumnsig<-colSums(!sigs)
              sumnsig[sumnsig==0]<-1
              sigNonNulls<- 0 
              nsigNonNulls<-colSums(!sigs &       !nulls)/sumnsig 
              isigNonNulls<-colSums( sigs & d<0 & !nulls)/sumsig 
              isigNulls<-   0 
              sigNulls<-    colSums( sigs & d>0 & nulls)/sumsig
              nsigNulls<-   colSums(!sigs & abs(d)<alphaSig  &    nulls)/sumnsig

              lines<-c(0.05)
            },      
            "PDF"={
              showVals<-exploreResult$result$dists
              ySingle<-colMeans(showVals=="Single")
              yGauss<-colMeans(showVals=="Gauss")
              yExp<-colMeans(showVals=="Exp")
              
              p<-colMeans(showVals==effect$world$populationPDF,na.rm=TRUE)
              p_se<-sqrt(p*(1-p)/nrow(showVals))
              y50<-p
              y75<-p+p_se*qnorm(0.75)
              y25<-p+p_se*qnorm(0.25)
              y62<-p+p_se*qnorm(0.625)
              y38<-p+p_se*qnorm(0.375)
              
              lines<-c()
              col<-"white"
              colFill<-col
            }
    )
    
    vals_offset<-0
    valsRange<-1
    if (vals[1]<0) valsRange<-2
    if (multi=="allEffects" || multi=="mainEffects") {
      vals_offset<-(ni2-1)*(valsRange*valsGap)
    }
    xscale<-FALSE
    xmargin<-1
    
    if (exploreResult$Explore_type=="EffectSize") {
      if (RZ=="z") {
        vals<-atanh(vals)
        xLabel<-zpLabel
      } else {
        xLabel<-rpLabel
      }
    }
    
    # draw the basic line and point data
    if (is.element(explore$Explore_show,c("EffectSize","EffectSizeA","p","w","t","likelihood","SampleSize",
                                          "log(lrs)","log(lrd)",
                                          "k","S","pNull",
                                          "mean(DV)","sd(DV)","skew(DV)","kurtosis(DV)"))) {
      quants<-explore$Explore_quants/2
      y75<-apply( showVals , 2 , quantile , probs = 0.50+quants , na.rm = TRUE ,names<-FALSE)
      y62<-apply( showVals , 2 , quantile , probs = 0.50+quants/2 , na.rm = TRUE ,names<-FALSE)
      y50<-apply( showVals , 2 , quantile , probs = 0.50 , na.rm = TRUE ,names<-FALSE)
      y38<-apply( showVals , 2 , quantile , probs = 0.50-quants/2 , na.rm = TRUE ,names<-FALSE)
      y25<-apply( showVals , 2 , quantile , probs = 0.50-quants , na.rm = TRUE ,names<-FALSE)
      y75[y75>ylim[2]]<-ylim[2]
      y62[y62>ylim[2]]<-ylim[2]
      y38[y38>ylim[2]]<-ylim[2]
      y25[y25>ylim[2]]<-ylim[2]
      
      y75[y75<ylim[1]]<-ylim[1]
      y62[y62<ylim[1]]<-ylim[1]
      y38[y38<ylim[1]]<-ylim[1]
      y25[y25<ylim[1]]<-ylim[1]
      y50[y50<ylim[1]]<-NA
      
      pts1<-data.frame(vals=vals+vals_offset,y25=y25,y38=y38,y50=y50,y62=y62,y75=y75)
      if (doLine) {
        pts1f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y25,rev(y75)))
        pts2f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y38,rev(y62)))
        if (ni_max2==1 || !no_se_multiple) {
          g<-g+geom_polygon(data=pts1f,aes(x=x,y=y),fill=col,alpha=0.2)
          g<-g+geom_polygon(data=pts2f,aes(x=x,y=y),fill=col,alpha=0.4)
        }
        g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
      } else{
        switch(explore$Explore_show,
               "EffectSize"={expType<-"r"},
               "EffectSizeA"={expType<-"ra"},
               "p"={expType<-"p"},
               "t"={expType<-"t"},
               "w"={expType<-"w"},
               "SampleSize"={expType<-"n"},
               "log(lrs)"={expType<-"log(lrs)"},
               "log(lrd)"={expType<-"log(lrd)"},
               expType=NULL
               )
        if (is.element(explore$Explore_show,c("EffectSize","EffectSizeA","p","w","t","SampleSize"))){
          sigVals<-isSignificant(STMethod,pVals,rVals,nVals,df1Vals,exploreResult$evidence,alphaSig)
          col<-"white"
        } else {
          sigVals<-!is.na(showVals)
        }
        for (i in 1:length(vals))
          g<-expected_plot(g,
                           data.frame(x=vals[i]+vals_offset,y1=showVals[,i],y2=sigVals[,i]),
                           expType=expType,scale=2.25/(length(vals)+1),col=col)
        if (ni_max2==1 || !no_se_multiple){
          g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.35/length(vals)))
        }
      }
      if (use_col_names){
        pts1<-data.frame(x=vals+vals_offset,y=y50,fill=explore$Explore_typeShow)
        g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
      } else {
        g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=shapes$parameter, colour = "black",fill=col, size = markersize)
      }
    } # end of line and point

    # p(sig) and FDR
    if (is.element(explore$Explore_show,c("p(sig)","FDR"))) {
      if (isempty(y50e)) {
        pts1<-data.frame(vals=vals+vals_offset,y50=y50)
        if (doLine) {
          pts1f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y25,rev(y75)))
          pts2f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y38,rev(y62)))
          if (ni_max2==1 || !no_se_multiple) {
            g<-g+geom_polygon(data=pts1f,aes(x=x,y=y),fill=col,alpha=0.5)
            g<-g+geom_polygon(data=pts2f,aes(x=x,y=y),fill=col,alpha=0.45)
          }
          g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
        } else{
          if (ni_max2==1 || !no_se_multiple){
            g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.7/length(vals)))
          }
        }
        if (use_col_names){
          pts1<-data.frame(x=vals+vals_offset,y=y50,fill=explore$Explore_typeShow)
          g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
        } else {
          g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=shapes$parameter, fill=col,colour = "black",size = markersize)
        }
      } else {
        pts1<-data.frame(vals=vals+vals_offset,y50=y50,fill=col)
        if (doLine) {
          pts1f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y25,rev(y75)),fill=col)
          pts2f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y38,rev(y62)),fill=col)
          if (ni_max2==1 || !no_se_multiple) {
            g<-g+geom_polygon(data=pts1f,aes(x=x,y=y,fill=fill),alpha=0.5)
            g<-g+geom_polygon(data=pts2f,aes(x=x,y=y,fill=fill),alpha=0.45)
          }
          g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
        } else{
          if (ni_max2==1 || !no_se_multiple){
            g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.7/length(vals)))
          }
        }
        if (use_col_names){
          pts1<-data.frame(x=vals+vals_offset,y=y50,fill=explore$Explore_typeShow)
          g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
        } else {
          g<-g+geom_point(data=pts1,aes(x=vals,y=y50,fill=fill),shape=shapes$parameter, colour = "black",size = markersize)
        }
        if (!isempty(y50e)) {
          pts1<-data.frame(vals=vals+vals_offset,y50=y50e,fill=cole)
          if (doLine) {
            pts1f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y25e,rev(y75e)),fill=cole)
            pts2f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y38e,rev(y62e)),fill=cole)
            if (ni_max2==1 || !no_se_multiple) {
              g<-g+geom_polygon(data=pts1f,aes(x=x,y=y,fill=fill),alpha=0.5)
              g<-g+geom_polygon(data=pts2f,aes(x=x,y=y,fill=fill),alpha=0.45)
            }
            g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
          } else{
            if (ni_max2==1 || !no_se_multiple){
              g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.7/length(vals)))
            }
          }
          if (use_col_names){
            pts1<-data.frame(x=vals+vals_offset,y=y50e,fill=explore$Explore_typeShow)
            g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
          } else {
            g<-g+geom_point(data=pts1,aes(x=vals,y=y50,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
          }
        }
        if (!isempty(y50a)) {
          pts1<-data.frame(vals=vals+vals_offset,y50=y50a,fill=cola)
          if (doLine) {
            pts1f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y25a,rev(y75a)),fill=cola)
            pts2f<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y38a,rev(y62a)),fill=cola)
            if (ni_max2==1 || !no_se_multiple) {
              g<-g+geom_polygon(data=pts1f,aes(x=x,y=y,fill=fill),alpha=0.5)
              g<-g+geom_polygon(data=pts2f,aes(x=x,y=y,fill=fill),alpha=0.45)
            }
            g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
          } else{
            if (ni_max2==1 || !no_se_multiple){
              g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.7/length(vals)))
            }
          }
          if (use_col_names){
            pts1<-data.frame(x=vals+vals_offset,y=y50a,fill=explore$Explore_typeShow)
            g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
          } else {
            g<-g+geom_point(data=pts1,aes(x=vals,y=y50,fill=fill),shape=shapes$parameter, colour = "black", size = markersize)
          }
        }
        
      }
      g<-g+geom_hline(yintercept=0,colour="white")
    }
    
    # now the NHST and FDR filled areas
    if (explore$Explore_show=="FDR;FMR" || explore$Explore_show=="NHSTErrors") {
      endI<-length(vals)
      # if (!effect$world$worldOn) {
      #   nsigNonNulls<-nsigNonNulls*2
      #   sigNulls<-sigNulls*2
      #   nsigNulls<-1-nsigNonNulls-sigNulls
      #   sigNonNulls<-0
      #   isigNonNulls<-0
      #   isigNulls<-0
      # }
      
      if (explore$Explore_show=="NHSTErrors") {
        ytop<-1-nsigNonNulls*0
        yn<-0.5
        
        col0<-plotcolours$infer_isigNonNull
        lb0<-nonNullNegative
        switch (STMethod,
                "NHST"={col1<-plotcolours$infer_nsNonNull},
                "sLLR"={col1<-plotcolours$infer_nsNonNull},
                "dLLR"={col1<-plotcolours$infer_nsdNonNull})
        lb1<-nonNullNS
        col2<-plotcolours$infer_sigNonNull
        lb2<-nonNullPositive
        col3<-plotcolours$infer_isigNull
        lb3<-nullNegative
        switch (STMethod,
                "NHST"={col4<-plotcolours$infer_nsNull},
                "sLLR"={col4<-plotcolours$infer_nsNull},
                "dLLR"={col4<-plotcolours$infer_nsdNull})
        lb4<-nullNS
        col5<-plotcolours$infer_sigNull
        lb5<-nullPositive
        
        # error Z+
        if (any(isigNonNulls!=0)) {
          ybottom<-ytop-isigNonNulls
          ybottom[ybottom<0]<-0
          pts0<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(ybottom,rev(ytop)))
          lb0xy<-data.frame(x=max(vals),y=1-yn/10)
          yn<-yn+1
          ytop<-ybottom
        } else {pts0<-NULL}
        
        # false misses
        if (any(nsigNonNulls!=0)) {
          ybottom<-ytop-nsigNonNulls
          ybottom[ybottom<0]<-0
          pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(ybottom,rev(ytop)))
          lb1xy<-data.frame(x=max(vals),y=1-yn/10)
          yn<-yn+1
          ytop<-ybottom
        } else {pts1<-NULL}
        
        # true hits
        if (any(sigNonNulls!=0)) {
          ybottom<-ytop-sigNonNulls
          ybottom[ybottom<0]<-0
          pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(ybottom,rev(ytop)))
          lb2xy<-data.frame(x=max(vals),y=1-yn/10)
          yn<-yn+1
          ytop<-ybottom
        } else {pts2<-NULL}
        
        yn<-any(isigNulls!=0)+any(nsigNulls!=0)+any(sigNulls!=0)-0.5
        # false hits
        if (any(isigNulls!=0)) {
          ybottom<-ytop-isigNulls
          ybottom[ybottom<0]<-0
          pts3<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(ybottom,rev(ytop)))
          lb3xy<-data.frame(x=max(vals),y=0+yn/10)
          yn<-yn-1
          ytop<-ybottom
        } else {pts3<-NULL}
        
        # true misses
        if (any(nsigNulls!=0)) {
          ybottom<-ytop-nsigNulls
          ybottom[ybottom<0]<-0
          pts4<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(ybottom,rev(ytop)))
          lb4xy<-data.frame(x=max(vals),y=0+yn/10)
          yn<-yn-1
          ytop<-ybottom
        } else {pts4<-NULL}

        # error Z0
        if (any(sigNulls!=0)) {
          ybottom<-ytop-sigNulls
          ybottom[ybottom<0]<-0
          pts5<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(ybottom,rev(ytop)))
          lb5xy<-data.frame(x=max(vals),y=0+yn/10)
          yn<-yn-1
          ytop<-ybottom
        } else {pts5<-NULL}
        
        
      } 
      
      if (explore$Explore_show=="FDR;FMR") {
        pts0<-NULL
        col0<-plotcolours$fmr
        # false misses
        pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=1-c(nsigNonNulls,rep(0,endI)))
        col1<-plotcolours$fmr
        lb1<-nonNullNS
        lb1xy<-data.frame(x=max(vals),y=0.9)
        
        pts2<-NULL
        
        if (any(nsigNulls!=0)) {
          pts3<-data.frame(x=c(vals,rev(vals))+vals_offset,y=1-c(nsigNulls+nsigNonNulls,rev(nsigNonNulls)))
          col3<-plotcolours$infer_nsNUll
          lb3<-nullNS
          lb3xy<-data.frame(x=max(vals),y=0.8)
        } else {pts3<-NULL}
        
        # false hits
        pts5<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(isigNonNulls,rev(isigNonNulls+sigNulls)))
        col5<-plotcolours$fdr
        lb5<-nullPositive
        lb5xy<-data.frame(x=max(vals),y=0.2)
        
        if (any(isigNonNulls!=0)) {
          # non-null errors
          pts4<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(isigNonNulls,rep(0,endI)))
          col4<-plotcolours$infer_isigNonNull
          lb4<-nonNullNegative
          lb4xy<-data.frame(x=max(vals),y=0.1)
        } else {pts4<-NULL}
      }
      
      if (explore$Explore_graphStyle=="relevant") {
        NHSTasArea<-TRUE
        NHSThalfArea<-TRUE
        pts2<-NULL
        pts3<-NULL
        if (STMethod!="dLLR") pts4<-NULL
      } else {
        NHSTasArea<-TRUE
        NHSThalfArea<-FALSE
      }
        
      if (doLine) {
        if (NHSTasArea) {
          # type 2 errors
          if (!is.null(pts0)) g<-g+geom_polygon(data=pts0,aes(x=x,y=y),fill=col0)
          if (!is.null(pts1)) g<-g+geom_polygon(data=pts1,aes(x=x,y=y),fill=col1)
          if (!is.null(pts2)) g<-g+geom_polygon(data=pts2,aes(x=x,y=y),fill=col2)
          if (!is.null(pts3)) g<-g+geom_polygon(data=pts3,aes(x=x,y=y),fill=col3)
          if (!is.null(pts4)) g<-g+geom_polygon(data=pts4,aes(x=x,y=y),fill=col4)
          if (!is.null(pts5)) g<-g+geom_polygon(data=pts5,aes(x=x,y=y),fill=col5)
        } 
        if (!NHSTasArea) {
          if (!is.null(pts1)) {
            use<-1:(nrow(pts1)/2)
            g<-g+geom_line(data=pts1[use,],aes(x=x,y=y),colour=col1)
          }
          # if (!is.null(pts1)) g<-g+geom_point(data=pts1[use,],aes(x=x,y=y),fill=col1,shape=shapes$parameter,size = markersize)
          if (!is.null(pts5)) {
            use<-1:(nrow(pts5)/2)
            g<-g+geom_line(data=pts5[nrow(pts5)/2+use,],aes(x=x,y=y),colour=col5)
          }
          # if (!is.null(pts5)) g<-g+geom_point(data=pts5[nrow(pts5)/2+use,],aes(x=x,y=y),fill=col5,shape=shapes$parameter,size = markersize)
        }
      } else {
        npts<-length(vals)
        bwidth<-0.4*(pts1$x[2]-pts1$x[1])
        for (i in 1:npts) {
          if (NHSTasArea) {
              if (!is.null(pts0)) g<-g+drawNHSTBar(i,npts,pts0,bwidth,col0)
            if (!is.null(pts1)) g<-g+drawNHSTBar(i,npts,pts1,bwidth,col1)
              if (!is.null(pts2)) g<-g+drawNHSTBar(i,npts,pts2,bwidth,col2)
              if (!is.null(pts3)) g<-g+drawNHSTBar(i,npts,pts3,bwidth,col3)
            if (!is.null(pts4)) g<-g+drawNHSTBar(i,npts,pts4,bwidth,col4)
            if (!is.null(pts5)) g<-g+drawNHSTBar(i,npts,pts5,bwidth,col5)
          } else {
          if (!is.null(pts1)) g<-g+drawNHSTBar(i,npts,pts1,bwidth,col1)
          if (!is.null(pts5)) g<-g+drawNHSTBar(i,npts,pts5,bwidth,col5)
        }
        }
      }

      if (explore$Explore_show=="NHSTErrors") {
        if (effect$world$worldOn) {
          if (!NHSTasArea || NHSThalfArea) 
            g<-g+geom_hline(yintercept=effect$world$populationNullp,color="black")
        } else {
          g<-g+geom_hline(yintercept=0.5,color="black")
        }
      }
      g<-g+geom_hline(yintercept=1,color="black")
      g<-g+geom_hline(yintercept=0,color="black")
      
      if (doLine) xoff<-0
      else        xoff<-bwidth

      if (!is.null(pts0)) g<-g+drawNHSTLabel(lb0,lb0xy,xoff,col0)
      if (!is.null(pts1)) g<-g+drawNHSTLabel(lb1,lb1xy,xoff,col1)
      if (!is.null(pts2)) g<-g+drawNHSTLabel(lb2,lb2xy,xoff,col2)
      if (!is.null(pts3)) g<-g+drawNHSTLabel(lb3,lb3xy,xoff,col3)
      if (!is.null(pts4)) g<-g+drawNHSTLabel(lb4,lb4xy,xoff,col4)
      if (!is.null(pts5)) g<-g+drawNHSTLabel(lb5,lb5xy,xoff,col5)
      xmargin<-2
    }
    
    if (explore$Explore_show=="PDF") {
        y=rep(0,length(ySingle))
        switch(effect$world$populationPDF,
               "Single"={
                 pts<-data.frame(x=vals+vals_offset,y=ySingle)
                 pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y,rev(y+ySingle)))
                 pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y+ySingle,rev(y+ySingle+yExp)))
               },
               "Gauss"={
                 pts<-data.frame(x=vals+vals_offset,y=yGauss)
                 pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y,rev(y+yGauss)))
                 pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y+yGauss,rev(y+yGauss+yExp)))
               },
               "Exp"={
                 pts<-data.frame(x=vals+vals_offset,y=yExp)
                 pts1<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y,rev(y+yExp)))
                 pts2<-data.frame(x=c(vals,rev(vals))+vals_offset,y=c(y+yExp,rev(y+yExp+yGauss)))
               })
        g<-g+geom_line(data=pts,aes(x=x,y=y),color="black")
        g<-g+geom_point(data=pts,aes(x=x,y=y),shape=shapes$parameter,fill=plotcolours$sampleC,size = markersize)
      }
    }
    
    # effect size vs effect size line
    if (is.element(explore$Explore_show,c("EffectSize","Interaction")) && is.element(exploreResult$Explore_type,c("EffectSize","EffectSize1","EffectSize2","Interaction"))){
      pts3<-data.frame(x=c(-1,1),y=c(-1,1))
      g<-g+geom_line(data=pts3,aes(x=x,y=y),colour="yellow", linetype="dotted")
    }
    
    # find n80
    if (explore$Explore_show=="p(sig)" && exploreResult$Explore_type=="SampleSize" && effect$world$populationPDF=="Single"){
      w<-y50
      n<-exploreResult$result$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
      r_est<-r_est$minimum
      nvals<-seq(min(n),max(n),length.out=101)
      yvals<-rn2w(r_est,nvals)
      ptsn<-data.frame(x=nvals+vals_offset,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(10,explore$Explore_nRange),w=0.8,r=r_est)
      
      if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
        label<-paste("n80 =",format(n80$minimum,digits=2))
        # label<-paste("n80 =",format(n80$minimum,digits=2),"  r_est =", format(r_est,digits=3))
      } else {
        if (sum(n<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
        if (sum(n>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        # label<-paste("Unsafe result","  r_est =", format(r_est,digits=3))
      }
      if (ni_max2>1){label<-paste(explore$Explore_typeShow,": ",label,sep="")}
      lpts<-data.frame(x=min(n)+vals_offset,y=0.8+(ni_max2-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=labelSize)
    }
    
    # find r80
    if (explore$Explore_show=="p(sig)" && exploreResult$Explore_type=="EffectSize"){
      w<-y50
      r<-exploreResult$result$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n_est<-optimize(minrw,c(0,100),w=w,r=r)
      n_est<-n_est$minimum
      rvals<-seq(min(r),max(r),length.out=101)
      yvals<-rn2w(rvals,n_est)
      ptsn<-data.frame(x=rvals+vals_offset,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(0,0.8),w=0.8,n=n_est)
      
      if (sum(r<n80$minimum)>=2 && sum(r>n80$minimum)>=2){
        label<-paste("n80 =",format(n80$minimum,digits=2))
        # label<-paste("n80 =",format(n80$minimum,digits=2),"  n_est =", format(n_est,digits=3))
      } else {
        if (sum(r<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
        if (sum(r>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        # label<-paste("Unsafe result","  r_est =", format(r_est,digits=3))
      }
      if (ni_max2>1){label<-paste(explore$Explore_typeShow,": ",label,sep="")}
      lpts<-data.frame(x=0+vals_offset,y=0.8+(ni_max2-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=labelSize)
    }
  }

  if (multi=="allEffects" || multi=="mainEffects") {
    for (ni2 in 1:ni_max2) {
      switch (ni2,
              {explore$Explore_whichShow<-"Main 1"},
              {explore$Explore_whichShow<-"Main 2"},
              {explore$Explore_whichShow<-"Interaction"}
              )
      if (is.character(exploreResult$result$vals[1])) {
        vals_offset<-(ni2-1)*valsGap+0.5
      } else {
        vals_offset<-(ni2-1)*valsGap*2 
      }
      td<-data.frame(x=vals_offset,y=ylim[2]-diff(ylim)/6,label=explore$Explore_whichShow)
      g<-g+geom_label(data=td,aes(x=x, y=y, label=label,hjust=0.5),size=labelSize)
    }
    if (is.character(exploreResult$result$vals[1])) {
      g<-g+geom_vline(aes(xintercept=valsGap*c(1,ni_max2-1)-0.5*(valsGap-1)))
    } else {
      g<-g+geom_vline(aes(xintercept=valsGap*c(1,ni_max2)))
    }
    if (min(vals)<0) {
      tk<-(-2:2)/2
      jk<-2*valsGap
    } else {
      tk<-(0:4)/4
      jk<-valsGap
    }
    if (is.character(exploreResult$result$vals[1])) {
      tk<-seq(0,1,length.out=length(vals))
      g<-g+scale_x_continuous(breaks=c(vals,vals+jk,vals+jk*2),labels=c(exploreResult$result$vals,exploreResult$result$vals,exploreResult$result$vals),limits=c(0,1+jk*2)+c(-1,1)*0.25) +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    } else {
      g<-g+scale_x_continuous(breaks=c(tk,tk+jk,tk+jk*2),labels=c(tk,tk,tk),limits=c(tk[1],1+jk*2)+c(-1,1)*0.25)
    }
    xscale<-TRUE
  } 
  
  if ((is.element(exploreResult$Explore_type,c("SampleSize","Repeats","CheatingAmount")) &&
                 explore$Explore_xlog) 
      || ((exploreResult$Explore_type=="Alpha") && explore$Explore_xlog)
      || ((exploreResult$Explore_type=="NoStudies") && explore$Explore_Mxlog)) {
    dx<-(log10(max(vals))-log10(min(vals)))/15
    g<-g+scale_x_log10(limits=c(10^(log10(min(vals))-dx),10^(log10(max(vals))+dx*xmargin)))
    xscale<-TRUE
  }
  if (!xscale) {
      if (!doLine) {
        dx<-(vals[2]-vals[1])/1.5
        g<-g+scale_x_continuous(limits=c(min(vals)-dx,max(vals)+dx*xmargin),breaks=vals,labels=exploreResult$result$vals)
      } else {
        dx<-(max(vals)-min(vals))/15
        g<-g+scale_x_continuous(limits=c(min(vals)-dx,max(vals)+dx*xmargin))
      }
    }

  if (explore$Explore_ylog) {
    ysc<-scale_y_log10
  } else {
    ysc<-scale_y_continuous
  }
  if (explore$ExploreFull_ylim){
    ylim<-ylim*1.05
  }
  
  if (explore$Explore_show=="NHSTErrors" && !effect$world$worldOn) {
    g<-g+scale_y_continuous(breaks=seq(0,1,0.125),labels=format(c(seq(0,0.75,0.25),seq(1,0,-0.25))),limits=c(0,1))
  } else {
    if (!explore$ExploreAny_ylim) {
      if (!is.null(secondY)) {
        g<-g+ysc(sec.axis=sec_axis(~ 1-.,name=secondY))
      } else {
        g<-g+ysc()
      }
    } else {
      if (!is.null(secondY)) {
        g<-g+ysc(limits=ylim,sec.axis=sec_axis(~ 1-.,name=secondY))
      } else {
        g<-g+ysc(limits=ylim)
      }
    }
  }

  g<-g+ylab(ylabel)
  switch (exploreResult$Explore_type,
          "EffectSize"={g<-g+xlab(xLabel)},
          "EffectSize1"={g<-g+xlab(bquote(MainEffect1:r[p]))},
          "EffectSize2"={g<-g+xlab(bquote(MainEffect2:r[p]))},
          "Covariation"={g<-g+xlab(bquote(covariation:r[p]))},
          "Interaction"={g<-g+xlab(bquote(interaction:r[p]))},
          "pNull"={g<-g+xlab(Plabel)},
          "k"={g<-g+xlab(Llabel)},
          "Alpha"={g<-g+xlab(alphaChar)},
          g<-g+xlab(exploreResult$Explore_type)
  )
  alphaSig<<-oldAlpha

  g+plotTheme
}

