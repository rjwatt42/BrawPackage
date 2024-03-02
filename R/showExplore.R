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
             hjust=-0.2,vjust=0.5,size=braw.env$labelSize,colour=col,fill=col1,parse=mathlabel)
}


trimExploreResult<-function(result) {
  
  use<- !is.na(result$rval[,1])
  nr=sum(use)
  nc=ncol(result$rval)
  result$rval=matrix(result$rval[use,],nrow=nr,ncol=nc)
  result$pval=matrix(result$pval[use,],nrow=nr,ncol=nc)
  result$rpval=matrix(result$rpval[use,],nrow=nr,ncol=nc)
  result$raval=matrix(result$raval[use,],nrow=nr,ncol=nc)
  result$roval=matrix(result$roval[use,],nrow=nr,ncol=nc)
  result$poval=matrix(result$poval[use,],nrow=nr,ncol=nc)
  result$nval=matrix(result$nval[use,],nrow=nr,ncol=nc)
  result$df1=matrix(result$df1[use,],nrow=nr,ncol=nc)
  
  return(result)
}

#' show the estimated population characteristics from varying parameter
#' 
#' @param showType        "r","p","n","w", "p(sig)" \cr
#' "NHSTErrors", "FDR","FDR;FMR"
#' @return ggplot2 object - and printed
#' @examples
#' showExplore(exploreResult=makeExplore(),
#'                        showType="r",
#'                        ylog=FALSE,
#'                        whichEffect="All",effectType="All")
#' @export
showExplore<-function(exploreResult=makeExplore(autoShow=TRUE),showType="r",ylog=FALSE,
                      whichEffect="All",effectType="All"){
  all_cols<-c()
  no_se_multiple<-TRUE
  valsGap<-1.4
  doBudget<-TRUE
  ErrorsWorld<-"1scale"
  
  explore<-exploreResult$explore
  hypothesis<-explore$hypothesis
  effect<-hypothesis$effect
  evidence<-explore$evidence
  
  result<-trimExploreResult(exploreResult$result)
  
  oldAlpha<-braw.env$alphaSig
  on.exit(braw.env$alphaSig<-oldAlpha)
  
  vals<-exploreResult$vals
  if (is.character(vals[1]) || is.element(explore$exploreType,c("IVcats","IVlevels","DVcats","DVlevels","Repeats","sig_only"))){
    if (is.character(vals[1]))  vals<-((1:length(vals))-1)/(length(vals)-1)
    doLine=FALSE
  } else {doLine=TRUE}
  
  if (explore$exploreType=="pNull" && braw.env$pPlus) vals<-1-vals
  
  g<-ggplot()
  ybreaks=c()
  ylabels=c()
  secondY<-NULL
  ylim<-c()
  switch (showType,
          "r"={
            ylim<-c(-1,1)
            ylabel<-bquote(bold(r['s']))
            if (braw.env$RZ=="z") {
              ylim<-c(-1,1)*braw.env$z_range
              ylabel<-bquote(bold(z['s']))
            }
          },
          "rA"={
            ylim<-c(-1,1)
            ylabel<-bquote(bold(r['a']))
            if (braw.env$RZ=="z") {
              ylim<-c(-1,1)*braw.env$z_range
              ylabel<-bquote(bold(z['a']))
            }
          },
          "p"={
            if (braw.env$pPlotScale=="log10") {
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
            if (braw.env$wPlotScale=="log10"){
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
            ylabel<-braw.env$pSigLabel
            if (ylog) {
              ylim<-c(0.001,1)
            } else {
              ylim<-c(0,1)
            }
          },
          "n(sig)"={
            doBudget<-TRUE
            ylabel<-bquote(bold(n[.('sig')]))
            showType<-"p(sig)"
            ylim<-c(0,100)
          },
          "FDR"={
            if (ylog) {
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
              g<-g+theme(axis.title.y.left = element_text(color=braw.env$plotColours$infer_sigNull),axis.title.y.right = element_text(color=braw.env$plotColours$infer_nsNonNull))
            }
          },
          "FDR;FMR"={
            ylim<-c(0,1)
            ylabel<-"False Discovery"
            secondY<-"False Miss"
              g<-g+theme(axis.title.y.left = element_text(color=braw.env$plotColours$fdr),axis.title.y.right = element_text(color=braw.env$plotColours$fmr))
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
          "n"={
            ylim<-c(braw.env$minN,braw.env$maxRandN*design$sN)
            ylabel<-"n"
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

  if (!is.null(hypothesis$IV2) && is.element(showType,c("r","rA","p","w","p(sig)"))) {
    switch (showType,
            "r"={use_cols<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))},
            "rA"={use_cols<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))},
            "p"=         {use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))},
            "w"=         {use_cols<-c(hsv(0.65,1,1),hsv(0.65+0.075,1,1),hsv(0.65+0.15,1,1))},
            "p(sig)"=    {use_cols<-c("#FFFFFF","#DDDDDD","#AAAAAA")},
    )
    names(use_cols)<-c("direct","unique","total")
    all_cols<-use_cols
    g<-g+scale_fill_manual(name=whichEffect,values=all_cols)
    use_col_names<-TRUE
  } else {
    all_cols<-c()
    use_col_names<-FALSE
  }
  
  markersize<-5
  ni_max1<-1
  ni_max2<-1
  multi="none"
  if (!is.null(hypothesis$IV2)) {
    if (effectType=="All") { # all of direct/unique/total
      markersize<-4
      ni_max1<-3
      multi<-"allTypes"
    } 
    if (whichEffect=="All") { # all of main1 main2 interaction
      markersize<-4
      ni_max2<-3
      multi<-"allEffects"
    } else {
      if (whichEffect=="Mains") { 
        markersize<-6
        ni_max2<-2
        multi<-"mainEffects"
      }
    }
  }
  
  for (ni1 in 1:ni_max1){
    for (ni2 in 1:ni_max2){
      if (ni_max1>1) {
      switch (ni1,
            {effectType<-"direct"},
            {effectType<-"unique"},
            {effectType<-"total"})
      } 
      if (ni_max2>1) {
      switch (ni2,
              {whichEffect<-"Main 1"},
              {whichEffect<-"Main 2"},
              {whichEffect<-"rIVIV2DV"})
      }

    extra_y_label<-""
    if (is.null(hypothesis$IV2)){
      rVals<-result$rval
      pVals<-result$pval
    } else {
      switch (whichEffect,
              "Main 1"={
                rVals<-result$r[[effectType]][,,1]
                pVals<-result$p[[effectType]][,,1]
                extra_y_label<-paste("Main Effect 1:",effectType)
              },
              "Main 2"={
                rVals<-result$r[[effectType]][,,2]
                pVals<-result$p[[effectType]][,,2]
                extra_y_label<-paste("Main Effect 2:",effectType)
              },
              "rIVIV2DV"={
                rVals<-result$r[[effectType]][,,3]
                pVals<-result$p[[effectType]][,,3]
                extra_y_label<-paste("Interaction:",effectType)
              }
      )
    }
    rpVals<-result$rpval
    nVals<-result$nval
    df1Vals<-result$df1
    
    switch (showType,
            "r"={
              showVals<-rVals
              if (braw.env$RZ=="z") {showVals<-atanh(rVals)}
              if (is.null(hypothesis$IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
                if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIV))}
              } else {
                switch (whichEffect,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                          if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIV))}
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                          if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIV2))}
                        },
                        "rIVIV2DV"={
                          lines<-c(0,effect$rIVIV2DV)
                          if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIVIV2DV))}
                        }
                )
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "rA"={
              showVals<-raVals
              if (braw.env$RZ=="z") {showVals<-atanh(raVals)}
              if (is.null(hypothesis$IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
                if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIV))}
              } else {
                switch (explore$whichEffect,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                          if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIV))}
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                          if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIV2))}
                        },
                        "rIVIV2DV"={
                          lines<-c(0,effect$rIVIV2DV)
                          if (braw.env$RZ=="z") {lines<-c(0,atanh(effect$rIVIV2DV))}
                        }
                )
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "p"={
              showVals<-pVals
              lines<-c(0.05)
              if (braw.env$pPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(hypothesis$IV2)){
                col<-"red"
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "w"={
              showVals<-rn2w(rVals,result$nval)
              lines<-c(0.05,0.8)
              if (braw.env$wPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(hypothesis$IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "n"={
              showVals<-result$nval
              lines<-c(design$sN)
              if (is.null(hypothesis$IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "likelihood"={
              showVals<-result$likes
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "log(lrs)"={
              ns<-result$nval
              df1<-result$df1
              showVals<-r2llr(rVals,ns,df1,"sLLR",evidence$llr,evidence$prior)
              
              lines<-c()
              if (is.null(hypothesis$IV2)){
                col<-"yellow"
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "log(lrd)"={
              ns<-result$nval
              df1<-result$df1
              showVals<-r2llr(rVals,ns,df1,"dLLR",evidence$llr,evidence$prior)
              
              lines<-c()
              if (is.null(hypothesis$IV2)){
                col<-"yellow"
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "k"={
              showVals<-result$k
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "pNull"={
              showVals<-result$pnull
              if (braw.env$pPlus) showVals<-1-showVals
              
              lines<-c()
              col<-"white"
              colFill<-col
            },
            "S"={
              showVals<-result$S
              
              lines<-c()
              col<-"white"
              colFill<-col
            },

            "p(sig)"={
              if (explore$exploreType=="Alpha") {
                braw.env$alphaSig<-exploreResult$vals
              }
              if (doBudget) {
                getStat<-function(x,n) {colMeans(x)*max(n)/colMeans(n)}
              } else {
                getStat<-function(x,n) {colMeans(x)}
              }
              ps<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
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
              if (is.null(hypothesis$IV2)){
                col<-braw.env$plotColours$infer_sigC
                cole<-braw.env$plotColours$infer_sigNull
                cola<-braw.env$plotColours$infer_nsNonNull
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
              
              if (effect$world$worldOn && effect$world$populationNullp>0) {
                g<-g+scale_fill_manual(name="outcome",values=c(braw.env$plotColours$psig,braw.env$plotColours$infer_sigC,braw.env$plotColours$infer_sigNull))
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
              if (explore$exploreType=="Alpha") {
                braw.env$alphaSig<-exploreResult$vals
              }
              
              sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
              nulls<-result$rpval==0
              if (braw.env$STMethod=="NHST") {
                p1<-colSums(sigs & nulls)/colSums(sigs)
              } else {
                # d<-r2llr(rVals,nVals,df1Vals,braw.env$STMethod,world=effect$world)
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
              
              col<-braw.env$plotColours$fdr
              colFill<-col
            },            
            "p(llrs)"={
              ns<-result$nval
              df1<-result$df1
              if (explore$exploreType=="Alpha") {
                braw.env$alphaSig<-exploreResult$vals
              }
              p<-mean(isSignificant("sLLR",pvals,rvals,nvals,df1Vals,evidence,braw.env$alphaSig),na.rm=TRUE)
              p_se<-sqrt(p*(1-p)/nrow(pVals))
              y50<-p
              y75<-p+p_se*qnorm(0.75)
              y25<-p+p_se*qnorm(0.25)
              y62<-p+p_se*qnorm(0.625)
              y38<-p+p_se*qnorm(0.375)
              lines<-c(0.05,0.8)
              if (is.null(hypothesis$IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            "p(llrd)"={
              ns<-result$nval
              df1<-result$df1
              if (explore$exploreType=="Alpha") {
                braw.env$alphaSig<-exploreResult$vals
              }
              p<-mean(isSignificant("dLLR",pvals,rvals,nvals,df1Vals,evidence,braw.env$alphaSig),na.rm=TRUE)
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
                col<-all_cols[[effectType]]
                colFill<-names(all_cols[effectType])
              }
            },
            
            
            
            "NHSTErrors"={
              if (explore$exploreType=="Alpha") {
                braw.env$alphaSig<-exploreResult$vals
              }
              if (!is.null(exploreResult$nullresult)) {
                pVals<-rbind(pVals,exploreResult$nullresult$pval)
                rVals<-rbind(rVals,exploreResult$nullresult$rval)
                nVals<-rbind(nVals,exploreResult$nullresult$nval)
                df1Vals<-rbind(df1Vals,exploreResult$nullresult$df1)
                rpVals<-rbind(rpVals,exploreResult$nullresult$rpval)
              }
              sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
              nulls<-rpVals==0
              if (braw.env$STMethod=="NHST") {
                d<-sigs+1
              } else {
                d<-r2llr(rVals,nVals,df1Vals,braw.env$STMethod,world=effect$world)
              }
              np<-sum(!is.na(pVals[,1]))
              sigNonNulls<- colSums( sigs & d>0 & !nulls,na.rm=TRUE)/np 
              nsigNonNulls<-colSums(!sigs &       !nulls,na.rm=TRUE)/np
              isigNonNulls<-colSums( sigs & d<0 & !nulls,na.rm=TRUE)/np 
              isigNulls<-   colSums( sigs & d<0 & nulls,na.rm=TRUE)/np 
              sigNulls<-    colSums( sigs & d>0 & nulls,na.rm=TRUE)/np 
              nsigNulls<-   colSums(!sigs &       nulls,na.rm=TRUE)/np 

              lines<-c(0.05)
            },
            "FDR;FMR"={
              if (explore$exploreType=="Alpha") {
                braw.env$alphaSig<-exploreResult$vals
              }
              sigs<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
              nulls<-rpVals==0
              
              if (braw.env$STMethod=="NHST") {
                d<-sigs+1
              } else {
                d<-r2llr(rVals,nVals,df1Vals,braw.env$STMethod,world=effect$world)
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
              nsigNulls<-   colSums(!sigs & abs(d)<braw.env$alphaSig  &    nulls)/sumnsig

              lines<-c(0.05)
            },      
            "PDF"={
              showVals<-result$dists
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
    
    valsOffset<-0
    valsRange<-1
    vals<-exploreResult$vals
    if (!doLine)  vals<-1:length(vals)
    if (vals[1]<0) valsRange<-2
    if (multi=="allEffects" || multi=="mainEffects") {
      valsOffset<-(ni2-1)*(valsRange*valsGap)
    }
    xscale<-FALSE
    xmargin<-1
    
    if (explore$exploreType=="rIV") {
      if (braw.env$RZ=="z") {
        vals<-atanh(vals)
        xLabel<-braw.env$zpLabel
      } else {
        xLabel<-braw.env$rpLabel
      }
    }
    
    # draw the basic line and point data
    if (is.element(showType,c("r","rA","p","w","likelihood","n",
                                          "log(lrs)","log(lrd)",
                                          "k","S","pNull",
                                          "mean(DV)","sd(DV)","skew(DV)","kurtosis(DV)"))) {
      quants<-0.25
      y75<-apply( showVals , 2 , quantile , probs = 0.50+quants , na.rm = TRUE ,names=FALSE)
      y62<-apply( showVals , 2 , quantile , probs = 0.50+quants/2 , na.rm = TRUE ,names=FALSE)
      y50<-apply( showVals , 2 , quantile , probs = 0.50 , na.rm = TRUE ,names=FALSE)
      y38<-apply( showVals , 2 , quantile , probs = 0.50-quants/2 , na.rm = TRUE ,names=FALSE)
      y25<-apply( showVals , 2 , quantile , probs = 0.50-quants , na.rm = TRUE ,names=FALSE)
      y75[y75>ylim[2]]<-ylim[2]
      y62[y62>ylim[2]]<-ylim[2]
      y38[y38>ylim[2]]<-ylim[2]
      y25[y25>ylim[2]]<-ylim[2]
      
      y75[y75<ylim[1]]<-ylim[1]
      y62[y62<ylim[1]]<-ylim[1]
      y38[y38<ylim[1]]<-ylim[1]
      y25[y25<ylim[1]]<-ylim[1]
      y50[y50<ylim[1]]<-NA
      
      pts1<-data.frame(vals=vals+valsOffset,y25=y25,y38=y38,y50=y50,y62=y62,y75=y75)
      if (doLine) {
        pts1f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y25,rev(y75)))
        pts2f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y38,rev(y62)))
        if (ni_max2==1 || !no_se_multiple) {
          g<-g+geom_polygon(data=pts1f,aes(x=x,y=y),fill=col,alpha=0.2)
          g<-g+geom_polygon(data=pts2f,aes(x=x,y=y),fill=col,alpha=0.4)
        }
        g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
      } else{
        switch(showType,
               "r"={expType<-"r"},
               "rA"={expType<-"ra"},
               "p"={expType<-"p"},
               "w"={expType<-"w"},
               "n"={expType<-"n"},
               "log(lrs)"={expType<-"log(lrs)"},
               "log(lrd)"={expType<-"log(lrd)"},
               expType=NULL
               )
        if (is.element(showType,c("r","rA","p","w","n"))){
          sigVals<-isSignificant(braw.env$STMethod,pVals,rVals,nVals,df1Vals,evidence,braw.env$alphaSig)
          col<-"white"
        } else {
          sigVals<-!is.na(showVals)
        }
        for (i in 1:length(vals))
          g<-expected_plot(g,
                           data.frame(x=vals[i]+valsOffset,y1=showVals[,i],y2=sigVals[,i]),
                           expType=expType,scale=0.9,col=col)
        if (ni_max2==1 || !no_se_multiple){
          g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.35/length(vals)))
        }
      }
      if (use_col_names){
        pts1<-data.frame(x=vals+valsOffset,y=y50,fill=effectType)
        g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
      } else {
        g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=braw.env$plotShapes$parameter, colour = "black",fill=col, size = markersize)
      }
    } # end of line and point

    # p(sig) and FDR
    if (is.element(showType,c("p(sig)","FDR"))) {
      if (isempty(y50e)) {
        pts1<-data.frame(vals=vals+valsOffset,y50=y50)
        if (doLine) {
          pts1f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y25,rev(y75)))
          pts2f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y38,rev(y62)))
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
          pts1<-data.frame(x=vals+valsOffset,y=y50,fill=effectType)
          g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
        } else {
          g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=braw.env$plotShapes$parameter, fill=col,colour = "black",size = markersize)
        }
      } else {
        pts1<-data.frame(vals=vals+valsOffset,y50=y50,fill=col)
        if (doLine) {
          pts1f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y25,rev(y75)),fill=col)
          pts2f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y38,rev(y62)),fill=col)
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
          pts1<-data.frame(x=vals+valsOffset,y=y50,fill=effectType)
          g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
        } else {
          g<-g+geom_point(data=pts1,aes(x=vals,y=y50,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black",size = markersize)
        }
        if (!isempty(y50e)) {
          pts1<-data.frame(vals=vals+valsOffset,y50=y50e,fill=cole)
          if (doLine) {
            pts1f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y25e,rev(y75e)),fill=cole)
            pts2f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y38e,rev(y62e)),fill=cole)
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
            pts1<-data.frame(x=vals+valsOffset,y=y50e,fill=effectType)
            g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
          } else {
            g<-g+geom_point(data=pts1,aes(x=vals,y=y50,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
          }
        }
        if (!isempty(y50a)) {
          pts1<-data.frame(vals=vals+valsOffset,y50=y50a,fill=cola)
          if (doLine) {
            pts1f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y25a,rev(y75a)),fill=cola)
            pts2f<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y38a,rev(y62a)),fill=cola)
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
            pts1<-data.frame(x=vals+valsOffset,y=y50a,fill=effectType)
            g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
          } else {
            g<-g+geom_point(data=pts1,aes(x=vals,y=y50,fill=fill),shape=braw.env$plotShapes$parameter, colour = "black", size = markersize)
          }
        }
        
      }
      g<-g+geom_hline(yintercept=0,colour="white")
    }
    
    # now the NHST and FDR filled areas
    if (showType=="FDR;FMR" || showType=="NHSTErrors") {
      endI<-length(vals)
      # if (!effect$world$worldOn) {
      #   nsigNonNulls<-nsigNonNulls*2
      #   sigNulls<-sigNulls*2
      #   nsigNulls<-1-nsigNonNulls-sigNulls
      #   sigNonNulls<-0
      #   isigNonNulls<-0
      #   isigNulls<-0
      # }
      
      if (showType=="NHSTErrors") {
        ytop<-1-nsigNonNulls*0
        yn<-0.5
        
        col0<-braw.env$plotColours$infer_isigNonNull
        lb0<-braw.env$nonNullNegative
        switch (braw.env$STMethod,
                "NHST"={col1<-braw.env$plotColours$infer_nsNonNull},
                "sLLR"={col1<-braw.env$plotColours$infer_nsNonNull},
                "dLLR"={col1<-braw.env$plotColours$infer_nsdNonNull})
        lb1<-braw.env$nonNullNS
        col2<-braw.env$plotColours$infer_sigNonNull
        lb2<-braw.env$nonNullPositive
        col3<-braw.env$plotColours$infer_isigNull
        lb3<-braw.env$nullNegative
        switch (braw.env$STMethod,
                "NHST"={col4<-braw.env$plotColours$infer_nsNull},
                "sLLR"={col4<-braw.env$plotColours$infer_nsNull},
                "dLLR"={col4<-braw.env$plotColours$infer_nsdNull})
        lb4<-braw.env$nullNS
        col5<-braw.env$plotColours$infer_sigNull
        lb5<-braw.env$nullPositive
        
        # error Z+
        if (any(isigNonNulls!=0)) {
          ybottom<-ytop-isigNonNulls
          ybottom[ybottom<0]<-0
          pts0<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(ybottom,rev(ytop)))
          lb0xy<-data.frame(x=max(vals),y=1-yn/10)
          yn<-yn+1
          ytop<-ybottom
        } else {pts0<-NULL}
        
        # false misses
        if (any(nsigNonNulls!=0)) {
          ybottom<-ytop-nsigNonNulls
          ybottom[ybottom<0]<-0
          pts1<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(ybottom,rev(ytop)))
          lb1xy<-data.frame(x=max(vals),y=1-yn/10)
          yn<-yn+1
          ytop<-ybottom
        } else {pts1<-NULL}
        
        # true hits
        if (any(sigNonNulls!=0)) {
          ybottom<-ytop-sigNonNulls
          ybottom[ybottom<0]<-0
          pts2<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(ybottom,rev(ytop)))
          lb2xy<-data.frame(x=max(vals),y=1-yn/10)
          yn<-yn+1
          ytop<-ybottom
        } else {pts2<-NULL}
        
        yn<-any(isigNulls!=0)+any(nsigNulls!=0)+any(sigNulls!=0)-0.5
        # false hits
        if (any(isigNulls!=0)) {
          ybottom<-ytop-isigNulls
          ybottom[ybottom<0]<-0
          pts3<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(ybottom,rev(ytop)))
          lb3xy<-data.frame(x=max(vals),y=0+yn/10)
          yn<-yn-1
          ytop<-ybottom
        } else {pts3<-NULL}
        
        # true misses
        if (any(nsigNulls!=0)) {
          ybottom<-ytop-nsigNulls
          ybottom[ybottom<0]<-0
          pts4<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(ybottom,rev(ytop)))
          lb4xy<-data.frame(x=max(vals),y=0+yn/10)
          yn<-yn-1
          ytop<-ybottom
        } else {pts4<-NULL}

        # error Z0
        if (any(sigNulls!=0)) {
          ybottom<-ytop-sigNulls
          ybottom[ybottom<0]<-0
          pts5<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(ybottom,rev(ytop)))
          lb5xy<-data.frame(x=max(vals),y=0+yn/10)
          yn<-yn-1
          ytop<-ybottom
        } else {pts5<-NULL}
        
        
      } 
      
      if (showType=="FDR;FMR") {
        pts0<-NULL
        col0<-braw.env$plotColours$fmr
        # false misses
        pts1<-data.frame(x=c(vals,rev(vals))+valsOffset,y=1-c(nsigNonNulls,rep(0,endI)))
        col1<-braw.env$plotColours$fmr
        lb1<-braw.env$nonNullNS
        lb1xy<-data.frame(x=max(vals),y=0.9)
        
        pts2<-NULL
        
        if (any(nsigNulls!=0)) {
          pts3<-data.frame(x=c(vals,rev(vals))+valsOffset,y=1-c(nsigNulls+nsigNonNulls,rev(nsigNonNulls)))
          col3<-braw.env$plotColours$infer_nsNUll
          lb3<-braw.env$nullNS
          lb3xy<-data.frame(x=max(vals),y=0.8)
        } else {pts3<-NULL}
        
        # false hits
        pts5<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(isigNonNulls,rev(isigNonNulls+sigNulls)))
        col5<-braw.env$plotColours$fdr
        lb5<-braw.env$nullPositive
        lb5xy<-data.frame(x=max(vals),y=0.2)
        
        if (any(isigNonNulls!=0)) {
          # non-null errors
          pts4<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(isigNonNulls,rep(0,endI)))
          col4<-braw.env$plotColours$infer_isigNonNull
          lb4<-braw.env$nonNullNegative
          lb4xy<-data.frame(x=max(vals),y=0.1)
        } else {pts4<-NULL}
      }
      
      # if (explore$Explore_graphStyle=="relevant") {
      if (1==1) {
        NHSTasArea<-TRUE
        NHSThalfArea<-TRUE
        pts2<-NULL
        pts3<-NULL
        if (braw.env$STMethod!="dLLR") pts4<-NULL
      } else {
        NHSTasArea<-TRUE
        NHSThalfArea<-FALSE
      }
        
      if (doLine) {
        if (NHSTasArea) {
          # exploreType 2 errors
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

      if (showType=="NHSTErrors") {
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
    
    if (showType=="PDF") {
        y=rep(0,length(ySingle))
        switch(effect$world$populationPDF,
               "Single"={
                 pts<-data.frame(x=vals+valsOffset,y=ySingle)
                 pts1<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y,rev(y+ySingle)))
                 pts2<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y+ySingle,rev(y+ySingle+yExp)))
               },
               "Gauss"={
                 pts<-data.frame(x=vals+valsOffset,y=yGauss)
                 pts1<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y,rev(y+yGauss)))
                 pts2<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y+yGauss,rev(y+yGauss+yExp)))
               },
               "Exp"={
                 pts<-data.frame(x=vals+valsOffset,y=yExp)
                 pts1<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y,rev(y+yExp)))
                 pts2<-data.frame(x=c(vals,rev(vals))+valsOffset,y=c(y+yExp,rev(y+yExp+yGauss)))
               })
        g<-g+geom_line(data=pts,aes(x=x,y=y),color="black")
        g<-g+geom_point(data=pts,aes(x=x,y=y),shape=braw.env$plotShapes$parameter,fill=braw.env$plotColours$sampleC,size = markersize)
      }
    }
    
    # effect size vs effect size line
    if (is.element(showType,c("r","rIVIV2DV")) && is.element(explore$exploreType,c("rIV","rIV2","rIVIV2DV"))){
      pts3<-data.frame(x=c(-1,1),y=c(-1,1))
      g<-g+geom_line(data=pts3,aes(x=x,y=y),colour="yellow", linetype="dotted")
    }
    
    # find n80
    if (showType=="p(sig)" && explore$exploreType=="n" && effect$world$populationPDF=="Single"){
      w<-y50
      n<-exploreResult$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
      r_est<-r_est$minimum
      nvals<-seq(min(n),max(n),length.out=101)
      yvals<-rn2w(r_est,nvals)
      ptsn<-data.frame(x=nvals+valsOffset,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(explore$min_n,explore$max_n),w=0.8,r=r_est)
      
      if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
        label<-paste("n80 =",format(round(n80$minimum),digits=2))
        # label<-paste("n80 =",format(n80$minimum,digits=2),"  r_est =", format(r_est,digits=3))
      } else {
        if (sum(n<n80$minimum)<2) label<-paste("Unsafe result - decrease range")
        if (sum(n>n80$minimum)<2) label<-paste("Unsafe result - increase range")
        # label<-paste("Unsafe result","  r_est =", format(r_est,digits=3))
      }
      if (ni_max2>1){label<-paste(effectType,": ",label,sep="")}
      lpts<-data.frame(x=min(n)+valsOffset,y=0.8+(ni_max2-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=braw.env$labelSize)
    }
    
    # find r80
    if (showType=="p(sig)" && explore$exploreType=="rIV"){
      w<-y50
      r<-exploreResult$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n_est<-optimize(minrw,c(0,100),w=w,r=r)
      n_est<-n_est$minimum
      rvals<-seq(min(r),max(r),length.out=101)
      yvals<-rn2w(rvals,n_est)
      ptsn<-data.frame(x=rvals+valsOffset,y=yvals)
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
      if (ni_max2>1){label<-paste(effectType,": ",label,sep="")}
      lpts<-data.frame(x=0+valsOffset,y=0.8+(ni_max2-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=braw.env$labelSize)
    }
  }

  if (multi=="allEffects" || multi=="mainEffects") {
    for (ni2 in 1:ni_max2) {
      switch (ni2,
              {explore$whichEffect<-"Main 1"},
              {explore$whichEffect<-"Main 2"},
              {explore$whichEffect<-"rIVIV2DV"}
              )
      if (is.character(exploreResult$vals[1])) {
        valsOffset<-(ni2-1)*valsGap+0.5
      } else {
        valsOffset<-(ni2-1)*valsGap*2 
      }
      td<-data.frame(x=valsOffset,y=ylim[2]-diff(ylim)/6,label=explore$whichEffect)
      g<-g+geom_label(data=td,aes(x=x, y=y, label=label,hjust=0.5),size=braw.env$labelSize)
    }
    if (is.character(exploreResult$vals[1])) {
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
    if (is.character(exploreResult$vals[1])) {
      tk<-seq(0,1,length.out=length(vals))
      g<-g+scale_x_continuous(breaks=c(vals,vals+jk,vals+jk*2),labels=c(exploreResult$vals,exploreResult$vals,exploreResult$vals),limits=c(0,1+jk*2)+c(-1,1)*0.25) +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    } else {
      g<-g+scale_x_continuous(breaks=c(tk,tk+jk,tk+jk*2),labels=c(tk,tk,tk),limits=c(tk[1],1+jk*2)+c(-1,1)*0.25)
    }
    xscale<-TRUE
  } 
  
  if ((is.element(explore$exploreType,c("n","Repeats","CheatingAmount")) &&
                 explore$xlog) 
      || ((explore$exploreType=="Alpha") && explore$xlog)
      || ((explore$exploreType=="NoStudies") && explore$mx_log)) {
    dx<-(log10(max(vals))-log10(min(vals)))/15
    g<-g+scale_x_log10(limits=c(10^(log10(min(vals))-dx),10^(log10(max(vals))+dx*xmargin)))
    xscale<-TRUE
  }
  if (!xscale) {
      if (!doLine) {
        dx<-(vals[2]-vals[1])/1.5
        g<-g+scale_x_continuous(limits=c(min(vals)-dx,max(vals)+dx*xmargin),breaks=vals,labels=exploreResult$vals)
      } else {
        dx<-(max(vals)-min(vals))/15
        g<-g+scale_x_continuous(limits=c(min(vals)-dx,max(vals)+dx*xmargin))
      }
    }

  if (ylog) {
    ysc<-scale_y_log10
  } else {
    ysc<-scale_y_continuous
  }

  if (showType=="NHSTErrors" && !effect$world$worldOn) {
    g<-g+scale_y_continuous(breaks=seq(0,1,0.125),labels=format(c(seq(0,0.75,0.25),seq(1,0,-0.25))),limits=c(0,1))
  } else {
    if (!is.null(secondY)) {
      g<-g+ysc(sec.axis=sec_axis(~ 1-.,name=secondY))
    } else {
      g<-g+ysc()
    }
  }

  g<-g+ylab(ylabel)
  switch (explore$exploreType,
          "rIV"={
            if (is.null(hypothesis$IV2))  g<-g+xlab(xLabel)
            else g<-g+xlab(bquote(MainEffect1:r[p]))
            },
          "rIV2"={g<-g+xlab(bquote(MainEffect2:r[p]))},
          "rIVIV2"={g<-g+xlab(bquote(Covariation:r[p]))},
          "rIVIV2DV"={g<-g+xlab(bquote(Interaction:r[p]))},
          "pNull"={g<-g+xlab(braw.env$Plabel)},
          "k"={g<-g+xlab(braw.env$Llabel)},
          "Alpha"={g<-g+xlab(braw.env$alphaChar)},
          g<-g+xlab(explore$exploreType)
  )
  g<-g+ggtitle(paste0("Explore: ",format(exploreResult$count)))
  g<-g+braw.env$plotTheme+theme(plot.title=element_text(face='plain', size=8, hjust=0.9))
  g
}

