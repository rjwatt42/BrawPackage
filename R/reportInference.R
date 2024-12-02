#' report population estimates from a simulated sample
#' 
#' @param analysisType "Model", "Anova"
#' @return ggplot2 object - and printed
#' @examples
#' reportInference(analysis=doAnalysis())
#' @export
reportInference<-function(analysis=braw.res$result,analysisType="Anova",showPowerN=TRUE){
  if (is.null(analysis)) analysis<-doResult(autoShow=FALSE)
  
  IV<-analysis$hypothesis$IV
  IV2<-analysis$hypothesis$IV2
  DV<-analysis$hypothesis$DV
  effect<-analysis$hypothesis$effect
  evidence<-analysis$evidence
  
  switch (analysisType,
          "Anova"= {anova<-analysis$anova},
          "Model"= {anova<-analysis$model}
  )
  nc<-ncol(anova)+2
  if (nc<7) nc<-7
  
  an_name<-analysis$an_name
    outputText<-rep(" ",nc)
    outputText[1]<-paste0("!T",an_name)
    if (!is.null(IV2)) {
      outputText[2]<-paste("(",analysisType,"/",braw.env$modelType,")",sep="")
    }
    outputText<-c(outputText,rep("",nc))
    
    if (is.null(IV2)){
      pval<-analysis$pIV
      if (pval>=10^(-braw.env$report_precision-1)) {
        pvalText<-paste("p = ",brawFormat(pval,digits=braw.env$report_precision+1),sep="")
      } else {
        pvalText<-paste0("p < ",10^(-braw.env$report_precision-1))
      }
      
      t_name<-analysis$test_name
      df<-analysis$df
      tval<-analysis$test_val
      
      n<-analysis$nval
      f1<-" "
      f2<-" "
      if (braw.env$STMethod=="sLLR") {
        analysis$sIV<-res2llr(analysis,"sLLR")
        f1<-"\bllr"
        f2<-paste("s=",brawFormat(analysis$sIV,digits=braw.env$report_precision),sep="")
      }
      if (braw.env$STMethod=="dLLR") {
        if (!analysis$evidence$prior$worldOn) {
          analysis$evidence$prior<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=analysis$rIV,populationRZ="r",populationNullp=0.5)
        }
        analysis$dIV<-res2llr(analysis,"dLLR")
        f1<-"\bllr"
        f2<-paste("d=",brawFormat(analysis$dIV,digits=braw.env$report_precision),sep="")
      }

      rvalText<-paste0(brawFormat(analysis$rIV,digits=braw.env$report_precision),
                       "\u00B1",brawFormat(r2se(analysis$rIV,analysis$nval),digits=braw.env$report_precision))

      if (IV$type=="Categorical" && IV$ncats==2 && DV$type=="Interval") {
        use1<-analysis$iv==IV$cases[1]
        use2<-analysis$iv==IV$cases[2]
        dval<-(mean(analysis$dv[use2],na.rm=TRUE)-mean(analysis$dv[use1],na.rm=TRUE))/
              sqrt(
                (
                  mean(use2,na.rm=TRUE)*sd(analysis$dv[use2],na.rm=TRUE)^2+
                    mean(use1,na.rm=TRUE)*sd(analysis$dv[use1],na.rm=TRUE)^2
                  )
                )
        outputText<-c(outputText,"!H!Ctest-statistic","(df) ","value","p",f1,"r[s]","Cohen's d",rep("",nc-7))
        outputText<-c(outputText,paste0("!c",t_name),df,brawFormat(tval,digits=braw.env$report_precision),pvalText,
                      f2,rvalText,brawFormat(dval,digits=braw.env$report_precision),rep("",nc-7))
      } else {
        outputText<-c(outputText,"!H!Ctest-statistic","(df) ","value","p",f1,"r[s]",rep("",nc-6))
        outputText<-c(outputText,paste0("!c",t_name),df,brawFormat(tval,digits=braw.env$report_precision),pvalText,
                      f2,rvalText,rep("",nc-6))
      }
    }
    
    if (!(braw.env$reducedOutput && is.null(IV2))) {
      outputText<-c(outputText,"!H!C ","r",paste0(sub("Pr\\(","p\\(",sub("^","",colnames(anova)))),"")
      total_done<-FALSE
      
      for (i in 1:nrow(anova)){
        vn<-rownames(anova)[i]
        if (analysisType=="Model") {
          vn<-gsub("(iv1)([^:].)(*)",paste0("\\1",braw.env$when_string,"\\2"),vn)
          vn<-gsub("(iv2)([^:].)(*)",paste0("\\1",braw.env$when_string,"\\2"),vn)
        }
        if (vn!="(Intercept)") {
          if (vn=="NULL") vn<-"Total"
          vn<-gsub("iv1",analysis$hypothesis$IV$name,vn)
          vn<-gsub("iv2",analysis$hypothesis$IV2$name,vn)
          vn<-gsub(":",braw.env$interaction_string,vn)
          # if (vn=="iv1"){vn<-paste("",analysis$hypothesis$IV$name,sep="")}
          # if (vn=="iv2"){vn<-paste("",analysis$hypothesis$IV2$name,sep="")}
          # if (vn=="iv1:iv2"){vn<-paste("",analysis$hypothesis$IV$name,":",analysis$hypothesis$IV2$name,sep="")}
          if (vn=="Residuals"){vn<-"Error"}
          if (vn=="Total"){
            vn<-"Total"
            total_done<-TRUE
          }
          
          outputText<-c(outputText,vn)
          if (i-1<=ncol(analysis$r$direct)) outputText<-c(outputText,brawFormat(analysis$r$direct[i-1],digits=braw.env$report_precision))
          else outputText<-c(outputText," ")
          for (j in 1:ncol(anova)){
            if (is.na(anova[i,j])){
              outputText<-c(outputText,"")
            } else {
              outputText<-c(outputText,paste0("!j",brawFormat(anova[i,j],digits=braw.env$report_precision)))
            }
          }
          outputText<-c(outputText,"")
        }
      }
      if (!total_done && analysisType=="Anova") {
        ssq<-sum(anova[,1])-anova[1,1]
        if (!is.na(ssq)) {ssq<-paste0("!j",brawFormat(ssq,digits=braw.env$report_precision))} else {ssq<-""}
        
        df<-sum(anova[,2])-anova[1,2]
        if (!is.na(df)) {df<-paste0("!j",brawFormat(df,digits=braw.env$report_precision))} else {df<-""}
        outputText<-c(outputText,"Total "," ",ssq,df,rep(" ",nc-4))
      }
    }
    
    AIC<-analysis$aic
    k<-nrow(anova)-2+2
    n_data<-analysis$nval
    llr<-(2*k-AIC)/2
    AICc=AIC+(2*k*k+2*k)/(n_data-k-1);
    BIC=AIC+k*log(n_data)-2*k;
    CAIC=k*(log(n_data)+1)+AIC-2*k;
    outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!HAIC","AICc","BIC","R^2","k","llr",rep("",nc-6))
    outputText<-c(outputText,
                  brawFormat(AIC,digits=1),
                  brawFormat(AICc,digits=1),
                  brawFormat(BIC,digits=1),
                  brawFormat(analysis$rFull^2,digits=braw.env$report_precision),
                  brawFormat(k),
                  brawFormat(llr,digits=1),
                  rep("",nc-6)
    )
    
    
    if (!braw.env$reducedOutput) {
    outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!HPower(w)", "Post Hoc","Actual","n80",rep("",nc-4))   
      if (is.na(effect$rIV)) {effect$rIV<-0}
      outputText<-c(outputText," ",brawFormat(rn2w(analysis$rIV,analysis$nval),digits=3),
                    brawFormat(rn2w(effect$rIV,analysis$nval),digits=3),
                    paste0("!j",brawFormat(ceil(rw2n(analysis$rIV,0.8,2)))),
                    rep("",nc-3))
    }
    
    nr=length(outputText)/nc

    reportPlot(outputText,nc,nr)
    
}
