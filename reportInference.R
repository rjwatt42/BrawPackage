reportInference<-function(result){
  IV<-result$IV
  IV2<-result$IV2
  DV<-result$DV
  effect<-result$effect
  
  switch (evidence$analysisType,
          "Anova"= {
            switch (evidence$dataType,
                    "Norm"={anova<-result$normAnova},
                    "Raw"={anova<-result$rawAnova},
                    "NormC"={anova<-result$normAnovaC},
                    "RawC"={anova<-result$rawAnovaC}
            )
          },
          "Model"= {
            switch (evidence$dataType,
                    "Norm"={anova<-result$normModel},
                    "Raw"={anova<-result$rawModel},
                    "NormC"={anova<-result$normModelC},
                    "RawC"={anova<-result$rawModelC}
            )
            anova<-data.frame(summary(anova)$coefficients)
          }
  )
  nc<-length(anova)+1
  if (nc<5) nc<-5
  
  an_name<-result$an_name
    outputText<-rep("",nc*2)
    outputText[1]<-paste("\b",an_name,sep="")
    if (!is.null(IV2)) {
      outputText[2]<-paste("(",evidence$analysisType,"/",evidence$dataType,")",sep="")
    }
    
    if (is.null(IV2)){
      pval<-result$pIV
      if (pval>=0.0001) {
        pvalText<-paste("p = ",format(pval,digits=report_precision),sep="")
      } else {
        pvalText<-"p < 0.0001"
      }
      
      t_name<-result$test_name
      df<-result$df
      tval<-result$test_val
      
      n<-result$nval
      f1<-" "
      f2<-" "
      if (STMethod=="sLLR") {
        result$sIV<-res2llr(result,"sLLR")
        f1<-"\bllr"
        f2<-paste("s=",format(result$sIV,digits=report_precision),sep="")
      }
      if (STMethod=="dLLR") {
        if (!result$evidence$prior$worldOn) {
          result$evidence$prior<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=result$rIV,populationRZ="r",populationNullp=0.5)
        }
        result$dIV<-res2llr(result,"dLLR")
        f1<-"\bllr"
        f2<-paste("d=",format(result$dIV,digits=report_precision),sep="")
      }

      outputText<-c(outputText,"\btest-statistic","\b(df) ","\bvalue   ","\bp",f1,rep("",nc-5))
      outputText<-c(outputText,t_name,df,format(tval,digits=report_precision),pvalText,f2,rep("",nc-5))
    }
    
    outputText<-c(outputText,rep(" ",nc))
    
    outputText<-c(outputText,paste0("\b",evidence$analysisType),sub("^","\b",colnames(anova)))
    total_done<-FALSE
    
    for (i in 1:nrow(anova)){
      vn<-rownames(anova)[i]
      if (vn!="(Intercept)") {
        if (vn=="NULL") vn<-"Total"
        if (vn=="iv1"){vn<-paste("",result$IVs$name,sep="")}
        if (vn=="iv2"){vn<-paste("",result$IV2s$name,sep="")}
        if (vn=="iv1:iv2"){vn<-paste("",result$IVs$name,":",result$IV2s$name,sep="")}
        if (vn=="Residuals"){vn<-"Error"}
        if (vn=="Total"){
          vn<-"\bTotal"
          total_done<-TRUE
          }
        
        outputText<-c(outputText,vn)
        for (j in 1:ncol(anova)){
          if (is.na(anova[i,j])){
            outputText<-c(outputText,"")
          } else {
            outputText<-c(outputText,format(anova[i,j],digits=report_precision))
          }
        }
        if (ncol(anova)+1<nc) {outputText<-c(outputText,rep("",nc-(ncol(anova)+1)))}
      }
    }
    if (!total_done && evidence$analysisType=="Anova") {
    ssq<-sum(anova[,1])-anova[1,1]
    if (!is.na(ssq)) {ssq<-format(ssq,digits=report_precision)} else {ssq<-""}
    
    df<-sum(anova[,2])-anova[1,2]
    if (!is.na(df)) {df<-format(df,digits=report_precision)} else {df<-""}
    outputText<-c(outputText,"\bTotal",ssq,df,rep("",nc-3))
    }
    outputText<-c(outputText,rep(" ",nc))

    outputText<-c(outputText,"\bPower(w)", "\bObserved","\bActual",rep("",nc-3))   
    if (is.na(effect$rIV)) {effect$rIV<-0}
    outputText<-c(outputText," ",format(rn2w(result$rIV,result$nval),digits=3),
                                 format(rn2w(effect$rIV,result$nval),digits=3),
                  rep("",nc-3))
    
    nr=length(outputText)/nc

    reportPlot(outputText,nc,nr)
    
}
