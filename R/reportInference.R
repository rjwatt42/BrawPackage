#' report population estimates from a simulated sample
#' 
#' @param analysisType "Model", "Anova"
#' @param modelType "Norm", "Raw"
#' @return ggplot2 object - and printed
#' @examples
#' reportInference(analysis=makeAnalysis())
#' @export
reportInference<-function(analysis=makeAnalysis(),modelType="Raw",analysisType="Anova"){
  IV<-analysis$hypothesis$IV
  IV2<-analysis$hypothesis$IV2
  DV<-analysis$hypothesis$DV
  effect<-analysis$hypothesis$effect
  evidence<-analysis$evidence
  
  switch (analysisType,
          "Anova"= {
            switch (modelType,
                    "Norm"={anova<-analysis$normAnova},
                    "Raw"={anova<-analysis$rawAnova},
                    "NormC"={anova<-analysis$normAnovaC},
                    "RawC"={anova<-analysis$rawAnovaC}
            )
          },
          "Model"= {
            switch (modelType,
                    "Norm"={anova<-analysis$normModel},
                    "Raw"={anova<-analysis$rawModel},
                    "NormC"={anova<-analysis$normModelC},
                    "RawC"={anova<-analysis$rawModelC}
            )
            anova<-data.frame(summary(anova)$coefficients)
          }
  )
  nc<-length(anova)+1
  if (nc<5) nc<-5
  
  an_name<-analysis$an_name
    outputText<-rep("",nc*2)
    outputText[1]<-paste("\b",an_name,sep="")
    if (!is.null(IV2)) {
      outputText[2]<-paste("(",analysisType,"/",modelType,")",sep="")
    }
    
    if (is.null(IV2)){
      pval<-analysis$pIV
      if (pval>=0.0001) {
        pvalText<-paste("p = ",format(pval,digits=braw.env$report_precision),sep="")
      } else {
        pvalText<-"p < 0.0001"
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
        f2<-paste("s=",format(analysis$sIV,digits=braw.env$report_precision),sep="")
      }
      if (braw.env$STMethod=="dLLR") {
        if (!analysis$evidence$prior$worldOn) {
          analysis$evidence$prior<-list(worldOn=TRUE,populationPDF="Single",populationPDFk=analysis$rIV,populationRZ="r",populationNullp=0.5)
        }
        analysis$dIV<-res2llr(analysis,"dLLR")
        f1<-"\bllr"
        f2<-paste("d=",format(analysis$dIV,digits=braw.env$report_precision),sep="")
      }

      outputText<-c(outputText,"\btest-statistic","\b(df) ","\bvalue   ","\bp",f1,rep("",nc-5))
      outputText<-c(outputText,t_name,df,format(tval,digits=braw.env$report_precision),pvalText,f2,rep("",nc-5))
    }
    
    outputText<-c(outputText,rep(" ",nc))
    
    outputText<-c(outputText,paste0("\b",analysisType),sub("^","\b",colnames(anova)))
    total_done<-FALSE
    
    for (i in 1:nrow(anova)){
      vn<-rownames(anova)[i]
      if (vn!="(Intercept)") {
        if (vn=="NULL") vn<-"Total"
        if (vn=="iv1"){vn<-paste("",analysis$hypothesis$IVs$name,sep="")}
        if (vn=="iv2"){vn<-paste("",analysis$hypothesis$IV2s$name,sep="")}
        if (vn=="iv1:iv2"){vn<-paste("",analysis$hypothesis$IVs$name,":",analysis$hypothesis$IV2s$name,sep="")}
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
            outputText<-c(outputText,format(anova[i,j],digits=braw.env$report_precision))
          }
        }
        if (ncol(anova)+1<nc) {outputText<-c(outputText,rep("",nc-(ncol(anova)+1)))}
      }
    }
    if (!total_done && analysisType=="Anova") {
    ssq<-sum(anova[,1])-anova[1,1]
    if (!is.na(ssq)) {ssq<-format(ssq,digits=braw.env$report_precision)} else {ssq<-""}
    
    df<-sum(anova[,2])-anova[1,2]
    if (!is.na(df)) {df<-format(df,digits=braw.env$report_precision)} else {df<-""}
    outputText<-c(outputText,"\bTotal",ssq,df,rep("",nc-3))
    }
    outputText<-c(outputText,rep(" ",nc))

    outputText<-c(outputText,"\bPower(w)", "\bObserved","\bActual",rep("",nc-3))   
    if (is.na(effect$rIV)) {effect$rIV<-0}
    outputText<-c(outputText," ",format(rn2w(analysis$rIV,analysis$nval),digits=3),
                                 format(rn2w(effect$rIV,analysis$nval),digits=3),
                  rep("",nc-3))
    
    nr=length(outputText)/nc

    reportPlot(outputText,nc,nr)
    
}
