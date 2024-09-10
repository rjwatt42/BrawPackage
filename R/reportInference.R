#' report population estimates from a simulated sample
#' 
#' @param analysisType "Model", "Anova"
#' @return ggplot2 object - and printed
#' @examples
#' reportInference(analysis=doAnalysis())
#' @export
reportInference<-function(analysis=braw.res$result,analysisType="Anova"){
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
  nc<-ncol(anova)+1
  if (nc<5) nc<-5
  
  an_name<-analysis$an_name
    outputText<-rep("",nc*2)
    outputText[1]<-paste("\b",an_name,sep="")
    if (!is.null(IV2)) {
      outputText[2]<-paste("(",analysisType,"/",braw.env$modelType,")",sep="")
    }
    
    if (is.null(IV2)){
      pval<-analysis$pIV
      if (pval>=0.0001) {
        pvalText<-paste("p = ",brawFormat(pval,digits=braw.env$report_precision),sep="")
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

      outputText<-c(outputText,"!j\btest-statistic","\b(df) ","\bvalue   ","\bp",f1,rep("",nc-5))
      outputText<-c(outputText,paste0("!j",t_name),df,brawFormat(tval,digits=braw.env$report_precision),pvalText,f2,rep("",nc-5))
    }
    
    if (!(braw.env$reducedOutput && is.null(IV2))) {
      outputText<-c(outputText,rep("",nc))
      outputText<-c(outputText," ",paste0("!j",sub("^","\b",colnames(anova))))
      total_done<-FALSE
      
      for (i in 1:nrow(anova)){
        vn<-rownames(anova)[i]
        if (vn!="(Intercept)") {
          if (vn=="NULL") vn<-"Total"
          if (vn=="iv1"){vn<-paste("",analysis$hypothesis$IV$name,sep="")}
          if (vn=="iv2"){vn<-paste("",analysis$hypothesis$IV2$name,sep="")}
          if (vn=="iv1:iv2"){vn<-paste("",analysis$hypothesis$IV$name,":",analysis$hypothesis$IV2$name,sep="")}
          if (vn=="Residuals"){vn<-"Error"}
          if (vn=="Total"){
            vn<-"Total"
            total_done<-TRUE
          }
          
          outputText<-c(outputText,paste0("!j!i",vn,": "))
          for (j in 1:ncol(anova)){
            if (is.na(anova[i,j])){
              outputText<-c(outputText,"")
            } else {
              outputText<-c(outputText,paste0("!j",brawFormat(anova[i,j],digits=braw.env$report_precision)))
            }
          }
          if (ncol(anova)+1<nc) {outputText<-c(outputText,rep("",nc-(ncol(anova)+1)))}
        }
      }
      if (!total_done && analysisType=="Anova") {
        ssq<-sum(anova[,1])-anova[1,1]
        if (!is.na(ssq)) {ssq<-paste0("!j",brawFormat(ssq,digits=braw.env$report_precision))} else {ssq<-""}
        
        df<-sum(anova[,2])-anova[1,2]
        if (!is.na(df)) {df<-paste0("!j",brawFormat(df,digits=braw.env$report_precision))} else {df<-""}
        outputText<-c(outputText,"!j!iTotal: ",ssq,df,rep(" ",nc-3))
      }
      outputText<-c(outputText,rep("",nc))
    }
    
    if (!braw.env$reducedOutput) {
      outputText<-c(outputText,"\bPower(w)", "\bPost Hoc","\bActual",rep("",nc-3))   
      if (is.na(effect$rIV)) {effect$rIV<-0}
      outputText<-c(outputText," ",brawFormat(rn2w(analysis$rIV,analysis$nval),digits=3),
                    brawFormat(rn2w(effect$rIV,analysis$nval),digits=3),
                    rep("",nc-3))
    }
    
    nr=length(outputText)/nc

    reportPlot(outputText,nc,nr)
    
}
