#' report population estimates from a simulated sample
#' 
#' @param analysisType "Model", "Anova"
#' @return ggplot2 object - and printed
#' @examples
#' reportInference(analysis=doAnalysis())
#' @export
reportInference<-function(analysis=braw.res$result,analysisType="Anova",showPowerN=TRUE){
  if (is.null(analysis)) analysis<-doSingle(autoShow=FALSE)
  
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
  if (evidence$doSEM) nc<-20
  if (nc<8) nc<-8
  
  an_name<-analysis$an_name
    outputText<-rep(" ",nc)
    outputText[1]<-paste0("!T",an_name)
    if (!is.null(IV2)) {
      outputText[2]<-paste("(",analysisType,"/",braw.env$modelType,")",sep="")
    }
    outputText<-c(outputText,rep("",nc))
    
    for (i in 1:1) {
      pval<-analysis$pIV
      df<-analysis$df
      nval<-analysis$nval
      rval<-analysis$rIV
      
      if (is.null(IV2)){
        if (pval>=10^(-braw.env$report_precision-1)) {
          pvalText<-paste("p = ",brawFormat(pval,digits=braw.env$report_precision+1),sep="")
        } else {
          pvalText<-paste0("p < ",10^(-braw.env$report_precision-1))
        }
        
      t_name<-analysis$test_name
      if (!is.character(df)) df<-paste0("(",brawFormat(df),")")
      tval<-analysis$test_val
      
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

      rvalText<-paste0(brawFormat(rval,digits=braw.env$report_precision),
                       "\u00B1",brawFormat(r2se(rval,nval),digits=braw.env$report_precision))

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
        outputText<-c(outputText,"!Htest-statistic","(df) ","value","p",f1,"r[s]","Cohen's d",rep("",nc-7))
        outputText<-c(outputText,paste0("!j",t_name),df,
                                 brawFormat(tval,digits=braw.env$report_precision),pvalText,
                      f2,rvalText,brawFormat(dval,digits=braw.env$report_precision),rep("",nc-7))
      } else {
        outputText<-c(outputText,"!Htest-statistic","(df) ","value","p",f1,"r[s]",rep("",nc-6))
        outputText<-c(outputText,paste0("!j",t_name),df,
                                 brawFormat(tval,digits=braw.env$report_precision),pvalText,
                      f2,rvalText,rep("",nc-6))
      }
    }
    }
    if (!is.null(IV2)) {
      nc1<-length(colnames(anova))+1
      outputText<-c(outputText,"!H!C ","r",paste0(sub("Pr\\(","p\\(",sub("^","",colnames(anova)))),rep("",nc-1-nc1))
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
          outputText<-c(outputText,rep("",nc-1-nc1))
        }
      }
      if (!total_done && analysisType=="Anova") {
        ssq<-sum(anova[,1])-anova[1,1]
        if (!is.na(ssq)) {ssq<-paste0("!j",brawFormat(ssq,digits=braw.env$report_precision))} else {ssq<-""}
        
        df<-sum(anova[,2])-anova[1,2]
        if (!is.na(df)) {df<-paste0("!j",brawFormat(df,digits=braw.env$report_precision))} else {df<-""}
        outputText<-c(outputText,"Total "," ",ssq,df,rep(" ",nc-4))
      }
      outputText<-c(outputText,rep("",nc))
      outputText<-c(outputText,paste0("Full model:"),paste0("r=",brawFormat(analysis$rFull)),paste0("p=",brawFormat(analysis$pFull)),rep("",nc-3))
      outputText<-c(outputText,rep("",nc))
    }
    
    if (braw.env$fullOutput>1) {
    AIC<-analysis$AIC
    llkNull<-exp(-0.5*(analysis$AIC-analysis$AICnull))
    k<-nrow(anova)-2+2
    n_data<-analysis$nval
    llr<-(2*k-AIC)/2
    AICc=AIC+(2*k*k+2*k)/(n_data-k-1);
    BIC=AIC+k*log(n_data)-2*k;
    CAIC=k*(log(n_data)+1)+AIC-2*k;
    outputText<-c(outputText,rep("",nc))
      outputText<-c(outputText,"!HAIC","AICc","BIC","AICnull","llr[+]","R^2","k","llr",rep("",nc-8))
      outputText<-c(outputText,
                    brawFormat(analysis$AIC,digits=1),
                    brawFormat(AICc,digits=1),
                    brawFormat(BIC,digits=1),
                    brawFormat(analysis$AICnull,digits=1),
                    brawFormat(log(llkNull),digits=3),
                    brawFormat(analysis$rFull^2,digits=braw.env$report_precision),
                    brawFormat(k),
                    brawFormat(llr,digits=1),
                    rep("",nc-8)
      )
    }
    
    
    if (braw.env$fullOutput>0) {
    outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!H","r[s]","n","p", "r[p]", "w[p]",rep("",nc-6))   
      if (is.na(effect$rIV)) {effect$rIV<-0}
      if (analysis$design$Replication$On) {
        outputText<-c(outputText,
                      "original",
                      paste0("!j",brawFormat(analysis$roIV,digits=3)),
                      paste0("!j",brawFormat(analysis$noval)),
                      paste0("!j",brawFormat(analysis$poIV,digits=3)),
                      paste0("!j",brawFormat(analysis$rpIV,digits=3)),
                      paste0("!j",brawFormat(rn2w(analysis$rpIV,analysis$noval),digits=3)),
                      rep("",nc-6)
        )
        if (analysis$design$Replication$Keep=="MetaAnalysis")
          outputText<-c(outputText,
                        "replication",
                        paste0("!j",brawFormat(analysis$ResultHistory$rIV[2],digits=3)),
                        paste0("!j",brawFormat(analysis$ResultHistory$nval[2])),
                        paste0("!j",brawFormat(analysis$ResultHistory$pIV[2],digits=3)),
                        paste0("!j",brawFormat(analysis$rpIV,digits=3)),
                        paste0("!j",brawFormat(rn2w(analysis$rpIV,analysis$ResultHistory$nval[2]),digits=3)),
                        rep("",nc-6))
        outputText<-c(outputText,
                      "final",
                      paste0("!j",brawFormat(analysis$rIV,digits=3)),
                      paste0("!j",brawFormat(analysis$nval)),
                      paste0("!j",brawFormat(analysis$pIV,digits=3)),
                      paste0("!j",brawFormat(analysis$rpIV,digits=3)),
                      paste0("!j",brawFormat(rn2w(analysis$rpIV,analysis$nval),digits=3)),
                      rep("",nc-6))
      } else {
        outputText<-c(outputText,
                      "sample",
                      paste0("!j",brawFormat(analysis$rIV,digits=3)),
                      paste0("!j",brawFormat(analysis$nval)),
                      paste0("!j",brawFormat(analysis$pIV,digits=3)),
                      paste0("!j",brawFormat(analysis$rpIV,digits=3)),
                      paste0("!j",brawFormat(rn2w(analysis$rpIV,analysis$nval),digits=3)),
                      rep("",nc-6))
      }
    }
    
    if (evidence$doSEM) {
      outputText<-c(outputText,rep("",nc))
      outputText<-c(outputText,"!TNested Paths",rep("",nc-1))
      header<-c("!H!CModel", "rIV","rIV2","rIVIV2","AIC","k","llr","srmr","rmsea","Chi^2","df","AIC[1]","k[1]")
      outputText<-c(outputText,header,rep("",nc-length(header)))
      for (ig in 1:(ncol(analysis$sem)-1))
        if (!is.na(analysis$sem[1,ig])) {
          if (analysis$sem1[1,ig]==min(analysis$sem1[1,1:7],na.rm=TRUE))
            col<-'!B'
          else col<-''
        row<-c(       paste0(col,colnames(analysis$sem)[ig]),
                      brawFormat(analysis$semRs[1,ig],digits=2,na.rm=TRUE),
                      brawFormat(analysis$semRs[2,ig],digits=2,na.rm=TRUE),
                      brawFormat(analysis$semRs[3,ig],digits=2,na.rm=TRUE),
                      brawFormat(analysis$sem[1,ig],digits=1,na.rm=TRUE),
                      brawFormat(analysis$semK[ig],digits=3,na.rm=TRUE),
                      brawFormat(analysis$semLLR[ig],digits=3,na.rm=TRUE),
                      brawFormat(analysis$semSRMR[ig],digits=3,na.rm=TRUE),
                      brawFormat(analysis$semRMSEA[ig],digits=3,na.rm=TRUE),
                      brawFormat(analysis$semCHI2[ig],digits=3,na.rm=TRUE),
                      brawFormat(analysis$semDF[ig],digits=0,na.rm=TRUE),
                      brawFormat(analysis$sem1[1,ig],digits=1,na.rm=TRUE),
                      brawFormat(analysis$semK1[ig],digits=3,na.rm=TRUE)
        )
        outputText<-c(outputText,row,rep("",nc-length(row)))
        }
    }
    
    nr=length(outputText)/nc

    reportPlot(outputText,nc,nr)
    
}
