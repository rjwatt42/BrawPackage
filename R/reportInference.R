#' report population estimates from a simulated sample
#' 
#' @param analysisType "Model", "Anova"
#' @return ggplot2 object - and printed
#' @examples
#' reportInference(analysis=doAnalysis())
#' @export
reportInference<-function(analysis=braw.res$result,analysisType="Anova",showPowerN=TRUE,compact=FALSE){
  if (is.null(analysis)) analysis<-doSingle(autoShow=FALSE)
  
  IV<-analysis$hypothesis$IV
  IV2<-analysis$hypothesis$IV2
  DV<-analysis$hypothesis$DV
  effect<-analysis$hypothesis$effect
  design<-analysis$design
  evidence<-analysis$evidence
  
  switch (analysisType,
          "Anova"= {anova<-analysis$anova},
          "Model"= {anova<-analysis$model}
  )
  nc<-ncol(anova)+2
  if (evidence$doSEM) nc<-20
  if (nc<8) nc<-8
  
  an_name<-analysis$an_name
  outputText<-c()
    outputText<-c(outputText,rep(" ",nc))
    outputText[1]<-paste0("!T",an_name)
    if (!is.null(IV2)) {
      outputText[2]<-paste("(",analysisType,"/",braw.env$modelType,")",sep="")
    }

  table1<-c()
    for (i in 1:1) {
      pval<-analysis$pIV
      df<-analysis$df
      nval<-analysis$nval
      rval<-analysis$rIV
      
      if (is.null(IV2)){
        if (pval>=10^(-braw.env$report_precision)) {
          pvalText<-paste0(brawFormat(pval,digits=braw.env$report_precision))
        } else {
          pvalText<-paste0("< ",10^(-braw.env$report_precision))
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
        if (!analysis$evidence$prior$On) {
          analysis$evidence$prior<-list(On=TRUE,PDF="Single",PDFk=analysis$rIV,RZ="r",pRplus=0.5)
        }
        analysis$dIV<-res2llr(analysis,"dLLR")
        f1<-"\bllr"
        f2<-paste("d=",brawFormat(analysis$dIV,digits=braw.env$report_precision),sep="")
      }

      rvalText<-paste0(brawFormat(rval,digits=braw.env$report_precision),
                       "\u00B1",brawFormat(r2se(rval,nval),digits=braw.env$report_precision))
      rvalText<-brawFormat(rval,digits=braw.env$report_precision)
      
      if (IV$type=="Categorical" && IV$ncats==2 && DV$type=="Interval") {
        use1<-analysis$iv==IV$cases[1]
        use2<-analysis$iv==IV$cases[2]
        diffMean<-mean(analysis$dv[use2],na.rm=TRUE)-mean(analysis$dv[use1],na.rm=TRUE)
        if (design$sIV1Use=="Within") {
          dval<-diffMean/sd(analysis$dv[use1]-analysis$dv[use2],na.rm=TRUE)
        } else 
        dval<-diffMean/sqrt(
                (
                  mean(use2,na.rm=TRUE)*sd(analysis$dv[use2],na.rm=TRUE)^2+
                    mean(use1,na.rm=TRUE)*sd(analysis$dv[use1],na.rm=TRUE)^2
                  )
                )
        table1<-c(table1,"!Htest","(df) ","value","p",f1,"r[s]","Cohen's d",rep("",nc-7))
        table1<-c(table1,paste0("!j",t_name),df,
                                 brawFormat(tval,digits=braw.env$report_precision),pvalText,
                      f2,rvalText,brawFormat(dval,digits=braw.env$report_precision),rep("",nc-7))
      } else {
        table1<-c(table1,"!Htest","(df) ","value","p",f1,"r[s]",rep("",nc-6))
        table1<-c(table1,paste0("!j",t_name),df,
                                 brawFormat(tval,digits=braw.env$report_precision),pvalText,
                      f2,rvalText,rep("",nc-6))
      }
    }
    }
    if (!is.null(IV2)) {
      nc1<-length(colnames(anova))+1
      table1<-c(table1,"!H!C ","r[s]",paste0(sub("Pr\\(","p\\(",sub("^","",colnames(anova)))),rep("",nc-1-nc1))
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
          
          table1<-c(table1,vn)
          if (i-1<=ncol(analysis$r$direct)) table1<-c(table1,brawFormat(analysis$r$direct[i-1],digits=braw.env$report_precision))
          else table1<-c(table1," ")
          for (j in 1:ncol(anova)){
            if (is.na(anova[i,j])){
              table1<-c(table1,"")
            } else {
              table1<-c(table1,paste0("!j",brawFormat(anova[i,j],digits=braw.env$report_precision)))
            }
          }
          table1<-c(table1,rep("",nc-1-nc1))
        }
      }
      if (!total_done && analysisType=="Anova") {
        ssq<-sum(anova[,1])-anova[1,1]
        if (!is.na(ssq)) {ssq<-paste0("!j",brawFormat(ssq,digits=braw.env$report_precision))} else {ssq<-""}
        
        df<-sum(anova[,2])-anova[1,2]
        if (!is.na(df)) {df<-paste0("!j",brawFormat(df,digits=braw.env$report_precision))} else {df<-""}
        table1<-c(table1,"Total "," ",ssq,df,rep(" ",nc-4))
      }
      table1<-c(table1,rep("",nc))
      table1<-c(table1,paste0("Full model:"),paste0("r[s]=",brawFormat(analysis$rFull)),paste0("p=",brawFormat(analysis$pFull)),rep("",nc-3))
      table1<-c(table1,rep("",nc))
    }
    
  table2<-c()
    if (braw.env$fullOutput>1) {
    AIC<-analysis$AIC
    llkNull<-exp(-0.5*(analysis$AIC-analysis$AICnull))
    k<-nrow(anova)-2+2
    n_data<-analysis$nval
    llr<-(2*k-AIC)/2
    AICc=AIC+(2*k*k+2*k)/(n_data-k-1);
    BIC=AIC+k*log(n_data)-2*k;
    CAIC=k*(log(n_data)+1)+AIC-2*k;
    table2<-c(table2,rep("",nc))
      table2<-c(table2,"!HAIC","AICc","BIC","AICnull","llr[+]","R^2","k","llr",rep("",nc-8))
      table2<-c(table2,
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
    
    
  table3<-c()
  if (braw.env$fullOutput>0) {
    table3<-c("!TPower",rep("",nc-1))
    nrep<-length(analysis$ResultHistory$rIV)
    if (!is.null(analysis$ResultHistory$Smax)) Smax<-"S[max]" else Smax<-""
    if (design$Replication$On && 1==2)
      table3<-c(table3,"!H","r[s]","n","p", "r[p]", "w[p]", "p(e)",rep("",nc-7))
    else table3<-c(table3,"!H","r[s]","n","p", "r[p]", "w[p]", "w[s]",Smax, rep("",nc-8))
    if (nrep>1) labels<-c("original",rep(" ",nrep-2),"final")
    else        labels<-""
    for (i in 1:nrep) {
      if (1==2) {
        if (design$Replication$On) {
          sig<-analysis$ResultHistory$pIV[i]<0.05
          if (sig) 
            p_error<-(1-effect$world$pRplus)*
              analysis$ResultHistory$pIV[i]
          else
            p_error<-(effect$world$pRplus)*
              (1-rn2w(analysis$ResultHistory$rpIV[i],analysis$ResultHistory$nval[i]))
          p_error<-brawFormat(p_error,digits=3)
          if (sig) p_error<-paste0("e[I]=",p_error)
          else     p_error<-paste0("e[II]=",p_error)
        } else
          p_error<-NULL
        
        table3<-c(table3,
                  labels[i],
                  paste0("!j",brawFormat(analysis$ResultHistory$rIV[i],digits=3)),
                  paste0("!j",brawFormat(analysis$ResultHistory$nval[i])),
                  paste0("!j",brawFormat(analysis$ResultHistory$pIV[i],digits=3)),
                  paste0("!j",brawFormat(analysis$ResultHistory$rpIV[i],digits=3)),
                  paste0("!j",brawFormat(rn2w(analysis$ResultHistory$rpIV[i],analysis$ResultHistory$nval[i]),digits=3))
        )
        if (!is.null(p_error)) table3<-c(table3,paste0("!j",p_error),rep("",nc-7))
        else table3<-c(table3,rep("",nc-6))
      } else {
        if (!is.null(analysis$ResultHistory$Smax[i]) && !is.na(analysis$ResultHistory$Smax[i])) 
             Smax<-brawFormat(analysis$ResultHistory$Smax[i],digits=2)
        else Smax<-""
        table3<-c(table3,
                  labels[i],
                  paste0("!j",brawFormat(analysis$ResultHistory$rIV[i],digits=3)),
                  paste0("!j",brawFormat(analysis$ResultHistory$nval[i])),
                  paste0("!j",brawFormat(analysis$ResultHistory$pIV[i],digits=3)),
                  paste0("!j",brawFormat(analysis$ResultHistory$rpIV[i],digits=3)),
                  paste0("!j",brawFormat(rn2w(analysis$ResultHistory$rpIV[i],analysis$ResultHistory$nval[i]),digits=3)),
                  paste0("!j",brawFormat(rn2w(analysis$ResultHistory$rIV[i],analysis$ResultHistory$nval[i]),digits=3)),
                  Smax,
                  rep("",nc-8)
                  )
      }
    }
  }
  
  table4<-c()
    if (evidence$doSEM) {
      table4<-c(table4,rep("",nc))
      table4<-c(table4,"!TNested Paths",rep("",nc-1))
      header<-c("!H!CModel", "rIV","rIV2","rIVIV2","AIC","k","llr","srmr","rmsea","Chi^2","df","AIC[1]","k[1]")
      table4<-c(table4,header,rep("",nc-length(header)))
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
        table4<-c(table4,row,rep("",nc-length(row)))
        }
    }
    
  # if (compact && braw.env$fullOutput==1) {
  #   t1<-matrix(table1,ncol=nc,byrow = TRUE)
  #   t3<-matrix(table3,ncol=nc,byrow = TRUE)
  #   while (nrow(t1)<nrow(t3)) t1<-rbind(t1,rep("",nc))
  #   outputText<-t(cbind(t1,t3))
  #   nc<-nc*2
  # } else
    outputText<-c(outputText,table1,table2,rep("",nc),table3,table4)
  
    nr=length(outputText)/nc

    reportPlot(outputText,nc,nr)
    
}
