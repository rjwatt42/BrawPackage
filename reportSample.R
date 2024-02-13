iqr<-function(s) {
  diff(quantile(s,c(0.25,0.75)))
}

reportSample<-function(result){
  
  if (is.null(result$IV2)) no_ivs<-1 else no_ivs<-2
  s1<-result$iv
  s2<-result$dv
  
  nc=7
  outputText<-c("\bVariables","","","","","","")

  outputTextI<-c()
  # Interval variables first
  done_interval<-FALSE
  if (result$IV$type=="Interval"){
    result$IV$sample_mu<-mean(s1)
    result$IV$sample_sd<-sd(s1,na.rm=TRUE)
    result$IV$sample_skew<-0
    result$IV$sample_kurtosis<-0
    outputTextI<-c(outputTextI,result$IV$name,
                   format(mean(s1,na.rm=TRUE),digits=report_precision),format(sd(s1,na.rm=TRUE),digits=report_precision),
                   format(skewness(s1,na.rm=TRUE),digits=report_precision),format(kurtosis(s1,na.rm=TRUE)+3,digits=report_precision),
                   format(median(s1),digits=report_precision),format(iqr(s1),digits=report_precision)
    )
    done_interval<-TRUE
  }
  if (no_ivs>1){
    s1a<-result$iv2
    if (result$IV2$type=="Interval"){
      result$IV2$sample_mu<-mean(s1a)
      result$IV2$sample_sd<-sd(s1a,na.rm=TRUE)
      outputTextI<-c(outputTextI,result$IV2$name,
                     format(mean(s1a),digits=report_precision),  format(sd(s1a),digits=report_precision),
                     format(skewness(s1a,na.rm=TRUE),digits=report_precision),format(kurtosis(s1a,na.rm=TRUE)+3,digits=report_precision),
                     format(median(s1a),digits=report_precision),format(iqr(s1a),digits=report_precision)
      )
      done_interval<-TRUE
    }
  }
  if (result$DV$type=="Interval"){
    result$DV$sample_mu<-mean(s2)
    result$DV$sample_sd<-sd(s2,na.rm=TRUE)
    outputTextI<-c(outputTextI,result$DV$name,
                   format(mean(s2),digits=report_precision),  format(sd(s2),digits=report_precision),
                   format(skewness(s2,na.rm=TRUE),digits=report_precision),format(kurtosis(s2,na.rm=TRUE)+3,digits=report_precision),
                   format(median(s2),digits=report_precision),format(iqr(s2),digits=report_precision)
    )
    done_interval<-TRUE
  }
  if (done_interval){
    outputText<-c(outputText,"\bInterval","\bmean","\bsd","\bskew","\bkurtosis","\bmedian","\biqr",outputTextI)
  }

  # Ordinal variables
  outputTextO=c()
  done_ordinal<-FALSE
  if (result$IV$type=="Ordinal"){
    outputTextO<-c(outputTextO,result$IV$name,
                   format(median(s1),digits=report_precision),  format(iqr(s1),digits=report_precision),
                   format(mean(s1),digits=report_precision),  format(sd(s1),digits=report_precision),
                   "",""
    )
    done_ordinal<-TRUE
  }
  if (no_ivs>1){
    if (result$IV2$type=="Ordinal"){
    outputTextO<-c(outputTextO,result$IV2$name,
                   format(median(s1a),digits=report_precision),  format(iqr(s1a),digits=report_precision),
                   format(mean(s1a),digits=report_precision),  format(sd(s1a),digits=report_precision),
                   "",""
    )
    done_ordinal<-TRUE
    }
  }
  if (result$DV$type=="Ordinal"){
    outputTextO<-c(outputTextO,result$DV$name,
                   format(median(s2),digits=report_precision),  format(iqr(s2),digits=report_precision),
                   format(mean(s2),digits=report_precision),  format(sd(s2),digits=report_precision),
                   "",""
    )
    done_ordinal<-TRUE
  }
  if (done_ordinal){
    outputText<-c(outputText,"\bOrdinal","\bmedian","\biqr","\bmean","\bsd","","",outputTextO)
  }

  # Categorical variables
  outputTextC=c()
  done_categorical<-FALSE
  if (result$IV$type=="Categorical"){
    counts<-""
    for (i in 1:result$IV$ncats){
      # counts<-paste(counts,  IV$cases[i],"=", format(sum(s1==IV$cases[i]))," ",sep="")
      counts<-paste0(counts,sum(s1==result$IV$cases[i]),",")
    }
    counts<-substr(counts,1,nchar(counts)-1)
    mode<-which.max(table(s1))
    mode<-mode[1]
    deviance<-(sum(s1!=mode)+(length(s1)-sum(s1==mode)))/length(s1)
    outputTextC<-c(outputTextC,result$IV$name,counts,"",result$IV$cases[mode],format(deviance,digits=2),"","")
    done_categorical<-TRUE
  }
  if (no_ivs>1){
    s1a<-result$iv2
    if (result$IV2$type=="Categorical"){
      counts<-""
      for (i in 1:result$IV2$ncats){
        # counts<-paste(counts, IV2$cases[i],"=", format(sum(s1a==IV2$cases[i]))," ",sep="")
        counts<-paste0(counts,sum(s1a==result$IV2$cases[i]),",")
      }
      counts<-substr(counts,1,nchar(counts)-1)
      mode<-which.max(table(s1a))
      mode<-mode[1]
      deviance<-(sum(s1a!=mode)+(length(s1a)-sum(s1a==mode)))/length(s1a)
      outputTextC<-c(outputTextC,result$IV2$name,counts,"",result$IV2$cases[mode],format(deviance,digits=2),"","")
      done_categorical<-TRUE
    }
  }
  if (result$DV$type=="Categorical"){
    counts<-""
    for (i in 1:result$DV$ncats){
      # counts<-paste(counts,  DV$cases[i],"=", format(sum(s2==DV$cases[i]))," ",sep="")
      counts<-paste0(counts,sum(s2==result$DV$cases[i]),",")
    }
    counts<-substr(counts,1,nchar(counts)-1)
    mode<-which.max(table(s2))
    mode<-mode[1]
    deviance<-(sum(s2!=mode)+(length(s2)-sum(s2==mode)))/length(s2)
    outputTextC<-c(outputTextC,result$DV$name,counts,"",result$DV$cases[mode],format(deviance,digits=2),"","")
    done_categorical<-TRUE
  }
  if (done_categorical){
    outputText<-c(outputText,"\bCategorical","\bcounts","","\bmode","\bdeviance","","",outputTextC)
  }
  
  outputText<-c(outputText,"  ","","","","","","")
  outputText<-c(outputText,
                "\bDesign","","","","","","",
                "Sample Size: ",result$nval,"","","","","",
                "Method: ",design$sMethod,"","","","","",
                "Usage: ",design$sIV1Use,"","","","",""
  )
  
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)
    
}
