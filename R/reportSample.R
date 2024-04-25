skewness<-function(s,na.rm=TRUE) {
  mean((s-mean(s,na.rm=na.rm))^3,na.rm=na.rm)/sd(s,na.rm)^3
}

kurtosis<-function(s,na.rm=TRUE) {
  mean((s-mean(s,na.rm=na.rm))^4,na.rm=na.rm)/sd(s,na.rm)^4
}

iqr<-function(s) {
  diff(quantile(s,c(0.25,0.75)))
}

#' report a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportSample(sample=doSample())
#' @export
reportSample<-function(sample=braw.res$result){
  if (is.null(sample)) sample<-doResult(autoShow=FALSE)
  
  hypothesis<-sample$hypothesis
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  design<-sample$design
  
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  s1<-sample$iv
  s2<-sample$dv
  
  nc=7
  outputText<-c("\bVariables","","","","","","")

  outputTextI<-c()
  # Interval variables first
  done_interval<-FALSE
  if (IV$type=="Interval"){
    outputTextI<-c(outputTextI,paste0("!j\b",IV$name),
                   brawFormat(mean(s1,na.rm=TRUE),digits=braw.env$report_precision),brawFormat(sd(s1,na.rm=TRUE),digits=braw.env$report_precision),
                   brawFormat(skewness(s1,na.rm=TRUE),digits=braw.env$report_precision),brawFormat(kurtosis(s1,na.rm=TRUE),digits=braw.env$report_precision),
                   brawFormat(median(s1),digits=braw.env$report_precision),brawFormat(iqr(s1),digits=braw.env$report_precision)
    )
    done_interval<-TRUE
  }
  if (no_ivs>1){
    s1a<-sample$iv2
    if (IV2$type=="Interval"){
      outputTextI<-c(outputTextI,paste0("!j\b",IV2$name),
                     brawFormat(mean(s1a),digits=braw.env$report_precision),  brawFormat(sd(s1a),digits=braw.env$report_precision),
                     brawFormat(skewness(s1a,na.rm=TRUE),digits=braw.env$report_precision),brawFormat(kurtosis(s1a,na.rm=TRUE),digits=braw.env$report_precision),
                     brawFormat(median(s1a),digits=braw.env$report_precision),brawFormat(iqr(s1a),digits=braw.env$report_precision)
      )
      done_interval<-TRUE
    }
  }
  if (DV$type=="Interval"){
    outputTextI<-c(outputTextI,paste0("!j\b",DV$name),
                   brawFormat(mean(s2),digits=braw.env$report_precision),  brawFormat(sd(s2),digits=braw.env$report_precision),
                   brawFormat(skewness(s2,na.rm=TRUE),digits=braw.env$report_precision),brawFormat(kurtosis(s2,na.rm=TRUE),digits=braw.env$report_precision),
                   brawFormat(median(s2),digits=braw.env$report_precision),brawFormat(iqr(s2),digits=braw.env$report_precision)
    )
    done_interval<-TRUE
  }
  if (done_interval){
    outputText<-c(outputText,"\bInterval","\bmean","\bsd","\bskew","\bkurtosis","\bmedian","\biqr",outputTextI)
  }

  # Ordinal variables
  outputTextO=c()
  done_ordinal<-FALSE
  if (IV$type=="Ordinal"){
    outputTextO<-c(outputTextO,paste0("!j\b",IV$name),
                   brawFormat(median(s1),digits=braw.env$report_precision),  brawFormat(iqr(s1),digits=braw.env$report_precision),
                   brawFormat(mean(s1),digits=braw.env$report_precision),  brawFormat(sd(s1),digits=braw.env$report_precision),
                   "",""
    )
    done_ordinal<-TRUE
  }
  if (no_ivs>1){
    if (IV2$type=="Ordinal"){
    outputTextO<-c(outputTextO,paste0("!j\b",IV2$name),
                   brawFormat(median(s1a),digits=braw.env$report_precision),  brawFormat(iqr(s1a),digits=braw.env$report_precision),
                   brawFormat(mean(s1a),digits=braw.env$report_precision),  brawFormat(sd(s1a),digits=braw.env$report_precision),
                   "",""
    )
    done_ordinal<-TRUE
    }
  }
  if (DV$type=="Ordinal"){
    outputTextO<-c(outputTextO,paste0("!j\b",DV$name),
                   brawFormat(median(s2),digits=braw.env$report_precision),  brawFormat(iqr(s2),digits=braw.env$report_precision),
                   brawFormat(mean(s2),digits=braw.env$report_precision),  brawFormat(sd(s2),digits=braw.env$report_precision),
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
  if (IV$type=="Categorical"){
    counts<-""
    for (i in 1:IV$ncats){
      # counts<-paste(counts,  IV$cases[i],"=", format(sum(s1==IV$cases[i]))," ",sep="")
      counts<-paste0(counts,sum(s1==IV$cases[i]),",")
    }
    counts<-substr(counts,1,nchar(counts)-1)
    mode<-which.max(table(s1))
    mode<-mode[1]
    deviance<-(sum(s1!=mode)+(length(s1)-sum(s1==mode)))/length(s1)
    outputTextC<-c(outputTextC,paste0("!j\b",IV$name),counts,"",IV$cases[mode],brawFormat(deviance,digits=2),"","")
    done_categorical<-TRUE
  }
  if (no_ivs>1){
    s1a<-sample$iv2
    if (IV2$type=="Categorical"){
      counts<-""
      for (i in 1:IV2$ncats){
        # counts<-paste(counts, IV2$cases[i],"=", format(sum(s1a==IV2$cases[i]))," ",sep="")
        counts<-paste0(counts,sum(s1a==IV2$cases[i]),",")
      }
      counts<-substr(counts,1,nchar(counts)-1)
      mode<-which.max(table(s1a))
      mode<-mode[1]
      deviance<-(sum(s1a!=mode)+(length(s1a)-sum(s1a==mode)))/length(s1a)
      outputTextC<-c(outputTextC,paste0("!j\b",IV2$name),counts,"",IV2$cases[mode],brawFormat(deviance,digits=2),"","")
      done_categorical<-TRUE
    }
  }
  if (DV$type=="Categorical"){
    counts<-""
    for (i in 1:DV$ncats){
      # counts<-paste(counts,  DV$cases[i],"=", format(sum(s2==DV$cases[i]))," ",sep="")
      counts<-paste0(counts,sum(s2==DV$cases[i]),",")
    }
    counts<-substr(counts,1,nchar(counts)-1)
    mode<-which.max(table(s2))
    mode<-mode[1]
    deviance<-(sum(s2!=mode)+(length(s2)-sum(s2==mode)))/length(s2)
    outputTextC<-c(outputTextC,paste0("!j\b",DV$name),counts,"",DV$cases[mode],brawFormat(deviance,digits=2),"","")
    done_categorical<-TRUE
  }
  if (done_categorical){
    outputText<-c(outputText,"\bCategorical","\bcounts","","\bmode","\bdeviance","","",outputTextC)
  }
  
  outputText<-c(outputText,"  ","","","","","","")
  outputText<-c(outputText,
                "\bDesign","","","","","","",
                "!jSample Size: ",sample$nval,"","","","","",
                "!jMethod: ",design$sMethod$type,"","","","","",
                "!jUsage: ",design$sIV1Use,"","","","",""
  )
  
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)
    
}
