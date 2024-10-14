skewness<-function(s,na.rm=TRUE) {
  # https://en.wikipedia.org/wiki/Skewness G1
  n<-length(s)
  s<-s-mean(s,na.rm=na.rm)
  term1<-n*sqrt(n-1)/(n-2)
  term2<-sum(s^3)
  term3<-sum(s^2)^1.5
  return(term1*term2/term3)
}

kurtosis<-function(s,na.rm=TRUE) {
  # https://en.wikipedia.org/wiki/Kurtosis G2
  n<-length(s)
  s<-s-mean(s,na.rm=na.rm)
  term1<-n*(n+1)*(n-1)/(n-2)/(n-3)
  term2<-sum(s^4)
  term3<-sum(s^2)^2
  term4<-3*(n-1)^2/(n-2)/(n-3)
  return(term1*term2/term3-term4)
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

  outputText<-c()
  outputTextI<-c()
  # Interval variables first
  done_interval<-FALSE
  if (IV$type=="Interval"){
    outputTextI<-c(outputTextI,IV$name,
                   paste0("!j",brawFormat(mean(s1,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(sd(s1,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(skewness(s1,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(kurtosis(s1,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(median(s1),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(iqr(s1),digits=braw.env$report_precision))
    )
    done_interval<-TRUE
  }
  if (no_ivs>1){
    s1a<-sample$iv2
    if (IV2$type=="Interval"){
      outputTextI<-c(outputTextI,IV2$name,
                     paste0("!j",brawFormat(mean(s1a,na.rm=TRUE),digits=braw.env$report_precision)),
                     paste0("!j",brawFormat(sd(s1a,na.rm=TRUE),digits=braw.env$report_precision)),
                     paste0("!j",brawFormat(skewness(s1a,na.rm=TRUE),digits=braw.env$report_precision)),
                     paste0("!j",brawFormat(kurtosis(s1a,na.rm=TRUE),digits=braw.env$report_precision)),
                     paste0("!j",brawFormat(median(s1a),digits=braw.env$report_precision)),
                     paste0("!j",brawFormat(iqr(s1a),digits=braw.env$report_precision))
      )
      done_interval<-TRUE
    }
  }
  if (DV$type=="Interval"){
    outputTextI<-c(outputTextI,DV$name,
                   paste0("!j",brawFormat(mean(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(sd(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(skewness(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(kurtosis(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(median(s2),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(iqr(s2),digits=braw.env$report_precision))
    )
    done_interval<-TRUE
  }

  # Ordinal variables
  outputTextO=c()
  done_ordinal<-FALSE
  if (IV$type=="Ordinal"){
    outputTextO<-c(outputTextO,IV$name,
                   paste0("!j",brawFormat(median(s1),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(iqr(s1),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(mean(s1),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(sd(s1),digits=braw.env$report_precision)),
                   "",""
    )
    done_ordinal<-TRUE
  }
  if (no_ivs>1){
    if (IV2$type=="Ordinal"){
    outputTextO<-c(outputTextO,IV2$name,
                   paste0("!j",brawFormat(median(s1a),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(iqr(s1a),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(mean(s1a),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(sd(s1a),digits=braw.env$report_precision)),
                   "",""
    )
    done_ordinal<-TRUE
    }
  }
  if (DV$type=="Ordinal"){
    outputTextO<-c(outputTextO,DV$name,
                   paste0("!j",brawFormat(median(s2),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(iqr(s2),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(mean(s2),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(sd(s2),digits=braw.env$report_precision)),
                   "",""
    )
    done_ordinal<-TRUE
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
    outputTextC<-c(outputTextC,IV$name,
                   paste0("!j",counts),
                   paste0("!j",IV$cases[mode]),
                   paste0("!j",brawFormat(deviance,digits=2)),
                   "","","")
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
      outputTextC<-c(outputTextC,IV2$name,
                     paste0("!j",counts),
                     paste0("!j",IV2$cases[mode]),
                     paste0("!j",brawFormat(deviance,digits=2)),
                     "","","")
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
    outputTextC<-c(outputTextC,DV$name,
                   paste0("!j",counts),
                   paste0("!j",DV$cases[mode]),
                   paste0("!j",brawFormat(deviance,digits=2)),
                   "","","")
    done_categorical<-TRUE
  }
  
  
  if (done_interval){
    # outputText<-c(outputText,"!TInterval",rep("",nc-1))
    # outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!H!CInterval","mean","sd","skew","kurtosis","median","iqr",
                  outputTextI)
    # outputText<-c(outputText,rep("",nc))
  }
  if (done_ordinal){
    # outputText<-c(outputText,"!T",rep("",nc-1))
    # outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!H!COrdinal","median","iqr","mean","sd","","",
                  outputTextO)
    # outputText<-c(outputText,rep("",nc))
  }
  if (done_categorical){
    # outputText<-c(outputText,"!TCategorical",rep("",nc-1))
    # outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!H!CCategorical","counts","mode","deviance","","","",
                  outputTextC)
  }
  outputText<-c(outputText,rep("",nc))
  
  outputText<-c(outputText,
                # "!TDesign","","","","","","",
                "!H!CDesign","Sample Size","Method","Usage","","","",
                "",paste0("!c",sample$nval),paste0("!c",design$sMethod$type),paste0("!c",design$sIV1Use),"","",""
  )
  
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)
    
}
