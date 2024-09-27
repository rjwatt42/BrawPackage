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

  outputText<-c()
  outputTextI<-c()
  # Interval variables first
  done_interval<-FALSE
  if (IV$type=="Interval"){
    outputTextI<-c(outputTextI,paste0("!j!i!r",IV$name," "),
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
      outputTextI<-c(outputTextI,paste0("!j!i!r",IV2$name," "),
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
    outputTextI<-c(outputTextI,paste0("!j!i!r",DV$name," "),
                   paste0("!j",brawFormat(mean(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(sd(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(skewness(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(kurtosis(s2,na.rm=TRUE),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(median(s2),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(iqr(s2),digits=braw.env$report_precision))
    )
    done_interval<-TRUE
  }
  if (done_interval){
    outputText<-c(outputText,"\b!jInterval Variables",rep("",nc-1))
    outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!n ","\b!c!umean","\b!c!usd","\b!c!uskew","\b!c!ukurtosis","\b!c!umedian","\b!c!uiqr",
                  outputTextI,rep("",nc))
  }

  # Ordinal variables
  outputTextO=c()
  done_ordinal<-FALSE
  if (IV$type=="Ordinal"){
    outputTextO<-c(outputTextO,paste0("!j!i!r",IV$name," "),
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
    outputTextO<-c(outputTextO,paste0("!j!i!r",IV2$name," "),
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
    outputTextO<-c(outputTextO,paste0("!j!i!r",DV$name," "),
                   paste0("!j",brawFormat(median(s2),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(iqr(s2),digits=braw.env$report_precision)),
                   paste0("!j",brawFormat(mean(s2),digits=braw.env$report_precision)),  
                   paste0("!j",brawFormat(sd(s2),digits=braw.env$report_precision)),
                   "",""
    )
    done_ordinal<-TRUE
  }
  if (done_ordinal){
    outputText<-c(outputText,"\b!jOrdinal Variables",rep("",nc-1))
    outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!n ","\b!j!umedian","\b!j!uiqr","\bmean","\b!j!usd","","",
                  outputTextO,rep("",nc))
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
    outputTextC<-c(outputTextC,paste0("!j!i!r",IV$name," "),
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
      outputTextC<-c(outputTextC,paste0("!j!i!r",IV2$name," "),
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
    outputTextC<-c(outputTextC,paste0("!j!i!r",DV$name," "),
                   paste0("!j",counts),
                   paste0("!j",DV$cases[mode]),
                   paste0("!j",brawFormat(deviance,digits=2)),
                   "","","")
    done_categorical<-TRUE
  }
  if (done_categorical){
    outputText<-c(outputText,"\b!jCategorical Variables",rep("",nc-1))
    outputText<-c(outputText,rep("",nc))
    outputText<-c(outputText,"!n ","\b!c!ucounts","\b!c!umode","\b!c!udeviance","","","",
                  outputTextC)
  }
  outputText<-c(outputText,rep(" ",nc))
  
  outputText<-c(outputText,rep("",nc))
  outputText<-c(outputText,
                "\bDesign","","","","","","",
                "!n ","\b!c!uSample Size","\b!c!uMethod","\b!c!uUsage","","","",
                "",paste0("!c",sample$nval),paste0("!c",design$sMethod$type),paste0("!c",design$sIV1Use),"","",""
  )
  
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)
    
}
