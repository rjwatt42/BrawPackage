
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "CILimits", "NHST", "Hits" \cr
#'        \emph{ or one or two of:} \cr
#'                   "rs","p","ci1","ci2", "rp","n" \cr 
#'                   "ws","wp","nw", ro","po"
#' @return ggplot2 object - and printed
#' @export
reportExpected<-function(expectedResult=braw.res$expected,showType="Basic",
                         whichEffect="All",effectType="direct"){
  if (is.null(expectedResult)) expectedResult=doExpected(autoShow=FALSE)
  if (is.element(whichEffect,c("All","Mains"))) whichEffect<-"Main 1"
  
  IV<-expectedResult$hypothesis$IV
  IV2<-expectedResult$hypothesis$IV2
  DV<-expectedResult$hypothesis$DV
  effect<-expectedResult$hypothesis$effect
  result<-expectedResult$result
  nullresult<-expectedResult$nullresult
  
  if (effect$world$worldOn) {
    r<-getNulls(result)
    result<-r$analysis
    nullresult<-r$nullanalysis
  }
  
  if (length(showType)==1) {
    switch(showType,
           "Basic"=     {pars<-c("rs","p")},
           "p(sig)"= {pars<-c("p")},
           "Power"=     {pars<-c("ws","wp")},
           "2D"=     {pars<-c("rs","p")},
           "CILimits"=  {pars<-c("ci1","ci2")},
           "NHST"={pars<-c("e2","e1")},
           "Hits"=       {pars<-c("e1a","e2a")},
           "Misses"=       {pars<-c("e1b","e2b")},
           { pars<-strsplit(showType,";")[[1]]
           }
    )
    if (length(pars)==1) pars<-c(pars,NA)
  } else pars<-showType
  
  if (is.null(IV2) | is.null(result$rIVIV2DV)){nc=3}
  else{
    if (is.na(result$rIVIV2DV[1])) {nc=6} else {nc=9}
  }
  if (is.element(showType,c("NHST","Hits","Misses"))) {nc=4}
  
  # header
  if (is.element(showType,c("NHST","Hits","Misses")) && sum(!is.na(nullresult$rIV))>0) {
    outputText<-c("\bExpected  ",paste("nsims = ",format(sum(!is.na(result$rIV))),"+",format(sum(!is.na(nullresult$rIV))),sep=""),rep("",nc-2))
  } else {
    outputText<-c("\bExpected  ",paste("nsims = ",format(sum(!is.na(result$rIV))),sep=""),rep("",nc-2))
  }
  outputText<-c(outputText,rep("",nc))
  
  outputText<-c(outputText,paste0("\b",whichEffect),rep("",nc-1))
  if (!(is.null(IV2) | is.null(result$rIVIV2DV))){
    outputText<-c(outputText,"","\bdirect","","","\bunique","","","\btotal","")
    }

  if (is.null(IV2)) {
    rs<-matrix(result$rIV,ncol=1)
    ps<-matrix(result$pIV,ncol=1)
  } else {
    switch (effectType,
            "direct"={rs<-result$r$direct
                      ps<-result$p$direct},
            "unique"={rs<-result$r$unique
                      ps<-result$p$unique},
            "total"={rs<-result$r$total
                     ps<-result$p$total},
            "all"={
                  rs<-c()
                  ps<-c()
                  xoff<-c()
                  for (jk in 1:ncol(result$r$direct)) {
                    rs<-cbind(rs,result$r$direct[,jk],result$r$unique[,jk],result$r$total[,jk])
                    ps<-cbind(ps,result$p$direct[,jk],result$p$unique[,jk],result$p$total[,jk])
                    xoff<-cbind(xoff,c(0,0.2,0.4)+(jk-1))
                  }
                },
            "coefficients"={rs<-result$r$coefficients
                      ps<-result$p$direct}
    )
  }

  # column labels
  if (is.element(showType,c("NHST","tDR","Hits","Misses"))) {
    outputText1<-c("!j\bErrors:","\bI","\bII"," ")
    } else {
    if (showType=="CILimits") {outputText1<-c("   ","lower","upper")}
    else {
      outputText1<-c()
      par1<-pars[1]
      par2<-pars[2]
      if (braw.env$RZ=="z") {
        switch(par1,
               "rs"={par1="zs"},
               "rp"={par1="zp"},
               "ro"={par1="zo"},
               "re"={par1="ze"},
               {par1=par1}
        )
        switch(par2,
               "rs"={par2="zs"},
               "rp"={par2="zp"},
               "ro"={par2="zo"},
               "re"={par2="ze"},
               {par2=par2}
        )
      }
      par1<-gsub("^([rz]{1})([spoe]{1})$","\\1\\[\\2\\]",par1)
      par2<-gsub("^([rz]{1})([spoe]{1})$","\\1\\[\\2\\]",par2)
      if (!is.na(pars[2]))
        outputText1<-c("   ",paste0("!j\b",par1,"  "),paste0("!j\b",par2,"  "))
      else 
        outputText1<-c("   ",paste0("!j\b",par1," "),paste0("!j\b"," "))
    }
    }
  outputText<-c(outputText,rep(outputText1,nc/3))

  if (is.element(showType,c("NHST","Hits","Misses"))){
    nullSig<-isSignificant(braw.env$STMethod,nullresult$pIV,nullresult$rIV,nullresult$nval,nullresult$df1,nullresult$evidence)
    resSig<-isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,result$evidence)
    if (braw.env$STMethod=="dLLR") {
      d<-res2llr(result,"dLLR")
      nulld<-res2llr(nullresult,"dLLR")
      nullSigW<-nulld>0 & nullSig
      nullSigC<-nulld<0 & nullSig
      resSigW<-(d<0 & resSig)
      resSigC<-(d>0 & resSig)
    }
    
    if (braw.env$STMethod=="NHST") {
      e1=paste0(brawFormat(mean(nullSig)*100,digits=braw.env$report_precision),"%")
      e2=paste0(brawFormat(mean(!resSig)*100,digits=braw.env$report_precision),"%")
    } else {
      e1=paste0(brawFormat(sum(nullSigW)/length(nullSig)*100,digits=braw.env$report_precision),"%")
      e2=paste0(brawFormat(sum(resSigW)/length(resSig)*100,digits=braw.env$report_precision),"%")
    }
    
    if (result$effect$world$worldOn) {
        nr<-(length(nullresult$pIV)+length(result$pIV))
        if (braw.env$STMethod=="NHST") {
          e1a<-paste0(brawFormat(sum(nullSig)/nr*100,digits=braw.env$report_precision),"%")
          e2a<-paste0(brawFormat(sum(!resSig)/nr*100,digits=braw.env$report_precision),"%")
          outputText<-c(outputText,"!jAll",e1a,e2a," ")
          
          e1=paste0(brawFormat(mean(nullSig)*100,digits=braw.env$report_precision),"%")
          e2=paste0(brawFormat(mean(!resSig)*100,digits=braw.env$report_precision),"%")
          outputText<-c(outputText,"!jr=0",e1," "," ")
          outputText<-c(outputText,paste0("!jr","\u2260","0")," ",e2," ")
          
          e1b=paste0("\b",brawFormat((sum(nullSig)+sum(!resSig))/nr*100,digits=braw.env$report_precision),"%")
          e2b=paste0(brawFormat((sum(!nullSig)+sum(resSig))/nr*100,digits=braw.env$report_precision),"%")
          e1c=paste0("(",brawFormat((sum(nullSig)+sum(resSig))/nr*100,digits=braw.env$report_precision),"%)")
          e2c=paste0("(",brawFormat((sum(!nullSig)+sum(!resSig))/nr*100,digits=braw.env$report_precision),"%)")
          
          e1n=paste0("\b",brawFormat(sum(nullSig)/(sum(nullSig)+sum(resSig))*100,digits=braw.env$report_precision),"%")
          e1p=paste0(brawFormat(sum(resSig)/(sum(nullSig)+sum(resSig))*100,digits=braw.env$report_precision),"%")
          e2n=paste0(brawFormat(sum(!nullSig)/(sum(!nullSig)+sum(!resSig))*100,digits=braw.env$report_precision),"%")
          e2p=paste0("\b",brawFormat(sum(!resSig)/(sum(!nullSig)+sum(!resSig))*100,digits=braw.env$report_precision),"%")
        } else {
          e1a<-paste0(brawFormat((sum(nullSigW))/nr*100,digits=braw.env$report_precision),"%")
          e2a<-paste0(brawFormat((sum(resSigW))/nr*100,digits=braw.env$report_precision),"%")
          outputText<-c(outputText,"!jAll",e1a,e2a," ")
          
          e1=paste0(brawFormat(sum(nullSigW)/length(nullSig)*100,digits=braw.env$report_precision),"%")
          e2=paste0(brawFormat(sum(resSigW)/length(resSig)*100,digits=braw.env$report_precision),"%")
          outputText<-c(outputText,"!jr=0",e1," "," ")
          outputText<-c(outputText,paste0("!jr","\u2260","0")," ",e2," ")

          e1b=paste0("\b",brawFormat((sum(nullSigW)+sum(resSigW))/nr*100,digits=braw.env$report_precision),"%")
          e2b=paste0(brawFormat((sum(nullSigC)+sum(resSigC))/nr*100,digits=braw.env$report_precision),"%")
          e1c=paste0("(",brawFormat((sum(nullSig)+sum(resSig))/nr*100,digits=braw.env$report_precision),"%)")

          e1n=paste0("\b",brawFormat((sum(nullSigW)+sum(resSigW))/(sum(nullSig)+sum(resSig))*100,digits=braw.env$report_precision),"%")
          e1p=paste0(brawFormat((sum(nullSigC)+sum(resSigC))/(sum(nullSig)+sum(resSig))*100,digits=braw.env$report_precision),"%")
        }
        # ea=paste0("Combined: ",brawFormat((sum(nullSig)+sum(!resSig))/nr*100,digits=braw.env$report_precision),"%")
        outputText<-c(outputText," ","","","")
        outputText<-c(outputText,"!j\bOutcomes:","\bFalse","\bValid","")
        
        outputText<-c(outputText,"!jAll:",e1b,e2b,"")
        outputText<-c(outputText,paste0("!jSig ",e1c,":"),e1n,e1p," ")
        
        if (braw.env$STMethod=="NHST") {
        outputText<-c(outputText,paste0("!jNot Sig ",e2c,":"),e2p,e2n," ")
        }
      } else {
        outputText<-c(outputText," ",e1,e2," ")
    }
      
  }else{
    
    ot1<-c()
    ot2<-c()
    ot4<-c()
    ot5<-c()
    ot6<-c()
    
    for (i in 1:(nc/3)) {
      r<-rs[,i]
      p<-ps[,i]

      if (showType=="CILimits"){
        a<-r2ci(r,result$nval[1],-1)
        b<-r2ci(r,result$nval[1],+1)
      } else {
        switch (pars[1],
                "rs"={
                  a<-r
                  if (braw.env$RZ=="z") a<-atanh(a)
                },
                "p"={a<-p},
                "rp"={
                  a<-result$rpIV
                  if (braw.env$RZ=="z") a<-atanh(a)
                },
                "ro"={
                  a<-result$roIV
                  if (braw.env$RZ=="z") a<-atanh(a)
                },
                "re"={
                  a<-result$rIV-result$rpIV
                  if (braw.env$RZ=="z") a<-atanh(a)
                },
                "po"={a<-result$poIV},
                "log(lrs)"={a<-res2llr(result,"sLLR")},
                "log(lrd)"={a<-res2llr(result,"dLLR")},
                "n"={a<-result$nval},
                "ws"={a<-rn2w(r,result$nval)},
                "nw"={a<-rw2n(r,0.8,result$design$Replication$Tails)},
                "wp"={a<-rn2w(result$rpIV,result$nval)}
        )
        switch (pars[2],
                "rs"={
                  b<-r
                  if (braw.env$RZ=="z") b<-atanh(b)
                },
                "p"={b<-p},
                "rp"={
                  b<-result$rpIV
                  if (braw.env$RZ=="z") b<-atanh(b)
                },
                "ro"={
                  b<-result$roIV
                  if (braw.env$RZ=="z") b<-atanh(b)
                },
                "re"={
                  b<-result$rIV-result$rpIV
                  if (braw.env$RZ=="z") b<-atanh(b)
                },
                "po"={b<-result$poIV},
                "log(lrs)"={b<-res2llr(result,"sLLR")},
                "log(lrd)"={b<-res2llr(result,"dLLR")},
                "n"={b<-result$nval},
                "ws"={b<-rn2w(r,result$nval)},
                "wp"={b<-rn2w(result$rpIV,result$nval)},
                "nw"={b<-rw2n(r,0.8,result$design$Replication$Tails)}
        )
      }
      if (!is.na(pars[2])) {
      ot1<-c(ot1,
             "!j!i\bmean ",
             paste0("!j",brawFormat(mean(a,na.rm=TRUE),digits=braw.env$report_precision)),
             paste0("!j",brawFormat(mean(b,na.rm=TRUE),digits=braw.env$report_precision))
      )
      ot2<-c(ot2,
             "!j!isd ",
             paste0("!j",brawFormat(sd(a,na.rm=TRUE),digits=braw.env$report_precision)),
             paste0("!j",brawFormat(sd(b,na.rm=TRUE),digits=braw.env$report_precision))
      )
      ot4<-c(ot4,
             "!j!iquant75 ",
             paste0("!j",brawFormat(quantile(a,0.75,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision)),
             paste0("!j",brawFormat(quantile(b,0.75,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision))
      )
      ot5<-c(ot5,
             "!j!i\bmedian ",
             paste0("!j",brawFormat(quantile(a,0.5,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision)),
             paste0("!j",brawFormat(quantile(b,0.5,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision))
      )
      ot6<-c(ot6,
             "!j!iquant25 ",
             paste0("!j",brawFormat(quantile(a,0.25,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision)),
             paste0("!j",brawFormat(quantile(b,0.25,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision))
      )
      } else {
        ot1<-c(ot1,
               "!j!i\bmean ",
               paste0("!j",brawFormat(mean(a,na.rm=TRUE),digits=braw.env$report_precision)),
               " "
        )
        ot2<-c(ot2,
               "!j!isd ",
               paste0("!j",brawFormat(sd(a,na.rm=TRUE),digits=braw.env$report_precision)),
               " "
        )
        ot4<-c(ot4,
               "!j!iquant75 ",
               paste0("!j",brawFormat(quantile(a,0.75,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision)),
               " "
        )
        ot5<-c(ot5,
               "!j!i\bmedian ",
               paste0("!j",brawFormat(quantile(a,0.5,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision)),
               " "
        )
        ot6<-c(ot6,
               "!j!iquant25 ",
               paste0("!j",brawFormat(quantile(a,0.25,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision)),
               " "
        )
      }
      if (i>1){
        ot1[length(ot1)-2]<-""
        ot2[length(ot1)-2]<-""
        ot4[length(ot1)-2]<-""
        ot5[length(ot1)-2]<-""
        ot6[length(ot1)-2]<-""
      }
    }
    outputText<-c(outputText,ot4,ot5,ot6,rep("  ",nc),ot1,ot2)
    if (pars[1]=="p" || pars[2]=="p") {
      if (is.null(IV2)) {
        outputText<-c(outputText,rep("  ",nc),"!j\bp(sig) = ",paste0("!j",brawFormat(mean(p<braw.env$alphaSig)*100,digits=braw.env$report_precision),"%"),rep(" ",nc-2))
      } else {
        if (is.na(result$rIVIV2DV[1])) {
          outputText<-c(outputText,rep("  ",nc),"!j\bp(sig) = ",
                        paste0("!j",brawFormat(mean(ps[,1]<braw.env$alphaSig)*100,digits=braw.env$report_precision),"%"),
                        " "," ",paste0("!j",brawFormat(mean(ps[,2]<braw.env$alphaSig)*100,digits=braw.env$report_precision),"%"),
                        rep(" ",nc-5))
        } else {
          outputText<-c(outputText,rep("  ",nc),"!j\bp(sig) = ",
                        paste0("!j",brawFormat(mean(ps[,1]<braw.env$alphaSig)*100,digits=braw.env$report_precision),"%"),
                        " "," ",paste0("!j",brawFormat(mean(ps[,2]<braw.env$alphaSig)*100,digits=braw.env$report_precision),"%"),
                        " "," ",paste0("!j",brawFormat(mean(ps[,3]<braw.env$alphaSig)*100,digits=braw.env$report_precision),"%"),
                        rep(" ",nc-8))
        }
      }
    }
  }
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
