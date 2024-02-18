
reportExpected<-function(expectedResult=makeExpected(100),type="Basic"){
  IV<-expectedResult$hypothesis$IV
  IV2<-expectedResult$hypothesis$IV2
  DV<-expectedResult$hypothesis$DV
  effect<-expectedResult$hypothesis$effect
  result<-expectedResult$result
  nullresult<-expectedResult$nullresult
  
  if (length(type)==1) {
    switch(type,
           "Basic"=     {pars<-c("r","p")},
           "CILimits"=  {pars<-c("ci1","ci2")},
           "NHSTErrors"={pars<-c("e2","e1")},
           "FDR"=       {pars<-c("e1","e2")},
           {pars<-c(type,NA)}
    )
  } else pars<-type
  
  if (is.null(IV2) | is.null(result$rIVIV2DV)){nc=3}
  else{
    if (is.na(result$rIVIV2DV[1])) {nc=6} else {nc=9}
  }
  if (type=="NHSTErrors" || type=="FDR"){nc=4}
  
  # header
  if (sum(!is.na(nullresult$rIV))>0) {
    outputText<-c("\bExpected",paste("nsims=",format(sum(!is.na(result$rIV))),"+",format(sum(!is.na(nullresult$rIV))),sep=""),rep("",nc-2))
  } else {
    outputText<-c("\bExpected",paste("nsims=",format(sum(!is.na(result$rIV))),sep=""),rep("",nc-2))
  }
  outputText<-c(outputText,rep("",nc))
  if (!(is.null(IV2) | is.null(result$rIVIV2DV))){
    outputText<-c(outputText,"","\b Main Effect 1","","","\b Main Effect 2","")
    if (!is.na(result$rIVIV2DV[1])) outputText<-c(outputText,"","\bInteraction","")
    }

  if (is.null(IV2)) {
    rs<-result$rIV
    ps<-result$pIV
  } else {
    switch (result$showType,
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
  if (type=="NHSTErrors") {outputText1<-c("!j\bErrors:","\bI","\bII"," ")}
  else {
    if (type=="CILimits") {outputText1<-c("   ","lower","upper")}
    else {
      outputText1<-
      par1<-pars[1]
      par2<-pars[2]
      if (RZ=="z") {
        switch(par1,
               "r"={par1="z"},
               "rp"={par1="zp"},
               "r1"={par1="z1"},
               {par1=par1}
        )
        switch(par2,
               "r"={par2="z"},
               "rp"={par2="zp"},
               "r1"={par2="z1"},
               {par2=par2}
        )
      }
      outputText1<-c("   ",paste0("\b",par1),paste0("\b",par2))
    }
  }
  outputText<-c(outputText,rep(outputText1,nc/3))

  if (type=="NHSTErrors"){
    nullSig<-isSignificant(BrawOpts$STMethod,nullresult$pIV,nullresult$rIV,nullresult$nval,nullresult$df1,nullresult$evidence)
    resSig<-isSignificant(BrawOpts$STMethod,result$pIV,result$rIV,result$nval,result$df1,result$evidence)
    if (BrawOpts$STMethod=="dLLR") {
      d<-res2llr(result,"dLLR")
      nulld<-res2llr(nullresult,"dLLR")
      nullSigW<-nulld>0 & nullSig
      nullSigC<-nulld<0 & nullSig
      resSigW<-(d<0 & resSig)
      resSigC<-(d>0 & resSig)
    }
    if (BrawOpts$STMethod=="NHST") {
      e1=paste0(format(mean(nullSig)*100,digits=report_precision),"%")
      e2=paste0(format(mean(!resSig)*100,digits=report_precision),"%")
    } else {
      e1=paste0(format(sum(nullSigW)/length(nullSig)*100,digits=report_precision),"%")
      e2=paste0(format(sum(resSigW)/length(resSig)*100,digits=report_precision),"%")
    }
    if (result$effect$world$worldOn) {
        nr<-(length(nullresult$pIV)+length(result$pIV))
        if (BrawOpts$STMethod=="NHST") {
          e1a<-paste0(format(sum(nullSig)/nr*100,digits=report_precision),"%")
          e2a<-paste0(format(sum(!resSig)/nr*100,digits=report_precision),"%")
          outputText<-c(outputText,"!jAll",e1a,e2a," ")
          
          e1=paste0(format(mean(nullSig)*100,digits=report_precision),"%")
          e2=paste0(format(mean(!resSig)*100,digits=report_precision),"%")
          outputText<-c(outputText,"!jr=0",e1," "," ")
          outputText<-c(outputText,paste0("!jr","\u2260","0")," ",e2," ")
          
          e1b=paste0("\b",format((sum(nullSig)+sum(!resSig))/nr*100,digits=report_precision),"%")
          e2b=paste0(format((sum(!nullSig)+sum(resSig))/nr*100,digits=report_precision),"%")
          e1c=paste0("(",format((sum(nullSig)+sum(resSig))/nr*100,digits=report_precision),"%)")
          e2c=paste0("(",format((sum(!nullSig)+sum(!resSig))/nr*100,digits=report_precision),"%)")
          
          e1n=paste0("\b",format(sum(nullSig)/(sum(nullSig)+sum(resSig))*100,digits=report_precision),"%")
          e1p=paste0(format(sum(resSig)/(sum(nullSig)+sum(resSig))*100,digits=report_precision),"%")
          e2n=paste0(format(sum(!nullSig)/(sum(!nullSig)+sum(!resSig))*100,digits=report_precision),"%")
          e2p=paste0("\b",format(sum(!resSig)/(sum(!nullSig)+sum(!resSig))*100,digits=report_precision),"%")
        } else {
          e1a<-paste0(format((sum(nullSigW))/nr*100,digits=report_precision),"%")
          e2a<-paste0(format((sum(resSigW))/nr*100,digits=report_precision),"%")
          outputText<-c(outputText,"!jAll",e1a,e2a," ")
          
          e1=paste0(format(sum(nullSigW)/length(nullSig)*100,digits=report_precision),"%")
          e2=paste0(format(sum(resSigW)/length(resSig)*100,digits=report_precision),"%")
          outputText<-c(outputText,"!jr=0",e1," "," ")
          outputText<-c(outputText,paste0("!jr","\u2260","0")," ",e2," ")

          e1b=paste0("\b",format((sum(nullSigW)+sum(resSigW))/nr*100,digits=report_precision),"%")
          e2b=paste0(format((sum(nullSigC)+sum(resSigC))/nr*100,digits=report_precision),"%")
          e1c=paste0("(",format((sum(nullSig)+sum(resSig))/nr*100,digits=report_precision),"%)")

          e1n=paste0("\b",format((sum(nullSigW)+sum(resSigW))/(sum(nullSig)+sum(resSig))*100,digits=report_precision),"%")
          e1p=paste0(format((sum(nullSigC)+sum(resSigC))/(sum(nullSig)+sum(resSig))*100,digits=report_precision),"%")
        }
        # ea=paste0("Combined: ",format((sum(nullSig)+sum(!resSig))/nr*100,digits=report_precision),"%")
        outputText<-c(outputText," ","","","")
        outputText<-c(outputText,"!j\bOutcomes:","\bFalse","\bValid","")
        
        outputText<-c(outputText,"!jAll:",e1b,e2b,"")
        outputText<-c(outputText,paste0("!jSig ",e1c,":"),e1n,e1p," ")
        
        if (BrawOpts$STMethod=="NHST") {
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

      if (type=="CILimits"){
        a<-r2ci(r,result$nval[1],-1)
        b<-r2ci(r,result$nval[1],+1)
      } else {
        switch (pars[1],
                "r"={
                  a<-r
                  if (RZ=="z") a<-atanh(a)
                },
                "p"={a<-p},
                "rp"={
                  a<-result$rpIV
                  if (RZ=="z") a<-atanh(a)
                },
                "ra"={
                  a<-result$raIV
                  if (RZ=="z") a<-atanh(a)
                },
                "r1"={
                  a<-result$roIV
                  if (RZ=="z") a<-atanh(a)
                },
                "p1"={a<-result$poIV},
                "log(lrs)"={a<-res2llr(result,"sLLR")},
                "log(lrd)"={a<-res2llr(result,"dLLR")},
                "n"={a<-result$nval},
                "w"={a<-rn2w(r,result$nval)},
                "nw"={a<-rw2n(r,0.8,result$design$sReplTails)},
                "wp"={a<-rn2w(result$rpIV,result$nval)}
        )
        switch (pars[2],
                "r"={
                  b<-r
                  if (RZ=="z") b<-atanh(b)
                },
                "p"={b<-p},
                "rp"={
                  b<-result$rpIV
                  if (RZ=="z") b<-atanh(b)
                },
                "ra"={
                  b<-result$rpIV
                  if (RZ=="z") b<-atanh(b)
                },
                "r1"={
                  b<-result$roIV
                  if (RZ=="z") b<-atanh(b)
                },
                "p1"={b<-result$poIV},
                "log(lrs)"={b<-res2llr(result,"sLLR")},
                "log(lrd)"={b<-res2llr(result,"dLLR")},
                "n"={b<-result$nval},
                "w"={b<-rn2w(r,result$nval)},
                "nw"={b<-rw2n(r,0.8,result$design$sReplTails)},
                "wp"={b<-rn2w(result$rpIV,result$nval)}
        )
      }
      ot1<-c(ot1,
             "!jmean",
             format(mean(a,na.rm=TRUE),digits=report_precision),
             format(mean(b,na.rm=TRUE),digits=report_precision)
      )
      ot2<-c(ot2,
             "!jsd",
             format(sd(a,na.rm=TRUE),digits=report_precision),
             format(sd(b,na.rm=TRUE),digits=report_precision)
      )
      ot4<-c(ot4,
             "!jquant75",
             format(quantile(a,0.75,na.rm=TRUE,names<-FALSE),digits=report_precision),
             format(quantile(b,0.75,na.rm=TRUE,names<-FALSE),digits=report_precision)
      )
      ot5<-c(ot5,
             "!jmedian",
             format(quantile(a,0.5,na.rm=TRUE,names<-FALSE),digits=report_precision),
             format(quantile(b,0.5,na.rm=TRUE,names<-FALSE),digits=report_precision)
      )
      ot6<-c(ot6,
             "!jquant25",
             format(quantile(a,0.25,na.rm=TRUE,names<-FALSE),digits=report_precision),
             format(quantile(b,0.25,na.rm=TRUE,names<-FALSE),digits=report_precision)
      )
      if (i>1){
        ot1[length(ot1)-2]<-""
        ot2[length(ot1)-2]<-""
        ot4[length(ot1)-2]<-""
        ot5[length(ot1)-2]<-""
        ot6[length(ot1)-2]<-""
      }
    }
    outputText<-c(outputText,ot1,ot2,rep("  ",nc),ot4,ot5,ot6)
    if (pars[1]=="p") {
      outputText<-c(outputText,rep("  ",nc),"p(sig)",paste0(format(mean(p<BrawOpts$alphaSig)*100,digits=report_precision),"%"),rep(" ",nc-2))
    }
    if (pars[2]=="p") {
      outputText<-c(outputText,rep("  ",nc),"p(sig)"," ",paste0(format(mean(p<BrawOpts$alphaSig)*100,digits=report_precision),"%"),rep(" ",nc-3))
    }
  }
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
