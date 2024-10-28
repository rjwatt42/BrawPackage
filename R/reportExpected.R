
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "CILimits", "NHST", "Hits" \cr
#'        \emph{ or one or two of:} \cr
#'                   "rs","p","ci1","ci2", "rp","n" \cr 
#'                   "ws","wp","nw", ro","po"
#' @return ggplot2 object - and printed
#' @export
reportExpected<-function(expectedResult=braw.res$expected,showType="Basic",
                         whichEffect="All",effectType="all",reportStats="Medians"){
  if (is.null(expectedResult)) expectedResult=doExpected(autoShow=FALSE)
  
  reportMeans<-(reportStats=="Means")
  reportQuants<-FALSE
  
  IV<-expectedResult$hypothesis$IV
  IV2<-expectedResult$hypothesis$IV2
  DV<-expectedResult$hypothesis$DV
  effect<-expectedResult$hypothesis$effect
  evidence<-expectedResult$evidence
  result<-expectedResult$result
  nullresult<-expectedResult$nullresult
  
  if (effect$world$worldOn) {
    r<-getNulls(result)
    result<-r$analysis
    nullresult<-r$nullanalysis
  }
  
  if (is.null(IV2))    whichEffects<-"Main 1"
  else {
    whichEffects<-whichEffect
    if (whichEffect=="All" && !evidence$rInteractionOn) whichEffect<-"Mains"
    if (whichEffect=="All")   {whichEffects<-c("Main 1","Main 2","Interaction")}
    if (whichEffect=="Mains") {whichEffects<-c("Main 1","Main 2")}
    if (whichEffect=="rIV") {whichEffects<-"Main 1"}
    if (whichEffect=="rIV2") {whichEffects<-"Main 2"}
    if (whichEffect=="rIVIV2DV") {whichEffects<-"Interaction"}
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
           "DV"=     {pars<-c("dv.mn","dv.sd","dv.sk","dv.kt")},
           "Residuals"=     {pars<-c("rs.mn","rs.sd","rs.sk","rs.kt")},
           { pars<-strsplit(showType,";")[[1]]
           }
    )
  } else pars<-showType
  
  if (is.null(IV2) || effectType!="all") {nc=4+length(pars)}
  else { nc=4+length(pars)*3 }
  
  if (is.element(showType,c("NHST","Hits","Misses"))) {nc=4}
  nc<-nc+1
  
  # header
  if (is.element(showType,c("NHST","Hits","Misses")) && sum(!is.na(nullresult$rIV))>0) {
    outputText<-c("!TExpected  ",paste("nsims = ",format(sum(!is.na(result$rIV))),"+",format(sum(!is.na(nullresult$rIV))),sep=""),rep("",nc-2))
  } else {
    outputText<-c("!TExpected  ",paste("nsims = ",format(sum(!is.na(result$rIV))+sum(!is.na(nullresult$rIV))),sep=""),rep("",nc-2))
  }
  outputText<-c(outputText,rep("",nc))
  
  effectTypes<-1
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
              effectTypes<-3
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
  if (is.element(showType,c("NHST","tDR","Hits","Misses"))) 
    outputText1<-c("!H ","!H!CErrors:","I","II","")
  else {
    
    if (!is.null(IV2)){
      if (effectTypes==1) outputText<-c(outputText,"!H!C ","!C ",effectType,rep(" ",nc-3))
      else outputText<-c(outputText,"!H!C ","!C ","direct"," "," ","unique"," "," ","total"," "," ")
    }
    
    outputText1<-c()
    for (par in pars) {
      if (braw.env$RZ=="z") {
        switch(par,
               "rs"={par="zs"},
               "rp"={par="zp"},
               "ro"={par="zo"},
               "re"={par="ze"},
               {par=par}
        )
      }
      par<-gsub("^([rz]{1})([spoe]{1})$","\\1\\[\\2\\]",par)
      if (!is.na(par))
        outputText1<-c(outputText1,par)
      else 
        outputText1<-c(outputText1," ")
    }
    outputText1<-c(outputText1," ")
    outputText1<-rep(outputText1,effectTypes)
    outputText1<-c("!H "," ",outputText1,rep("",nc-length(outputText1)-2))
  }
  outputText<-c(outputText,outputText1)
  for (whichEffect in whichEffects)  {
    
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
        e1=paste0(brawFormat(mean(nullSig)*100,digits=1),"%")
        e2=paste0(brawFormat(mean(!resSig)*100,digits=1),"%")
      } else {
        e1=paste0(brawFormat(sum(nullSigW)/length(nullSig)*100,digits=1),"%")
        e2=paste0(brawFormat(sum(resSigW)/length(resSig)*100,digits=1),"%")
      }
      
      if (result$effect$world$worldOn) {
        nr<-(length(nullresult$pIV)+length(result$pIV))
        if (braw.env$STMethod=="NHST") {
          e1a<-paste0("!j",brawFormat(sum(nullSig)/nr*100,digits=1),"%")
          e2a<-paste0("!j",brawFormat(sum(!resSig)/nr*100,digits=1),"%")
          outputText<-c(outputText," ","All",e1a,e2a," ")
          
          e1=paste0("!j",brawFormat(mean(nullSig)*100,digits=1),"%")
          e2=paste0("!j",brawFormat(mean(!resSig)*100,digits=1),"%")
          outputText<-c(outputText," ","r=0",e1," "," ")
          outputText<-c(outputText," ",paste0("r","\u2260","0")," ",e2," ")
          
          e1b=paste0("!j",brawFormat((sum(nullSig)+sum(!resSig))/nr*100,digits=1),"%")
          e2b=paste0("!j",brawFormat((sum(!nullSig)+sum(resSig))/nr*100,digits=1),"%")
          e1c=paste0("(",brawFormat((sum(nullSig)+sum(resSig))/nr*100,digits=1),"%)")
          e2c=paste0("(",brawFormat((sum(!nullSig)+sum(!resSig))/nr*100,digits=1),"%)")
          
          e1n=paste0("!j",brawFormat(sum(nullSig)/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
          e1p=paste0("!j",brawFormat(sum(resSig)/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
          e2n=paste0("!j",brawFormat(sum(!nullSig)/(sum(!nullSig)+sum(!resSig))*100,digits=1),"%")
          e2p=paste0("!j",brawFormat(sum(!resSig)/(sum(!nullSig)+sum(!resSig))*100,digits=1),"%")
        } else {
          e1a<-paste0("!j",brawFormat((sum(nullSigW))/nr*100,digits=1),"%")
          e2a<-paste0("!j",brawFormat((sum(resSigW))/nr*100,digits=1),"%")
          outputText<-c(outputText,"","!jAll",e1a,e2a," ")
          
          e1=paste0("!j",brawFormat(sum(nullSigW)/length(nullSig)*100,digits=1),"%")
          e2=paste0("!j",brawFormat(sum(resSigW)/length(resSig)*100,digits=1),"%")
          outputText<-c(outputText,"","!jr=0",e1," "," ")
          outputText<-c(outputText,"",paste0("!jr","\u2260","0")," ",e2," ")
          e1b=paste0("!j",brawFormat((sum(nullSigW)+sum(resSigW))/nr*100,digits=1),"%")
          e2b=paste0("!j",brawFormat((sum(nullSigC)+sum(resSigC))/nr*100,digits=1),"%")
          e1c=paste0("(",brawFormat((sum(nullSig)+sum(resSig))/nr*100,digits=1),"%)")
          
          e1n=paste0("!j",brawFormat((sum(nullSigW)+sum(resSigW))/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
          e1p=paste0("!j",brawFormat((sum(nullSigC)+sum(resSigC))/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
        }
        # ea=paste0("Combined: ",brawFormat((sum(nullSig)+sum(!resSig))/nr*100,digits=braw.env$report_precision),"%")
        outputText<-c(outputText,"","","","","")
        outputText<-c(outputText,"!H ","!H!COutcomes:","False","Valid","")
        
        outputText<-c(outputText," ","All:",e1b,e2b,"")
        outputText<-c(outputText," ",paste0("Sig ",e1c,":"),e1n,e1p," ")
        
        if (braw.env$STMethod=="NHST") {
          outputText<-c(outputText," ",paste0("Not Sig ",":"),e2p,e2n," ")
        }
      } else {
        outputText<-c(outputText," "," ",e1,e2," ","")
      }
      
    }else{
      
      ot1<-c()
      ot2<-c()
      ot3<-c()
      ot4<-c()
      ot5<-c()
      ot6<-c()
      
      for (i in 1:effectTypes) {
        switch(whichEffect,
               "Main 1"=off<-0,
               "Main 2"=off<-effectTypes,
               "Interaction"=off<-effectTypes*2,
        )
        r<-rs[,i+off]
        p<-ps[,i+off]
        
        for (j in 1:length(pars)) {
          if (i==1 && j==1) {
            ot1<-c(ot1,"","mean ")
            ot2<-c(ot2,"","sd ")
            ot3<-c(ot3,"","median ")
            ot4<-c(ot4,"","iqr ")
            ot5<-c(ot5,"","quant75 ")
            ot6<-c(ot6,"","quant25 ")
          } 
          if (i>1 && j==1) {
            ot1<-c(ot1,"")
            ot2<-c(ot2,"")
            ot3<-c(ot3,"")
            ot4<-c(ot4,"")
            ot5<-c(ot5,"")
            ot6<-c(ot6,"")
          }
          switch (pars[j],
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
                  "wp"={a<-rn2w(result$rpIV,result$nval)},
                  "ci1"={a<-r2ci(r,result$nval[1],-1)},
                  "ci2"={a<-r2ci(r,result$nval[1],+1)},
                  "iv.mn"={a<-result$iv.mn},
                  "iv.sd"={a<-result$iv.sd},
                  "iv.sk"={a<-result$iv.sk},
                  "iv.kt"={a<-result$iv.kt},
                  "dv.mn"={a<-result$dv.mn},
                  "dv.sd"={a<-result$dv.sd},
                  "dv.sk"={a<-result$dv.sk},
                  "dv.kt"={a<-result$dv.kt},
                  "rs.mn"={a<-result$rs.mn},
                  "rs.sd"={a<-result$rs.sd},
                  "rs.sk"={a<-result$rs.sk},
                  "rs.kt"={a<-result$rs.kt}
          )
          ot1<-c(ot1,
                 paste0("!j",brawFormat(mean(a,na.rm=TRUE),digits=braw.env$report_precision))
          )
          ot2<-c(ot2,
                 paste0("!j",brawFormat(sd(a,na.rm=TRUE),digits=braw.env$report_precision))
          )
          ot3<-c(ot3,
                 paste0("!j",brawFormat(quantile(a,0.5,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision))
          )
          ot4<-c(ot4,
                 paste0("!j",brawFormat(IQR(a,na.rm=TRUE),digits=braw.env$report_precision))
          )
          ot5<-c(ot5,
                 paste0("!j",brawFormat(quantile(a,0.75,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision))
          )
          ot6<-c(ot6,
                 paste0("!j",brawFormat(quantile(a,0.25,na.rm=TRUE,names=FALSE),digits=braw.env$report_precision))
          )
        }
      }
      
      if (reportQuants) ot4[1]<-paste0("\b",whichEffect)
      else              ot5[3]<-paste0("\b",whichEffect)
      
      if (reportMeans) { outputText<-c(outputText,ot1,rep(" ",nc-length(ot1)),
                                       ot2,rep(" ",nc-length(ot2)))
      } else {
        if (reportQuants) outputText<-c(outputText,ot5,rep(" ",nc-length(ot5)))
        outputText<-c(outputText,ot3,rep(" ",nc-length(ot3)))
        if (reportQuants) outputText<-c(outputText,ot6,rep(" ",nc-length(ot6)))
        else              outputText<-c(outputText,ot4,rep(" ",nc-length(ot4)))
      }
      
      if (pars[1]=="p" || pars[2]=="p") {
        if (is.null(IV2)) {
          outputText<-c(outputText,rep("",nc),
                        paste0("!j\bp(sig) = ",brawFormat(mean(p<braw.env$alphaSig)*100,digits=1),"%"),rep(" ",nc-1))
        }
      }
    }
  }
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
