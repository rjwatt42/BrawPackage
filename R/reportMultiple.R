
#' show the estimated population characteristics from multiple simulated sample
#' 
#' @param showType "Basic", "CILimits", "NHST", "Hits" \cr
#'        \emph{ or one or two of:} \cr
#'                   "rs","p","ci1","ci2", "rp","n" \cr 
#'                   "ws","wp","nw", ro","po"
#' @return ggplot2 object - and printed
#' @export
reportMultiple<-function(multipleResult=braw.res$multiple,showType="Basic",
                         whichEffect="All",effectType="all",reportStats="Medians"){
  
  if (is.null(multipleResult)) multipleResult=doMultiple(autoShow=FALSE)
  if (!multipleResult$hypothesis$effect$world$worldOn && is.element(showType[1],c("NHST","Inference","Source","Hits","Misses"))) {
    if (multipleResult$nullcount<multipleResult$count) {
      multipleResult<-doMultiple(0,multipleResult,doingNull=TRUE)
    }
  }
  
    reportMeans<-(reportStats=="Means")
    reportQuants<-FALSE
    
    IV<-multipleResult$hypothesis$IV
    IV2<-multipleResult$hypothesis$IV2
    DV<-multipleResult$hypothesis$DV
    effect<-multipleResult$hypothesis$effect
    evidence<-multipleResult$evidence
    result<-multipleResult$result
    nullresult<-multipleResult$nullresult
    
    if (effect$world$worldOn) {
      r<-getNulls(result)
      result<-r$analysis
      nullresult<-r$nullanalysis
    }
    
    if (is.null(IV2) || showType=="SEM")    whichEffects<-"Main 1"
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
             "Single"=    {pars<-c("rs")},
             "Basic"=     {pars<-c("rs","p")},
             "p(sig)"=    {pars<-c("p")},
             "Power"=     {pars<-c("ws","wp")},
             "2D"=        {pars<-c("rs","p")},
             "CILimits"=  {pars<-c("ci1","ci2")},
             "rse"=       {pars<-c("e2p","e1p")},
             "NHST"=      {pars<-c("e2p","e1p")},
             "Inference"= {pars<-c("e1a","e2a")},
             "Source"=    {pars<-c("e1a","e2a")},
             "Hits"=      {pars<-c("e1a","e2a")},
             "Misses"=    {pars<-c("e1b","e2b")},
             "DV"=        {pars<-c("dv.mn","dv.sd","dv.sk","dv.kt")},
             "Residuals"= {pars<-c("er.mn","er.sd","er.sk","er.kt")},
             { pars<-strsplit(showType,";")[[1]]
             }
      )
    } else pars<-showType
    
    if (is.null(IV2) || effectType!="all") {nc=4+length(pars)}
    else { nc=4+length(pars)*9 }
    
    if (is.element(showType,c("SEM"))) {nc=6}
    if (is.element(showType,c("NHST","Hits","Misses","Inference"))) {
      if (braw.env$STMethod=="NHST") nc=4 else nc=5
      }
    nc<-nc+1
    
    # header
    if (is.element(showType[1],c("NHST","Hits","Misses","Inference","SEM")) && sum(!is.na(nullresult$rIV))>0) {
      outputText<-c("!TMultiple  ",paste("nsims = ",format(sum(!is.na(result$rIV))),"+",format(sum(!is.na(nullresult$rIV))),sep=""),rep("",nc-2))
    } else {
      outputText<-c("!TMultiple  ",paste("nsims = ",format(sum(!is.na(result$rIV))+sum(!is.na(nullresult$rIV))),sep=""),rep("",nc-2))
    }
    outputText<-c(outputText,rep("",nc))
    
    if (nullresult$count>0) {
      result$rp[result$rp==0]<-0.00000000001
      result$rIV<-rbind(result$rIV,nullresult$rIV)
      result$rp<-rbind(result$rp,nullresult$rp)
      result$pIV<-rbind(result$pIV,nullresult$pIV)
      result$nval<-rbind(result$nval,nullresult$nval)
      result$df1<-rbind(result$df1,nullresult$df1)
    }
    
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
    if (is.element(showType,c("NHST","Hits","Misses","Inference"))) {
      if (braw.env$STMethod=="NHST")
        outputText1<-c("!H ","!H!CSources:","sig","ns",rep("",nc-4))
      else
        outputText1<-c("!H ","!H!CSources:","sig","ns","err",rep("",nc-5))
    }
    else 
      if (is.element(showType,c("SEM"))) {
        use<-evidence$useAIC 
        outputText1<-c("","!HResult","!H!C%","",paste0("mean(",use,")"),
                                         paste0("sd(",use,")"),rep("",nc-6))
        outputText1[2]<-"!HModel"
      } else {
      if (!is.null(IV2)){
        
        if (effectTypes==1) headerText<-c("!H!C ","!C ",effectType)
        else headerText<-c("!H!C ","!C ","direct",rep(" ",length(pars)),"unique",rep(" ",length(pars)),"total",rep(" ",length(pars)))
        outputText<-c(outputText,headerText,rep(" ",nc-length(headerText)))
        }
      
      outputText1<-c()
      for (par in pars) {
        if (is.element(par,c("rs","rp","re","ro","metaRiv","metaRsd")))
          switch(braw.env$RZ,
                 "r"={},
                 "z"={par<-gsub("^r","z",par)}
                 )
        par<-gsub("^([rz]{1})([spoe]{1})$","\\1\\[\\2\\]",par)
        if (par=="llknull") par<-"llr[+]"
        if (par=="AIC") par<-"diff(AIC)"
        if (par=="e1a") par<-"sig"
        if (par=="e2a") par<-"ns"
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
      
      if (is.element(showType,c("NHST","Hits","Misses","Inference","Source"))){
        nullSig<-isSignificant(braw.env$STMethod,nullresult$pIV,nullresult$rIV,nullresult$nval,nullresult$df1,nullresult$evidence)
        resSig<-isSignificant(braw.env$STMethod,result$pIV,result$rIV,result$nval,result$df1,result$evidence)
        if (braw.env$STMethod=="dLLR") {
          nullSigW<-nullSig>0
          nullSigN<-nullSig==0
          nullSigC<-nullSig<0
          resSigW<-resSig<0
          resSigN<-resSig==0
          resSigC<-resSig>0
        }
        
        if (braw.env$STMethod=="NHST") {
          e1=paste0(brawFormat(mean(nullSig)*100,digits=1),"%")
          e2=paste0(brawFormat(mean(!resSig)*100,digits=1),"%")
        } else {
          e1=paste0(brawFormat(sum(nullSigW)/length(nullSig)*100,digits=1),"%")
          e2=paste0(brawFormat(sum(resSigW)/length(resSig)*100,digits=1),"%")
        }
        
        if (effect$world$worldOn) {
          nr<-(length(nullresult$pIV)+length(result$pIV))
          if (braw.env$STMethod=="NHST") {
            e1e<-paste0("!j",brawFormat(sum(nullSig)/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
            e2e<-paste0("!j",brawFormat(sum(!resSig)/(sum(!nullSig)+sum(!resSig))*100,digits=1),"%")
            e1a<-paste0("!j",brawFormat((sum(nullSig)+sum(resSig))/nr*100,digits=1),"%")
            e2a<-paste0("!j",brawFormat((sum(!nullSig)+sum(!resSig))/nr*100,digits=1),"%")
            outputText<-c(outputText," ","!jAll:",e1a,e2a,rep("",nc-4))
            
            e1=paste0("!j",brawFormat(mean(nullSig)*100,digits=1),"%")
            e2=paste0("!j",brawFormat(mean(!resSig)*100,digits=1),"%")
            e3=paste0("!j",brawFormat(mean(!nullSig)*100,digits=1),"%")
            e4=paste0("!j",brawFormat(mean(resSig)*100,digits=1),"%")
            nn<-paste0("(",brawFormat(length(nullresult$pIV)/nr*100,digits=1),"%)")
            nnn<-paste0("(",brawFormat(length(result$pIV)/nr*100,digits=1),"%)")
            if (result$evidence$minRp!=0) {
              outputText<-c(outputText," ",paste0("!j",braw.env$inactiveTitle," ",nn,":"),e1,e3,rep("",nc-4))
            outputText<-c(outputText," ",paste0("!j",braw.env$activeTitle," ",nnn,":"),e4,e2,rep("",nc-4))
            } else {
              outputText<-c(outputText," ",paste0("!j",braw.env$nullTitle," ",nn,":"),e1,e3,rep("",nc-4))
              outputText<-c(outputText," ",paste0("!j",braw.env$nonnullTitle," ",nnn,":"),e4,e2,rep("",nc-4))
            }
            e1b=paste0("!j",brawFormat((sum(nullSig)+sum(!resSig))/nr*100,digits=1),"%")
            e2b=paste0("!j",brawFormat((sum(!nullSig)+sum(resSig))/nr*100,digits=1),"%")
            e1c=paste0("(",brawFormat((sum(nullSig)+sum(resSig))/nr*100,digits=1),"%)")
            e2c=paste0("(",brawFormat((sum(!nullSig)+sum(!resSig))/nr*100,digits=1),"%)")
            
            e1n=paste0("!j",brawFormat(sum(nullSig)/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
            e1p=paste0("!j",brawFormat(sum(resSig)/(sum(nullSig)+sum(resSig))*100,digits=1),"%")
            e2n=paste0("!j",brawFormat(sum(!nullSig)/(sum(!nullSig)+sum(!resSig))*100,digits=1),"%")
            e2p=paste0("!j",brawFormat(sum(!resSig)/(sum(!nullSig)+sum(!resSig))*100,digits=1),"%")

            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText,"!H ","!H!C!jInferences:","false","correct",rep("",nc-4))
            
            outputText<-c(outputText," ","!jAll:",e1b,e2b,rep("",nc-4))
            outputText<-c(outputText," ",paste0("!jHits ",e1c,":"),e1n,e1p,rep("",nc-4))
            outputText<-c(outputText," ",paste0("!jMisses ",e2c,":"),e2p,e2n,rep("",nc-4))
          } else {
            e1a<-paste0("!j",brawFormat((sum(nullSigC)+sum(resSigC))/nr*100,digits=1),"%")
            e2a<-paste0("!j",brawFormat((sum(nullSigW)+sum(resSigW))/nr*100,digits=1),"%")
            e3a<-paste0("!j",brawFormat((sum(nullSigN)+sum(resSigN))/nr*100,digits=1),"%")
            outputText<-c(outputText,"","!jAll",e1a,e3a,e2a,rep("",nc-5))
            
            e1=paste0("!j",brawFormat(sum(nullSigC)/length(nullSig)*100,digits=1),"%")
            e2=paste0("!j",brawFormat(sum(resSigC)/length(resSig)*100,digits=1),"%")
            e3=paste0("!j",brawFormat(sum(nullSigN)/length(nullSig)*100,digits=1),"%")
            e4=paste0("!j",brawFormat(sum(resSigN)/length(resSig)*100,digits=1),"%")
            e5=paste0("!j",brawFormat(sum(nullSigW)/length(nullSig)*100,digits=1),"%")
            e6=paste0("!j",brawFormat(sum(resSigW)/length(resSig)*100,digits=1),"%")
            if (result$evidence$minRp!=0) {
              outputText<-c(outputText,"",paste0("!j",braw.env$inactiveTitle),e1,e3,e5,rep("",nc-5))
            outputText<-c(outputText,"",paste0("!j",braw.env$activeTitle),e2,e4,e6,rep("",nc-5))
            } else {
              outputText<-c(outputText,"",paste0("!j",braw.env$nullTitle),e1,e3,e5,rep("",nc-5))
              outputText<-c(outputText,"",paste0("!j",braw.env$nonnullTitle),e2,e4,e6,rep("",nc-5))
            }
            e1b=paste0("!j",brawFormat((sum(nullSigW)+sum(resSigW))/nr*100,digits=1),"%")
            e2b=paste0("!j",brawFormat((sum(nullSigC)+sum(resSigC))/nr*100,digits=1),"%")
            e3b=paste0(brawFormat((sum(nullSigN)+sum(resSigN))/nr*100,digits=1),"%")
            e1c=paste0("(",brawFormat((sum(nullSigW)+sum(resSigW)+sum(nullSigC)+sum(resSigC))/nr*100,digits=1),"%)")
            e2c=paste0("(",brawFormat((sum(nullSigW)+sum(resSigC))/nr*100,digits=1),"%)")
            e3c=paste0("(",brawFormat((sum(nullSigC)+sum(resSigW))/nr*100,digits=1),"%)")

            e1n=paste0("!j",brawFormat((sum(nullSigW)+sum(resSigW))/(sum(nullSigW)+sum(resSigW)+sum(nullSigC)+sum(resSigC))*100,digits=1),"%")
            e1p=paste0("!j",brawFormat((sum(nullSigC)+sum(resSigC))/(sum(nullSigW)+sum(resSigW)+sum(nullSigC)+sum(resSigC))*100,digits=1),"%")
            e2n=paste0("!j",brawFormat((sum(nullSigW))/(sum(nullSigW)+sum(resSigC))*100,digits=1),"%")
            e2p=paste0("!j",brawFormat((sum(resSigC))/(sum(nullSigW)+sum(resSigC))*100,digits=1),"%")
            e3n=paste0("!j",brawFormat((sum(resSigW))/(sum(nullSigC)+sum(resSigW))*100,digits=1),"%")
            e3p=paste0("!j",brawFormat((sum(nullSigC))/(sum(nullSigC)+sum(resSigW))*100,digits=1),"%")
            
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText,"!H ","!H!C!jInferences:","correct","missing","false",rep("",nc-5))
            
            outputText<-c(outputText," ","!jAll:",e2b,e3b,e1b,rep("",nc-5))
            outputText<-c(outputText," ",paste0("!jHits ",e1c,":"),e1p,"",e1n,rep("",nc-5))
            outputText<-c(outputText," ",paste0("!jHits+ ",e2c,":"),e2p,"",e2n,rep("",nc-5))
            outputText<-c(outputText," ",paste0("!jHits0 ",e3c,":"),e3p,"",e3n,rep("",nc-5))
          }
          
        } else {
          outputText<-c(outputText," "," ",e1,e2,rep("",nc-4))
        }
        
      } else 
        if (is.element(showType,c("SEM"))) {
          nulls<-abs(result$rp)<=evidence$minRp
          sigs<-isSignificant(braw.env$STMethod,
                              result$pIV,result$rIV,
                              result$nval,result$df1,
                              result$evidence)
          outcomes<-multipleResult$result$sem[,8]
          data<-multipleResult$result$sem[,1:7]
          digits=1
          nbar<-sum(!is.na(data[1,]))
          
          if (!all(nulls) && !all(!nulls)) {
            for (ig in nbar:1) {
              nextLine<-c("",colnames(data)[ig],
                          paste0(brawFormat(100*sum(outcomes[!nulls]==ig)/sum(!nulls | nulls),digits=1),"%"),
                          paste0("(",brawFormat(100*sum(outcomes[!nulls]==ig)/sum(!nulls),digits=1),"%",")"),
                          brawFormat(mean(abs(data[!nulls,ig]),na.rm=TRUE),digits=digits),
                          brawFormat(sd(abs(data[!nulls,ig]),na.rm=TRUE),digits=digits),
                          rep("",nc-6))
              if (ig==nbar) nextLine[1]<-paste0("!jNon-Nulls(",
                                                brawFormat(100*sum(!nulls)/sum(!nulls | nulls),digits=1),"%)",
                                                ": ",nextLine[1])
              outputText<-c(outputText,nextLine)
            }
            for (ig in nbar:1) {
              nextLine<-c("",colnames(data)[ig],
                          paste0(brawFormat(100*sum(outcomes[nulls]==ig)/sum(!nulls | nulls),digits=1),"%"),
                          paste0("(",brawFormat(100*sum(outcomes[nulls]==ig)/sum(nulls),digits=1),"%",")"),
                          brawFormat(mean(abs(data[nulls,ig]),na.rm=TRUE),digits=digits),
                          brawFormat(sd(abs(data[nulls,ig]),na.rm=TRUE),digits=digits),
                          rep("",nc-6))
              if (ig==nbar) nextLine[1]<-paste0("!jNulls(",
                                                brawFormat(100*sum(nulls)/sum(!nulls | nulls),digits=1),"%)",
                                                ": ",nextLine[1])
              outputText<-c(outputText,nextLine)
            }
          } else {
            for (ig in nbar:1) {
              nextLine<-c("",
                          colnames(data)[ig],paste0(brawFormat(100*mean(outcomes==ig),digits=1),"%"),
                          "",
                          brawFormat(mean(data[,ig],na.rm=TRUE),digits=1),
                          brawFormat(sd(data[,ig],na.rm=TRUE),digits=1),
                          rep("",nc-6))
              outputText<-c(outputText,nextLine)
            }
          }
          } else {
        
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
                    "rs"={ a<-r },
                    "p"={ a<-p},
                    "rp"={ a<-result$rpIV },
                    "ro"={ a<-result$roIV },
                    "re"={ a<-result$rIV-result$rpIV},
                    "po"={a<-result$poIV},
                    "llknull"={a<-(-0.5*(result$AIC-result$AICnull))},
                    "AIC"={a<-result$AIC-result$AICnull},
                    "sLLR"={a<-res2llr(result,"sLLR")},
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
                    "er.mn"={a<-result$er.mn},
                    "er.sd"={a<-result$er.sd},
                    "er.sk"={a<-result$er.sk},
                    "er.kt"={a<-result$er.kt}
            )
            if (is.element(pars[j],c("rs","rp","re","ro","metaRiv","metaRsd")))
              switch(braw.env$RZ,
                     "r"={},
                     "z"={a<-atan(a)}
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
        
        if (reportMeans) ot1[1]<-paste0("\b",whichEffect)
        else             ot3[1]<-paste0("\b",whichEffect)
        
        if (reportMeans) { outputText<-c(outputText,ot1,rep(" ",nc-length(ot1)),
                                         ot2,rep(" ",nc-length(ot2)))
        } else {
          if (reportQuants) outputText<-c(outputText,ot5,rep(" ",nc-length(ot5)))
          outputText<-c(outputText,ot3,rep(" ",nc-length(ot3)))
          if (reportQuants) outputText<-c(outputText,ot6,rep(" ",nc-length(ot6)))
          else              outputText<-c(outputText,ot4,rep(" ",nc-length(ot4)))
        }
        
        # if (any(pars=="p")) {
        if (is.null(IV2)) {
          outputText<-c(outputText,rep("",nc),
                        paste0("\bp(sig) = ",brawFormat(sum(p<braw.env$alphaSig,na.rm=TRUE)/sum(!is.na(p))*100,digits=1),"%"),rep(" ",nc-1))
        }
        # }
        if (any(pars=="wp")) {
          if (is.null(IV2)) {
            outputText<-c(outputText,
                          paste0("\bp(w[p]>0.8) = ",
                                 brawFormat(sum(rn2w(result$rpIV,result$nval)>0.8,na.rm=TRUE)/sum(!is.na(result$rpIV))*100,digits=1),"%"),rep(" ",nc-1))
          }
        }
          }
    }
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
