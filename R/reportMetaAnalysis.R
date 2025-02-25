
reportMetaSingle<-function(metaResult=braw.res$metaSingle,reportStats="Medians"){
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  nc<-6
  switch(reportStats,
         "Medians"={
           funcCT<-median
           lbCT<-"median"
           funcDP<-iqr
           lbDP<-"iqr"
         },
         "Means"={
           funcCT<-mean
           lbCT<-"mean"
           funcDP<-std
           lbDP<-"sd"
         })
  # header
  outputText<-c(paste0("\bMeta Analysis"," - ",metaResult$metaAnalysis$analysisType," (nstudies=",brawFormat(metaResult$metaAnalysis$nstudies),")"),rep("",nc-1))
  outputText<-c(outputText,rep("",nc))
  
  if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={
             outputText<-c(outputText,"!H","!C","r[est]","S"," "," ")
             outputText<-c(outputText,"Actual"," ",brawFormat(metaResult$result$rpIV[1],digits=3)," "," "," ")
             outputText<-c(outputText,"Estimate"," ",brawFormat(metaResult$fixed$param1Max,digits=3),brawFormat(metaResult$fixed$Smax,digits=3)," "," ")
           },
           "random"={
             outputText<-c(outputText,"!H"," ","r[est]","sd(r)[est]","S"," ")
             outputText<-c(outputText,"Actual"," ",brawFormat(metaResult$result$rpIV[1],digits=3),brawFormat(metaResult$hypothesis$effect$rSD,digits=3)," "," ")
             outputText<-c(outputText,"Used"," ",brawFormat(metaResult$random$rpIV,digits=3),brawFormat(metaResult$random$rpSD,digits=3)," "," ")
             outputText<-c(outputText,"Estimate"," ",brawFormat(metaResult$random$param1Max,digits=3),brawFormat(metaResult$random$param2Max,digits=3),brawFormat(metaResult$random$Smax,digits=3)," ")
           }
    )
  } else {
    outputText<-c(outputText,"!H!C","\bDistr","","\b\u03bb","\bp(0)","\bllk")
    outputText<-c(outputText,"Actual",metaResult$hypothesis$effect$world$populationPDF,"",brawFormat(metaResult$hypothesis$effect$world$populationPDFk,digits=3),brawFormat(metaResult$hypothesis$effect$world$populationNullp,digits=3),"")
    outputText<-c(outputText,"Best",metaResult$bestDist," ",brawFormat(funcCT(metaResult$bestParam1),digits=3),brawFormat(funcCT(metaResult$bestParam2),digits=3),brawFormat(funcCT(metaResult$bestS),digits=3))
    outputText<-c(outputText,rep(" ",nc))
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"Estimated","Single"," ",
                    paste0(brawFormat(funcCT(metaResult$single$param1Max),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$single$param2Max),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$single$Smax),digits=3))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Gauss"," ",
                    paste0(brawFormat(funcCT(metaResult$gauss$param1Max),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$gauss$param2Max),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$gauss$Smax),digits=3))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Exp"," ",
                    paste0(brawFormat(funcCT(metaResult$exp$param1Max),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$exp$param2Max),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$exp$Smax),digits=3))
      )
    }
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}


reportMetaMultiple<-function(metaResult=braw.res$metaMultiple,reportStats="Medians"){
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  nc<-6
  switch(reportStats,
         "Medians"={
           funcCT<-median
           lbCT<-"median"
           funcDP<-iqr
           lbDP<-"iqr"
         },
         "Means"={
           funcCT<-mean
           lbCT<-"mean"
           funcDP<-std
           lbDP<-"sd"
         })
  # header
  outputText<-c(paste0("\bMeta Analysis"," - ",metaResult$metaAnalysis$analysisType," (nstudies=",brawFormat(metaResult$metaAnalysis$nstudies),")"),paste("nsims=",brawFormat(metaResult$count),sep=""),rep("",nc-2))
  outputText<-c(outputText,rep("",nc))
  
  if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={
             outputText<-c(outputText,"!H","!C","r[est]","S"," "," ")
             outputText<-c(outputText,"Actual"," ",brawFormat(metaResult$hypothesis$effect$rIV,digits=3)," "," "," ")
             outputText<-c(outputText,"Estimate",lbCT,brawFormat(funcCT(metaResult$fixed$param1Max),digits=3),brawFormat(funcCT(metaResult$fixed$Smax),digits=3)," "," ")
             outputText<-c(outputText,"",lbDP,brawFormat(funcDP(metaResult$fixed$param1Max),digits=3),brawFormat(funcDP(metaResult$fixed$Smax),digits=3)," "," ")
           },
           "random"={
             outputText<-c(outputText,"!H"," ","r[est]","sd(r)[est]","S"," ")
             outputText<-c(outputText,"Actual"," ",brawFormat(metaResult$hypothesis$effect$rIV,digits=3),brawFormat(metaResult$hypothesis$effect$rSD,digits=3)," "," ")
             outputText<-c(outputText,"Estimate",lbCT,brawFormat(funcCT(metaResult$random$param1Max),digits=3),brawFormat(funcCT(metaResult$random$param2Max),digits=3),brawFormat(funcCT(metaResult$random$Smax),digits=3)," ")
             outputText<-c(outputText,"",lbDP,brawFormat(funcDP(metaResult$random$param1Max),digits=3),brawFormat(funcDP(metaResult$random$param2Max),digits=3),brawFormat(funcDP(metaResult$random$Smax),digits=3)," ")
           }
    )
  } else {
    n1<-sum(metaResult$bestDist=="Single")
    n2<-sum(metaResult$bestDist=="Gauss")
    n3<-sum(metaResult$bestDist=="Exp")
    use<-which.max(c(n1,n2,n3))
    bestD<-c("Single","Gauss","Exp")[use]
    outputText<-c(outputText,"Best",bestD,paste0(sum(metaResult$bestDist==bestD),"/",length(metaResult$bestDist)),brawFormat(funcCT(metaResult$bestParam1),digits=3),brawFormat(funcCT(metaResult$bestParam2),digits=3),brawFormat(funcCT(metaResult$bestS),digits=3))
    outputText<-c(outputText,rep(" ",nc))
    
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"Estimated","Single",brawFormat(n1),
                    paste0(brawFormat(funcCT(metaResult$single$param1Max),digits=3),"\u00B1",brawFormat(funcDP(metaResult$single$param1Max),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$single$param2Max),digits=3),"\u00B1",brawFormat(funcDP(metaResult$single$param2Max),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$single$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$single$Smax),digits=2))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Gauss",brawFormat(n2),
                    paste0(brawFormat(funcCT(metaResult$gauss$param1Max),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gauss$param1Max),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$gauss$param2Max),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gauss$param2Max),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$gauss$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gauss$Smax),digits=2))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Exp",brawFormat(n3),
                    paste0(brawFormat(funcCT(metaResult$exp$param1Max),digits=3),"\u00B1",brawFormat(funcDP(metaResult$exp$param1Max),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$exp$param2Max),digits=3),"\u00B1",brawFormat(funcDP(metaResult$exp$param2Max),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$exp$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$exp$Smax),digits=2))
      )
    }
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
