#' report a simulated metaAnalysis sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportMetaSingle(metaResult=doMetaAnalysis(),reportStats="Medians")
#' @export
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
    switch(braw.env$RZ,
           "r"={cvt<-function(x){x}},
           "z"={cvt<-function(x){atanh(x)}},
    )
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={
             outputText<-c(outputText,"!H","!C",paste0(braw.env$RZ,"[m]"),"bias[m]","llk"," ")
             outputText<-c(outputText,"Actual"," ",brawFormat(cvt(metaResult$hypothesis$effect$rIV),digits=3),brawFormat(metaResult$metaAnalysis$sourceBias,digits=3)," "," ")
             outputText<-c(outputText,"Estimate"," ",brawFormat(cvt(metaResult$fixed$param1),digits=3),brawFormat(metaResult$fixed$param3,digits=3),brawFormat(metaResult$fixed$Smax,digits=3)," ")
           },
           "random"={
             outputText<-c(outputText,"!H"," ",paste0(braw.env$RZ,"[m]"),paste0("sd(",braw.env$RZ,")[m]"),"bias[m]","llk")
             outputText<-c(outputText,"Actual"," ",brawFormat(cvt(metaResult$hypothesis$effect$rIV),digits=3),brawFormat(metaResult$hypothesis$effect$rSD,digits=3),brawFormat(metaResult$metaAnalysis$sourceBias,digits=3)," ")
             outputText<-c(outputText,"Estimate"," ",brawFormat(cvt(metaResult$random$param1),digits=3),brawFormat(cvt(metaResult$random$param2),digits=3),brawFormat(metaResult$random$param3,digits=3),brawFormat(metaResult$random$Smax,digits=3))
           }
    )
  } else {
    outputText<-c(outputText,"!H!C","\bDistr","","\b\u03bb","\bp(0)","\bllk")
    outputText<-c(outputText,"Actual",metaResult$hypothesis$effect$world$PDF,"",brawFormat(metaResult$hypothesis$effect$world$PDFk,digits=3),brawFormat(metaResult$hypothesis$effect$world$pRPlus,digits=3),"")
    outputText<-c(outputText,"Best",metaResult$best$dist," ",brawFormat(funcCT(metaResult$best$param1),digits=3),brawFormat(funcCT(metaResult$best$param2),digits=3),brawFormat(funcCT(metaResult$best$S),digits=3))
    outputText<-c(outputText,rep(" ",nc))
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"Estimated","Single"," ",
                    paste0(brawFormat(funcCT(metaResult$single$param1),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$single$param2),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$single$Smax),digits=3))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Gauss"," ",
                    paste0(brawFormat(funcCT(metaResult$gauss$param1),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$gauss$param2),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$gauss$Smax),digits=3))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Exp"," ",
                    paste0(brawFormat(funcCT(metaResult$exp$param1),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$exp$param2),digits=3)),
                    paste0(brawFormat(funcCT(metaResult$exp$Smax),digits=3))
      )
    }
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}


#' report a multiple metaAnalysis samples
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportMetaMultiple(metaResult=doMetaMultiple(),reportStats="Medians")
#' @export
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
    switch(braw.env$RZ,
           "r"={cvt<-function(x){x}},
           "z"={cvt<-function(x){atanh(x)}},
    )
    switch(metaResult$metaAnalysis$analysisType,
           "fixed"={
             outputText<-c(outputText,"!H","!C",paste0(braw.env$RZ,"[m]"),"bias[m]","llk"," ")
             outputText<-c(outputText,"Actual"," ",brawFormat(cvt(metaResult$effect$rIV),digits=3),brawFormat(metaResult$metaAnalysis$sourceBias,digits=3)," "," ")
             outputText<-c(outputText,"Estimate",lbCT,brawFormat(funcCT(cvt(metaResult$fixed$param1)),digits=3),brawFormat(funcCT(metaResult$fixed$param3),digits=3),brawFormat(funcCT(metaResult$fixed$Smax),digits=3)," ")
             outputText<-c(outputText,"",lbDP,brawFormat(funcDP(metaResult$fixed$param1),digits=3),brawFormat(funcDP(metaResult$fixed$param2),digits=3),brawFormat(funcDP(metaResult$fixed$Smax),digits=3)," ")
           },
           "random"={
             outputText<-c(outputText,"!H"," ",paste0(braw.env$RZ,"[m]"),paste0("sd(",braw.env$RZ,")[m]"),"bias[m]","llk")
             outputText<-c(outputText,"Actual"," ",brawFormat(cvt(metaResult$hypothesis$effect$rIV),digits=3),brawFormat(metaResult$hypothesis$effect$rSD,digits=3),brawFormat(metaResult$metaAnalysis$sourceBias,digits=3)," ")
             outputText<-c(outputText,"Estimate",lbCT,brawFormat(funcCT(cvt(metaResult$random$param1)),digits=3),brawFormat(funcCT(cvt(metaResult$random$param2)),digits=3),brawFormat(funcCT(metaResult$random$param3),digits=3),brawFormat(funcCT(metaResult$random$Smax),digits=3))
             outputText<-c(outputText,"",lbDP,brawFormat(funcDP(metaResult$random$param1),digits=3),brawFormat(funcDP(metaResult$random$param2),digits=3),brawFormat(funcDP(metaResult$random$param3),digits=3),brawFormat(funcDP(metaResult$random$Smax),digits=3))
           }
    )
  } else {
    outputText<-c(outputText,"!H","!C","",braw.env$Llabel,"p[null]","log(lk)")
    n1<-sum(metaResult$best$dist=="Single")
    n2<-sum(metaResult$best$dist=="Gauss")
    n3<-sum(metaResult$best$dist=="Exp")
    n4<-sum(metaResult$best$dist=="Gamma")
    n5<-sum(metaResult$best$dist=="GenExp")
    use<-which.max(c(n1,n2,n3,n4,n5))
    bestD<-c("Single","Gauss","Exp","Gamma","GenExp")[use]
    outputText<-c(outputText,"Best",bestD,paste0(sum(metaResult$best$dist==bestD),"/",length(metaResult$best$dist)),brawFormat(funcCT(metaResult$best$param1),digits=3),brawFormat(funcCT(metaResult$best$param2),digits=3),brawFormat(funcCT(metaResult$best$S),digits=3))
    outputText<-c(outputText,rep(" ",nc))
    
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"Estimated","Single",brawFormat(n1),
                    paste0(brawFormat(funcCT(metaResult$single$param1),digits=3),"\u00B1",brawFormat(funcDP(metaResult$single$param1),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$single$param2),digits=3),"\u00B1",brawFormat(funcDP(metaResult$single$param2),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$single$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$single$Smax),digits=2))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Gauss",brawFormat(n2),
                    paste0(brawFormat(funcCT(metaResult$gauss$param1),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gauss$param1),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$gauss$param2),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gauss$param2),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$gauss$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gauss$Smax),digits=2))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Exp",brawFormat(n3),
                    paste0(brawFormat(funcCT(metaResult$exp$param1),digits=3),"\u00B1",brawFormat(funcDP(metaResult$exp$param1),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$exp$param2),digits=3),"\u00B1",brawFormat(funcDP(metaResult$exp$param2),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$exp$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$exp$Smax),digits=2))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gamma" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeGamma)) {
      outputText<-c(outputText,"Estimated","Gamma",brawFormat(n4),
                    paste0(brawFormat(funcCT(metaResult$gamma$param1),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gamma$param1),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$gamma$param2),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gamma$param2),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$gamma$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$gamma$Smax),digits=2))
      )
    }
    if (metaResult$metaAnalysis$modelPDF=="GenExp" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeGenExp)) {
      outputText<-c(outputText,"Estimated","GenExp",brawFormat(n4),
                    paste0(brawFormat(funcCT(metaResult$genexp$param1),digits=3),"\u00B1",brawFormat(funcDP(metaResult$genexp$param1),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$genexp$param2),digits=3),"\u00B1",brawFormat(funcDP(metaResult$genexp$param2),digits=2)),
                    paste0(brawFormat(funcCT(metaResult$genexp$Smax),digits=3),"\u00B1",brawFormat(funcDP(metaResult$genexp$Smax),digits=2))
      )
    }
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
