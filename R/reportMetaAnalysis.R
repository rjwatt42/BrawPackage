
reportMetaAnalysis<-function(metaResult=braw.res$metaResult){
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  nc<-6

    # header
  outputText<-c(paste0("\bMeta Analysis"," - ",metaResult$metaAnalysis$analysisType),paste("nstudies=",brawFormat(metaResult$metaAnalysis$nstudies)),paste("nsims=",brawFormat(length(metaResult$bestDist)),sep=""),rep("",nc-3))
  outputText<-c(outputText,rep("",nc))
  
  if (metaResult$count==1) {
    if (is.element(metaResult$metaAnalysis$analysisType,c("fixed","random"))) {
      outputText<-c(outputText,"!H!C","\bDistr"," ","\b\u03bb","\b\u00b1","\bllk")
      if (metaResult$hypothesis$effect$world$worldOn)
      outputText<-c(outputText,"Actual",metaResult$hypothesis$effect$world$populationPDF," ",brawFormat(metaResult$hypothesis$effect$world$populationPDFk,digits=3),brawFormat(metaResult$hypothesis$effect$world$populationNullp,digits=3),"")
      else
        outputText<-c(outputText,"Actual","Single"," ",brawFormat(metaResult$hypothesis$effect$rIV,digits=3),brawFormat(metaResult$hypothesis$effect$rSD,digits=3),"")
      outputText<-c(outputText,"Estimate",metaResult$metaAnalysis$analysisType," ",
                    brawFormat(mean(metaResult$bestK),digits=3),
                    brawFormat(mean(metaResult$bestNull),digits=3),
                    brawFormat(mean(metaResult$bestS),digits=3))
    } else {
      outputText<-c(outputText,"!H!C","\bDistr","","\b\u03bb","\bp(0)","\bllk")
      outputText<-c(outputText,"Actual",metaResult$hypothesis$effect$world$populationPDF,"",brawFormat(metaResult$hypothesis$effect$world$populationPDFk,digits=3),brawFormat(metaResult$hypothesis$effect$world$populationNullp,digits=3),"")
      outputText<-c(outputText,"Best",metaResult$bestDist," ",brawFormat(mean(metaResult$bestK),digits=3),brawFormat(mean(metaResult$bestNull),digits=3),brawFormat(mean(metaResult$bestS),digits=3))
      outputText<-c(outputText,rep(" ",nc))
      if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
        outputText<-c(outputText,"Estimated","Single"," ",
                      paste0(brawFormat(mean(metaResult$single$Kmax),digits=3)),
                      paste0(brawFormat(mean(metaResult$single$Nullmax),digits=3)),
                      paste0(brawFormat(mean(metaResult$single$Smax),digits=3))
        )
      }
      if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
        outputText<-c(outputText," ","Gauss"," ",
                      paste0(brawFormat(mean(metaResult$gauss$Kmax),digits=3)),
                      paste0(brawFormat(mean(metaResult$gauss$Nullmax),digits=3)),
                      paste0(brawFormat(mean(metaResult$gauss$Smax),digits=3))
        )
      }
      if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
        outputText<-c(outputText," ","Exp"," ",
                      paste0(brawFormat(mean(metaResult$exp$Kmax),digits=3)),
                      paste0(brawFormat(mean(metaResult$exp$Nullmax),digits=3)),
                      paste0(brawFormat(mean(metaResult$exp$Smax),digits=3))
        )
      }
    }
  } else {
    if (is.element(metaResult$metaAnalysis$analysisType,c("fixed"))) {
      outputText<-c(outputText,"Estimate"," ",brawFormat(mean(metaResult$bestK),digits=3),brawFormat(mean(metaResult$bestNull),digits=3),brawFormat(mean(metaResult$bestS),digits=3))
      outputText<-c(outputText,rep(" ",nc))
    } else {
      n1<-sum(metaResult$bestDist=="Single")
      n2<-sum(metaResult$bestDist=="Gauss")
      n3<-sum(metaResult$bestDist=="Exp")
      use<-which.max(c(n1,n2,n3))
      bestD<-c("Single","Gauss","Exp")[use]
      outputText<-c(outputText,"Best",bestD,paste0(sum(metaResult$bestDist==bestD),"/",length(metaResult$bestDist)),brawFormat(mean(metaResult$bestK),digits=3),brawFormat(mean(metaResult$bestNull),digits=3),brawFormat(mean(metaResult$bestS),digits=3))
      outputText<-c(outputText,rep(" ",nc))
    }
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"Estimated","Single",brawFormat(n1),
                  paste0(brawFormat(mean(metaResult$single$Kmax),digits=3),"\u00B1",brawFormat(std(metaResult$single$Kmax),digits=2)),
                  paste0(brawFormat(mean(metaResult$single$Nullmax),digits=3),"\u00B1",brawFormat(std(metaResult$single$Nullmax),digits=2)),
                  paste0(brawFormat(mean(metaResult$single$Smax),digits=3),"\u00B1",brawFormat(std(metaResult$single$Smax),digits=2))
                  )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
        outputText<-c(outputText," ","Gauss",brawFormat(n2),
                  paste0(brawFormat(mean(metaResult$gauss$Kmax),digits=3),"\u00B1",brawFormat(std(metaResult$gauss$Kmax),digits=2)),
                  paste0(brawFormat(mean(metaResult$gauss$Nullmax),digits=3),"\u00B1",brawFormat(std(metaResult$gauss$Nullmax),digits=2)),
                  paste0(brawFormat(mean(metaResult$gauss$Smax),digits=3),"\u00B1",brawFormat(std(metaResult$gauss$Smax),digits=2))
                  )
      }
    if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
      outputText<-c(outputText," ","Exp",brawFormat(n3),
                  paste0(brawFormat(mean(metaResult$exp$Kmax),digits=3),"\u00B1",brawFormat(std(metaResult$exp$Kmax),digits=2)),
                  paste0(brawFormat(mean(metaResult$exp$Nullmax),digits=3),"\u00B1",brawFormat(std(metaResult$exp$Nullmax),digits=2)),
                  paste0(brawFormat(mean(metaResult$exp$Smax),digits=3),"\u00B1",brawFormat(std(metaResult$exp$Smax),digits=2))
                  )
    }
  }

  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
