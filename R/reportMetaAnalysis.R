
reportMetaAnalysis<-function(metaResult=braw.res$metaResult){
  if (is.null(metaResult)) metaResult<-doMetaAnalysis()
  
  nc<-6

    # header
  outputText<-c("\bMeta Analysis",paste("nstudies=",brawFormat(metaResult$metaAnalysis$nstudies)),paste("nsims=",brawFormat(length(metaResult$bestDist)),sep=""),rep("",nc-3))
  outputText<-c(outputText,rep("",nc))
  
  outputText<-c(outputText," ","\bDistr","","\b\u03bb","\bp(0)","\bS")
  outputText<-c(outputText,"\bActual",metaResult$hypothesis$effect$world$populationPDF,"",brawFormat(metaResult$hypothesis$effect$world$populationPDFk,digits=3),brawFormat(metaResult$hypothesis$effect$world$populationNullp,digits=3),"")
  
  n1<-sum(metaResult$bestDist=="Single")
  n2<-sum(metaResult$bestDist=="Gauss")
  n3<-sum(metaResult$bestDist=="Exp")
  use<-which.max(c(n1,n2,n3))
  bestD<-c("Single","Gauss","Exp")[use]
  outputText<-c(outputText,"\bBest",bestD,paste0(sum(metaResult$bestDist==bestD),"/",length(metaResult$bestDist)),brawFormat(mean(metaResult$bestK),digits=3),brawFormat(mean(metaResult$bestNull),digits=3),brawFormat(mean(metaResult$bestS),digits=3))
  outputText<-c(outputText,rep(" ",nc))
  
  if (metaResult$count==1) {
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"\bEstimated","Single",brawFormat(n1),
                  paste0(brawFormat(mean(metaResult$single$Kmax),digits=3)),
                  paste0(brawFormat(mean(metaResult$single$Nullmax),digits=3)),
                  paste0(brawFormat(mean(metaResult$single$Smax),digits=3))
                  )
    }
    if (metaResult$metaAnalysis$modelPDF=="Gauss" || metaResult$metaAnalysis$modelPDF=="All") {
        outputText<-c(outputText," ","Gauss",brawFormat(n2),
                  paste0(brawFormat(mean(metaResult$gauss$Kmax),digits=3)),
                  paste0(brawFormat(mean(metaResult$gauss$Nullmax),digits=3)),
                  paste0(brawFormat(mean(metaResult$gauss$Smax),digits=3))
                  )
      }
    if (metaResult$metaAnalysis$modelPDF=="Exp" || metaResult$metaAnalysis$modelPDF=="All") {
          outputText<-c(outputText," ","Exp",brawFormat(n3),
                  paste0(brawFormat(mean(metaResult$exp$Kmax),digits=3)),
                  paste0(brawFormat(mean(metaResult$exp$Nullmax),digits=3)),
                  paste0(brawFormat(mean(metaResult$exp$Smax),digits=3))
                  )
        }
  } else {
    if (metaResult$metaAnalysis$modelPDF=="Single" || (metaResult$metaAnalysis$modelPDF=="All" && braw.env$includeSingle)) {
      outputText<-c(outputText,"\bEstimated","Single",brawFormat(n1),
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
