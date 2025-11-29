#' report the system
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportSystem(hypothesis,design)
#' @export
reportSystem<-function(hypothesis=braw.def$hypothesis,design=braw.def$design){
  
  IV<-hypothesis$IV
  IV2<-hypothesis$IV2
  DV<-hypothesis$DV
  
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  
  nc=5
  
  outputText<-c("!THypothesis:",rep(" ",nc-1))
  outputText<-c(outputText,
                "!H!CVariables",rep(" ",nc-1)
  )
  switch (DV$type,
          "Interval"={ DVtype<-paste0("Interval(",DV$mu,",",DV$sd,")") },
          "Ordinal"={ DVtype<-paste0("Ordinal(",DV$median,",",DV$iqr,")") },
          "Categorical"={ DVtype<-paste0("Categorical(",DV$ncats,")") }
  )
  outputText<-c(outputText,
                "!jDV",paste0('"',DV$name,'"'),DVtype,rep(" ",nc-3)
  )
  switch (IV$type,
          "Interval"={ IVtype<-paste0("Interval(",IV$mu,",",IV$sd,")") },
          "Ordinal"={ IVtype<-paste0("Ordinal(",IV$median,",",IV$iqr,")") },
          "Categorical"={ IVtype<-paste0("Categorical(",IV$ncats,")") }
  )
  outputText<-c(outputText,
                "!jIV",paste0('"',IV$name,'"'),IVtype,rep(" ",nc-3)
  )
  if (no_ivs>1) {
    switch (IV2$type,
            "Interval"={ IV2type<-paste0("Interval(",IV2$mu,",",IV2$sd,")") },
            "Ordinal"={ IV2type<-paste0("Ordinal(",IV2$median,",",IV2$iqr,")") },
            "Categorical"={ IV2type<-paste0("Categorical(",IV2$ncats,")") }
    )
    outputText<-c(outputText,
                  "!jIV2",paste0('"',IV2$name,'"'),IV2type,rep(" ",nc-3)
    )
  }
  
  outputText<-c(outputText,
                "!H!CEffects",rep(" ",nc-1)
  )
  outputText<-c(outputText,
                "!jDV~IV",hypothesis$effect$rIV,rep(" ",nc-2)
  )
  if (no_ivs>1) {
    outputText<-c(outputText,
                  "!jDV~IV2",hypothesis$effect$rIV2,rep(" ",nc-2)
    )
    outputText<-c(outputText,
                  "!jDV~IV*IV2",hypothesis$effect$rIVIV2DV,rep(" ",nc-2)
    )
  }
  outputText<-c(outputText,rep("",nc))
  if (!is.null(design)) {
  outputText<-c(outputText,
                "!TDesign:",rep("",nc-1),
                "!H!C","Sample Size","Method","Usage",rep("",nc-4),
                "",paste0("!c",design$sN),paste0("!c",design$sMethod$type),paste0("!c",design$sIV1Use),rep("",nc-4)
  )
  outputText<-c(outputText,rep("",nc))
  }
  
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)
  
}

#' report the system
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportWorld(hypothesis,design)
#' @export
reportWorld<-function(hypothesis=braw.def$hypothesis,plain=FALSE){
  
  world<-hypothesis$effect$world
  nc<-5
  outputText<-c()
  outputText<-c(outputText,
                "!TWorld:",rep("",nc-1),
                "!HPart"," ","Formula",rep("",nc-3)
  )
  
  if (world$PDF=="Uniform") pdf<-paste0(world$PDF,"(",world$RZ,")")
  else pdf<-paste0(world$PDF,"(",world$RZ,"=",brawFormat(world$PDFk,digits=2),")")
  outputText<-c(outputText,
                paste0(braw.env$nonnullTitle),
                paste0("!j",reportNumber(world$pRplus,1,FALSE)),
                paste0("r[p]"," ~ ",tolower(pdf)),
                rep("",nc-3)
                )
  outputText<-c(outputText,
                paste0(braw.env$nullTitle),
                paste0("!j",reportNumber(1-world$pRplus,1,FALSE)),
                paste0("r[p]"," = ",0),
                rep("",nc-3)
  )

  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr,plain=plain)
  
}

#' report a design
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportDesign(design=braw.def$design)
#' @export
reportDesign<-function(design=braw.def$design,plain=FALSE) {

  nc=6
  
  outputText<-c()
  secondRow<-rep("",nc*2)
  
  if (design$sMethod$type=="Random") col<-'' else col<-'!r'
  if (design$sCheating=="None") col1<-'' else col1<-'!r'
  if (design$sCheating=="None") cheat<-'-' else cheat<-tolower(design$sCheating)
  if (design$Replication$On) repl<-tolower(design$Replication$Keep) else repl<-'-'
  if (is.null(braw.def$hypothesis$IV2)) usage<-tolower(design$sIV1Use)
  else usage<-paste0("IV1: ",tolower(design$sIV1Use))
  outputText<-c(outputText,
                "!TDesign:",rep("",nc-1),
                "!Hn","Method","Usage","Replication","Cheating",rep("",nc-5),
                paste0("!c",design$sN),
                paste0("!c",col,tolower(design$sMethod$type)),
                paste0("!c",usage),
                paste0("!c",repl),
                paste0("!c",col1,cheat),
                rep("",nc-5)
  )
  if (!is.null(braw.def$hypothesis$IV2))
    secondRow[3]<-paste0("IV2: ",tolower(design$sIV2Use))
                  
  if (is.element(design$sMethod$type,c("Convenience","Cluster","Snowball")) ){
    if (design$sMethodSeverity<1)  sMethodSeverity<-design$sN*design$sMethodSeverity
    else                           sMethodSeverity<-design$sMethodSeverity
    secondRow[2]<-paste0("severity=",brawFormat(design$sMethodSeverity,digits=2))
  }
  if (nchar(secondRow[2])>2) off2<-2+nc else off2<-2
  if (design$sIVRangeOn)  secondRow[off2]<-"!rIV limited"
  if (design$sIV2RangeOn) secondRow[off2]<-"!rIV2 limited"
  
  if (design$sCheating!="None") {
    if (design$sCheating=="Retry")    
      secondRow[5]<-paste0("!j",tolower(design$sCheatingLimit),"=",brawFormat(design$sCheatingBudget+design$sN,digits=2))
    else
      secondRow[5]<-paste0("!j",tolower(design$sCheatingLimit),"=",brawFormat(design$sCheatingBudget,digits=2))
  }
  outputText<-c(outputText,secondRow)
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr,plain=plain)
  
  
}



#' report the system
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportWorldDesign()
#' @export
reportWorldDesign<-function() {
  outputFront<-paste0('<div style="display:inline-block;padding:10px;margin-bottom:20px;">')
  outputBack<-'</div>'
  if (is.null(braw.def$hypothesis$IV2)) rw<-reportWorld(plain=TRUE)
  else {
    graphicsType<-braw.env$graphicsType
    setHTML()
    svgBox(height=150,aspect=1.5,fontScale=1.2)
    rw<-paste0(
      '<div style="display:inline-block;float:left;">',
      showHypothesis(),
      '</div>'
    )
    setBrawEnv("graphicsType",graphicsType)
  }
  paste0(
    outputFront,
    rw,
    reportDesign(plain=TRUE),
    outputBack
  )
}
