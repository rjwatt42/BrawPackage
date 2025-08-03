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
  
  nc=7
  
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
  outputText<-c(outputText,
                "!TDesign:",rep("",nc-1),
                "!H!C","Sample Size","Method","Usage",rep("",nc-4),
                "",paste0("!c",design$sN),paste0("!c",design$sMethod$type),paste0("!c",design$sIV1Use),rep("",nc-4)
  )
  outputText<-c(outputText,rep("",nc))
  
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
                "!H ","Amount","Formula",rep("",nc-3)
  )
  
  outputText<-c(outputText,
                paste0("NonNulls","(",braw.env$nonnullTitle,")"),
                reportNumber(1-world$populationNullp,1,TRUE),
                paste0("r[p]","~",world$populationPDF,"(",world$populationRZ,"=",world$populationPDFk,")"),
                rep("",nc-3)
                )
  outputText<-c(outputText,
                paste0("Nulls","(",braw.env$nullTitle,")"),
                reportNumber(world$populationNullp,1,TRUE),
                paste0("r[p]","=",0),
                rep("",nc-3)
  )

  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr,plain)
  
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
  
  if (design$sMethod$type=="Random") col<-'' else col<-'!r'
  if (design$sCheating=="None") col1<-'' else col1<-'!r'
  outputText<-c(outputText,
                "!TDesign:",rep("",nc-1),
                "!HSample n","Usage","Method","Cheating",rep("",nc-4),
                paste0("!c",design$sN),paste0("!c",design$sIV1Use),
                paste0(col,design$sMethod$type),paste0(col1,design$sCheating),
                rep("",nc-4)
  )
  if (is.element(design$sMethod$type,c("Convenience","Cluster","Snowball")) ){
    if (design$sMethodSeverity<1) 
      sMethodSeverity<-design$sN*design$sMethodSeverity
    else               sMethodSeverity<-design$sMethodSeverity
    n<-design$sN
    nClusts<-n-sMethodSeverity
    outputText<-c(outputText,rep("",2),
                  paste0("seeds=",brawFormat(nClusts,digits=1)),
                  rep("",nc-3))
    switch(design$sMethod$type,
           "Cluster"={
             Cluster_n<-n/nClusts-1
             Contact_n<-0
           },
           "Snowball"={
             Contact_n<-n/nClusts-1
             Cluster_n<-0
           },
           "Convenience"={
             Cluster_n<-sqrt(n/nClusts-1)
             Contact_n<-sqrt(n/nClusts-1)
           })
    
    if (Cluster_n>0) {
      outputText<-c(outputText,rep("",2),
                    paste0("clust_r=",brawFormat(design$sMethod$Cluster_rad,digits=1)),
                    rep("",nc-3))
      outputText<-c(outputText,rep("",2),
                    paste0("clust_n=",brawFormat(Cluster_n,digits=1)),
                    rep("",nc-3))
    }
    if (Contact_n>0) {
      outputText<-c(outputText,rep("",2),
                    paste0("chain_r=",brawFormat(design$sMethod$Contact_rad,digits=1)),
                    rep("",nc-3))
      outputText<-c(outputText,rep("",2),
                    paste0("chain_n=",brawFormat(Contact_n,digits=1)),
                    rep("",nc-3))
    }
  }
   
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr,plain)
  
  
}



#' report the system
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportWorldDesign()
#' @export
reportWorldDesign<-function() {
  fontSize<-0.85
  fontSize<-fontSize*13
  mainStyle<-paste0("font-size:",format(fontSize) ,"px;font-weight:normal;text-align: left;")
  placing<-''
  outputFront<-paste0('<div style="padding:',fontSize,'px;',placing,mainStyle,'">')
  outputBack<-'</div>'
  paste0(
    outputFront,
    reportWorld(),
    reportDesign(),
    outputBack
  )
}