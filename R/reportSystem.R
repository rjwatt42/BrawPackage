#' report a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportSample(sample=doSample())
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
