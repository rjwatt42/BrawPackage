
makeFormula<-function(IV,IV2,DV,design,evidence,analysis,an_vars){

  assign_string = " <<"  
  when_string = "="
  times_string = "x"
  interaction_char = "\u2217"
  
  coeffs<-analysis$coefficients
  
  switch (DV$type,
          "Interval"={
            an_model<-paste(DV$name,assign_string,sep="")
          },
          "Ordinal"={
            an_model<-paste(DV$name,assign_string,sep="")
          },
          "Categorical"={
            an_model<-paste("logit(", DV$name, when_string, DV$cases[2], ") ", assign_string, sep="")
          }
  )
  
  if (coeffs[1]>=0){join<-" "}else{join<-" -"}
  an_model<-paste(an_model, join, brawFormat(abs(coeffs[1]),digits=braw.env$report_precision)   ,sep="")
  
  
  for (i in 2:length(coeffs)){
    if (!is.na(coeffs[i])) {
      if (coeffs[i]>=0){join<-" +"}else{join<-" -"}
      an_vars[i]<-gsub(":",interaction_char,an_vars[i])
      an_model<-paste(an_model, join, brawFormat(abs(coeffs[i]),digits=braw.env$report_precision)   ,times_string,an_vars[i],sep="")
    }
  }
  
  an_model
}


#' report a sample effect-size analysis of a simulated sample
#' 
#' @return ggplot2 object - and printed
#' @examples
#' reportDescription(analysis=doAnalysis())
#' @export
reportDescription<-function(analysis=braw.res$result){
  if (is.null(analysis)) analysis<-doResult(autoShow=FALSE)
  
  IV<-analysis$hypothesis$IV
  IV2<-analysis$hypothesis$IV2
  DV<-analysis$hypothesis$DV
  effect<-analysis$hypothesis
  design<-analysis$design
  evidence<-analysis$evidence
  
  if (is.null(IV2)) no_ivs<-1 else no_ivs<-2
  
  if (IV$type=="Categorical" && is.null(IV2)) {
    nc<-max(4,IV$ncats+1)
  } else {
    nc<-4
  }
  
  switch (DV$type,
          "Interval"={
            outputText<-c("\bLinear Model", rep("",nc-1))
          },
          "Ordinal"={
            outputText<-c("\bLinear Model", rep("",nc-1))
          },
          "Categorical"={
            outputText<-c("\bGeneralized Linear Model",rep("",nc-1))
          }
  )
  
  
  if (design$sIV1Use=="Within" || design$sIV2Use=="Within") {
    an_vars<-c("Intercept")
    if (IV$type=="Categorical") {
      for (i in 2:IV$ncats) {
      an_vars<-c(an_vars,paste0("iv1",IV$cases[i]))
      }
    } else {
      an_vars<-c(an_vars,IV$name)
    }
    if (!is.null(IV2$name)) {
      if (IV2$type=="Categorical") {
        for (i in 2:IV2$ncats) {
          an_vars<-c(an_vars,paste0("iv2",IV2$cases[i]))
        }
      } else {
        an_vars<-c(an_vars,IV2$name)
      }
    }
  } else {
    an_vars<-row.names(analysis$model)
    an_vars<-sub("iv1$",IV$name,an_vars)
  }
  
  an_vars<-sub("iv1:",paste(IV$name,":",sep=""),an_vars)
  an_vars<-sub("iv1",paste(IV$name,"|",sep=""),an_vars)
  if (!is.null(IV2)) {
    an_vars<-sub("iv2$",IV2$name,an_vars)
    an_vars<-sub("iv2",paste(IV2$name,"|",sep=""),an_vars)
  } 
  
  an_model<-makeFormula(IV,IV2,DV,design,evidence,analysis,an_vars)
  if (nchar(an_model)>80) {
    breaks<-unlist(gregexpr("[+-]",an_model))
    use<-sum(breaks<80)
    an_model1<-substr(an_model,1,breaks[use])
    an_model2<-substr(an_model,breaks[use],nchar(an_model))
    outputText<-c(outputText,"Formula:",paste(an_model1),rep("",nc-2))
    outputText<-c(outputText,"  ",paste("         ",an_model2),rep("",nc-2))
  } else {
    outputText<-c(outputText,"Formula:",paste(an_model),rep("",nc-2))
  }
  outputText<-c(outputText,"R^2",paste(brawFormat(analysis$rFull^2,digits=braw.env$report_precision),sep=""),rep("",nc-2))
  outputText<-c(outputText,"AIC",paste(brawFormat(analysis$aic,digits=braw.env$report_precision),sep=""),rep("",nc-2))
  
  outputText<-c(outputText,rep("",nc))
  outputText<-c(outputText,"\bEffect Size ","\bNormalized",rep("",nc-2))
  
  switch (no_ivs,
          { analysis$rIVse<-r2se(analysis$rIV,analysis$nval)
          outputText<-c(outputText,paste0("\b!j",IV$name,":"),
                                   paste(brawFormat(analysis$rIV,digits=braw.env$report_precision),
                                                             " +/- ",brawFormat(analysis$rIVse,digits=braw.env$report_precision),
                                                             sep=""),
                        paste0("CI = (",brawFormat(analysis$rFullCI[1],digits=braw.env$report_precision),
                               ",",brawFormat(analysis$rFullCI[2],digits=braw.env$report_precision),
                               ")"),
                        rep("",nc-3)
          )
          },{
            outputText<-c(outputText,"\b!jVariable","\bdirect","\bunique","\btotal",rep("",nc-4))
            outputText<-c(outputText,paste0("!j",IV$name,":"),
                          brawFormat(analysis$r$direct[1],digits=braw.env$report_precision),brawFormat(analysis$r$unique[1],digits=braw.env$report_precision),brawFormat(analysis$r$total[1],digits=braw.env$report_precision),
                          rep("",nc-4))
            outputText<-c(outputText,paste0("!j",IV2$name,":"),
                          brawFormat(analysis$r$direct[2],digits=braw.env$report_precision),brawFormat(analysis$r$unique[2],digits=braw.env$report_precision),brawFormat(analysis$r$total[2],digits=braw.env$report_precision),
                          rep("",nc-4))
            if (evidence$rInteractionOn) {
              outputText<-c(outputText,paste0("!j",IV$name,"*",IV2$name,":"),
                          brawFormat(analysis$r$direct[3],digits=braw.env$report_precision),brawFormat(analysis$r$unique[3],digits=braw.env$report_precision),brawFormat(analysis$r$total[3],digits=braw.env$report_precision),
                          rep("",nc-4))
            }
  
            outputText<-c(outputText,rep("",nc))
            
            an_rt<-brawFormat(analysis$rFull,digits=braw.env$report_precision) 
            an_rset<-brawFormat(analysis$rFullse,digits=braw.env$report_precision)
            outputText<-c(outputText,
                          "\b!jFull model:",
                          paste(an_rt,"+/-",an_rset),
                          paste0("CI = (",brawFormat(analysis$rFullCI[1],digits=braw.env$report_precision),
                                 ",",brawFormat(analysis$rFullCI[2],digits=braw.env$report_precision),
                                 ")"),
                          rep("",nc-3)
            )
          }
  )
  
  switch (no_ivs,
          {
            if (IV$type=="Categorical" && DV$type=="Interval"){
              outputText<-c(outputText,rep("",nc))
              mn<-c()
              ss<-c()
              cases<-levels(analysis$iv)
              if (braw.env$reportGroupMeans) {
                outputText<-c(outputText,paste0("\b",IV$name,"="))
                for (i in 1:IV$ncats){
                  outputText<-c(outputText,paste0("\b",IV$cases[i]))
                }
                outputText<-c(outputText,rep("",nc-(IV$ncats+1)))
                outputText<-c(outputText,"\bMean")
                for (i in 1:IV$ncats){
                  use<-(analysis$iv==cases[i])
                v<-analysis$dv[use]
                mn[i]<-mean(v,na.rm=TRUE)
                ss[i]<-sd(v,na.rm=TRUE)
                outputText<-c(outputText,brawFormat(mn[i],digits=braw.env$report_precision))
                }
                outputText<-c(outputText,rep("",nc-(IV$ncats+1)))
                outputText<-c(outputText,"\bSD")
                for (i in 1:IV$ncats){
                  outputText<-c(outputText,brawFormat(ss[i],digits=braw.env$report_precision))
                }
                outputText<-c(outputText,rep("",nc-(IV$ncats+1)))
              }
              fitted<-analysis$fitted
              residuals<-analysis$residuals
              rsd<-sd(residuals,na.rm=TRUE)
              outputText<-c(outputText,rep("",nc))
              if (IV$ncats==2){
                outputText<-c(outputText,"Difference(means):",brawFormat(diff(mn),digits=braw.env$report_precision),"sd(residuals):",brawFormat(rsd,digits=braw.env$report_precision),
                              rep("",nc-4))
              } else {
                outputText<-c(outputText,"sd(means):",brawFormat(sd(fitted),digits=braw.env$report_precision),"sd(residuals):",brawFormat(rsd,digits=braw.env$report_precision),
                              rep("",nc-4))
              }
            }
            if (IV$type=="Categorical" && DV$type=="Categorical"){
              outputText<-c(outputText,rep("",nc))
              obs<-matrix(nrow=IV$ncats,ncol=DV$ncats)
              for (i in 1:IV$ncats){
                for (j in 1:DV$ncats){
                  use<-(analysis$iv==IV$cases[i] & analysis$dv==DV$cases[j])
                  obs[i,j]<-sum(use)
                }
              }
              expect<-rowSums(obs)%*%t(colSums(obs))
              outputText<-c(outputText,"deviance",brawFormat(sum(abs(obs-expect))/sum(obs),digits=braw.env$report_precision),rep("",nc-2))
            }
            
          })
  
  
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)

}
