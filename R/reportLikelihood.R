#' report population estimates from a simulated sample
#' 
#' @param analysisType "Model", "Anova"
#' @return ggplot2 object - and printed
#' @examples
#' reportInference(analysis=doAnalysis())
#' @export
reportLikelihood<-function(possibleResult=braw.res$possibleResult){
  if (is.null(possibleResult)) possibleResult<-doPossible()
  if (is.null(possibleResult$possible)) possibleResult<-doPossible(possible=possibleResult)

  targetSample<-possibleResult$possible$targetSample
  n<-possibleResult$possible$design$sN
  zsample<-atanh(targetSample)

  t<-possibleResult$Theory
  rs<-t$rs
  rp<-t$rp
  
  sampleLikelihood_r<-t$sampleLikelihood_r
  rp_stats<-densityFunctionStats(sampleLikelihood_r,rp)
  rp_peak_full<-rp_stats$peak
  sampleLikelihood_r_show<-t$sampleLikelihood_r_show
  rp_stats<-densityFunctionStats(sampleLikelihood_r_show,rp)
  rp_peak_plus<-rp_stats$peak
  
  llrA<-dnorm(zsample,mean=zsample,sd=1/sqrt(n-3))
  llr0<-dnorm(0,mean=zsample,sd=1/sqrt(n-3))
  # zsample<-atanh(rp_peak_plus)
  # llrAmle<-dnorm(zsample,mean=zsample,sd=1/sqrt(n-3))
  # llr0mle<-dnorm(0,mean=zsample,sd=1/sqrt(n-3))
  
  llr0P<-approx(rp,t$priorSampDens_r_null,targetSample)$y
  llrAP<-approx(rp,colSums(t$priorSampDens_r_plus),targetSample)$y

  nc<-4
  nr<-12
  outputText<-rep("  ",nc*nr)
  
  row<-1
  outputText[1+nc*row]<-"!H!CEstimates  "
  outputText[2+nc*row]<-"r[s]"
  outputText[3+nc*row]<-"r[mle]"
  outputText[4+nc*row]<-"r[mle](+)"
  row<-row+1
  outputText[2+nc*row]<-paste0("!j",brawFormat(targetSample))
  outputText[3+nc*row]<-paste0("!j",brawFormat(rp_peak_full))
  outputText[4+nc*row]<-paste0("!j",brawFormat(rp_peak_plus))
  
  row<-row+1
  outputText[1+nc*row]<-" "
  outputText[2+nc*row]<-" "
  outputText[3+nc*row]<-" "
  outputText[4+nc*row]<-" "
  
  row<-row+1
  outputText[1+nc*row]<-""
  outputText[2+nc*row]<-""
  outputText[3+nc*row]<-""
  outputText[4+nc*row]<-""
  
  row<-row+1
  outputText[1+nc*row]<-"!H!CLikelihoods"
  outputText[2+nc*row]<-"LLR "
  
  row<-row+1
  outputText[1+nc*row]<-"sLLR(r[s]/r[0] | r[s])"
  outputText[2+nc*row]<-paste0("!j",brawFormat(log(llrA/llr0)))

  if (possibleResult$possible$UsePrior!="none" && possibleResult$prior$populationNullp>0) {
  row<-row+1
  outputText[1+nc*row]<-"Prior(r[+]/r[0] | r[s])"
  outputText[2+nc*row]<-paste0("!j",brawFormat((log(llrAP)-log(llrA))-(log(llr0P)-log(llr0))))

  row<-row+1
  outputText[1+nc*row]<-"dLLR(r[+]/r[0] | r[s])"
  outputText[2+nc*row]<-paste0("!j",brawFormat(log(llrAP/llr0P)))
  }
  
  reportPlot(outputText,nc,nr)
  
}
