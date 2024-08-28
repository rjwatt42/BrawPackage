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
  z<-atanh(targetSample)

  llrA<-dnorm(z,mean=z,sd=1/sqrt(n-3))
  llr0<-dnorm(0,mean=z,sd=1/sqrt(n-3))
  
  t<-possibleResult$Theory
  rs<-t$rs
  llr0P<-approx(rs,t$priorSampDens_r_null,targetSample)$y
  llrAP<-approx(rs,colMeans(t$priorSampDens_r_plus),targetSample)$y

  rp<-t$rp
  sampleLikelihood_r<-t$sampleLikelihood_r
  rp_stats<-densityFunctionStats(sampleLikelihood_r,rp)
  rp_peak_full<-rp_stats$peak
  sampleLikelihood_r_show<-t$sampleLikelihood_r_show
  rp_stats<-densityFunctionStats(sampleLikelihood_r_show,rp)
  rp_peak_plus<-rp_stats$peak
  
  nc<-4
  nr<-12
  
  outputText<-rep("  ",nc*nr)
  
  row<-1
  outputText[1+nc*row]<-"\bEstimates  "
  outputText[2+nc*row]<-"\br[s]        "
  outputText[3+nc*row]<-"\br[mle](plus)"
  outputText[4+nc*row]<-"\br[mle](full)"
  row<-row+1
  outputText[2+nc*row]<-brawFormat(targetSample)
  outputText[3+nc*row]<-brawFormat(rp_peak_plus)
  outputText[4+nc*row]<-brawFormat(rp_peak_full)
  
  row<-row+1
  outputText[1+nc*row]<-""
  outputText[2+nc*row]<-""
  outputText[3+nc*row]<-""
  outputText[4+nc*row]<-""
  
  row<-row+1
  outputText[2+nc*row]<-"\bPlus "
  outputText[3+nc*row]<-"\bNull "
  outputText[4+nc*row]<-"\bLLR "
  
  row<-row+1
  outputText[1+nc*row]<-"\b!jEvidential sLLR   "
  outputText[2+nc*row]<-brawFormat(log(llrA))
  outputText[3+nc*row]<-brawFormat(log(llr0))
  outputText[4+nc*row]<-brawFormat(log(llrA/llr0))
  
  if (possibleResult$possible$UsePrior!="none" && possibleResult$prior$populationNullp>0) {
    
  row<-row+1
  outputText[1+nc*row]<-"\b!jPrior LLR   "
  outputText[2+nc*row]<-brawFormat(log(llrAP)-log(llrA))
  outputText[3+nc*row]<-brawFormat(log(llr0P)-log(llr0))
  outputText[4+nc*row]<-brawFormat((log(llrAP)-log(llrA))-(log(llr0P)-log(llr0)))

  row<-row+1
  outputText[1+nc*row]<-"\b!jEvidential*Prior dLLR   "
  outputText[2+nc*row]<-brawFormat(log(llrAP))
  outputText[3+nc*row]<-brawFormat(log(llr0P))
  outputText[4+nc*row]<-brawFormat(log(llrAP/llr0P))
  }
  
  reportPlot(outputText,nc,nr)
  
}
