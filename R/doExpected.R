##################################################################################    
# EXPECTED    

mergeExpected<-function(r1,r2) {
  newResult<-list(
    rIV=rbind(r1$rIV,r2$rIV),
    pIV=rbind(r1$pIV,r2$pIV),
    rpIV=rbind(r1$rpIV,r2$rpIV),
    roIV=rbind(r1$roIV,r2$roIV),
    poIV=rbind(r1$poIV,r2$poIV),
    nval=rbind(r1$nval,r2$nval),
    noval=rbind(r1$noval,r2$noval),
    df1=rbind(r1$df1,r2$df1)
  )
  if (!is.null(r1$rIV2)) {
    newResult<-c(newResult,list(
      rIV2=rbind(r1$rIV2,r2$rIV2),
      pIV2=rbind(r1$pIV2,r2$pIV2),
      rIVIV2DV=rbind(r1$rIVIV2DV,r2$rIVIV2DV),
      pIVIV2DV=rbind(r1$pIVIV2DV,r2$rIVIV2DV),
      r=list(direct=rbind(r1$r$direct,r2$r$direct),
             unique=rbind(r1$r$unique,r2$r$unique),
             total=rbind(r1$r$total,r2$r$total)
      ),
      p=list(direct=rbind(r1$p$direct,r2$p$direct),
             unique=rbind(r1$p$unique,r2$p$unique),
             total=rbind(r1$p$total,r2$p$total)
      )
    )
    )
  }
}
# function to clear 
resetExpected<-function(nsims=0,evidence,expectedResult=NULL){
  
  if (nsims>0) {
    b<-matrix(NA,nsims,1)
    if (evidence$rInteractionOn) bm<-matrix(NA,nsims,3)
    else  bm<-matrix(NA,nsims,2)
  } else {
    b<-NA
    bm<-NA
  }
  newResult<-list(
    rIV=b,pIV=b,rpIV=b,roIV=b,poIV=b,nval=b,noval=b,df1=b
  )
  newResult<-c(newResult,list(
    rIV2=b,pIV2=b,rIVIV2DV=b,pIVIV2DV=b,
    r=list(direct=bm,unique=bm,total=bm),
    p=list(direct=bm,unique=bm,total=bm)
  )
  )
  newNullResult<-newResult

  if (!is.null(expectedResult)) {
    newResult<-mergeExpected(expectedResult$result,newResult)
    count<-expectedResult$count
    newNullResult<-mergeExpected(expectedResult$nullresult,newNullResult)
    nullcount<-expectedResult$nullcount
  } else {
    count<-0
    nullcount<-0
  }

  list(result=newResult,
       nullresult=newNullResult,
       count=count,
       nullcount=nullcount,
       nsims=nsims+count)
}

#' make multiple samples with analysis
#' 
#' @returns expectedResult object
#' @examples
#' expectedResult<-doExpected(nsims=100,expectedResult=NULL,hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
#'                              doingNull=FALSE,autoShow=braw.env$autoShow,showType="Basic")
#' @seealso showExpected() and reportExpected())
#' @export
doExpected <- function(nsims=10,expectedResult=NULL,hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=makeEvidence(),
                         doingNull=FALSE,autoShow=braw.env$autoShow,showType="Basic") {

  if (!is.null(expectedResult)) {
    hypothesis<-expectedResult$hypothesis
    design<-expectedResult$design
    evidence<-expectedResult$evidence
  }
  if (nsims>0)
    expectedResult<-c(resetExpected(nsims,evidence,expectedResult),
                      list(hypothesis=hypothesis,
                           design=design,
                           evidence=evidence)
    )
  #
  if (doingNull && !hypothesis$effect$world$worldOn) {
    hypothesisNull<-hypothesis
    hypothesisNull$effect$rIV<-0
    # catch up - make enough null results to match results
    if (expectedResult$nullcount<expectedResult$count) {
      ns<-expectedResult$count-expectedResult$nullcount
      expectedResult$nullresult<-multipleAnalysis(ns,hypothesisNull,design,evidence,expectedResult$nullresult)
      expectedResult$nullcount<-expectedResult$nullcount+ns
    }
  }
  
  if (autoShow) {
    min_ns<-floor(log10(nsims/100))
    min_ns<-max(0,min_ns)
    ns<-10^min_ns
  } else
    ns<-nsims

  nsims<-nsims+expectedResult$count
  while (expectedResult$count<nsims) {
    if (expectedResult$count/ns>=10) ns<-ns*10
    if (expectedResult$count+ns>nsims) ns<-nsims-expectedResult$count
    expectedResult$result<-multipleAnalysis(ns,hypothesis,design,evidence,expectedResult$result)
    expectedResult$count<-expectedResult$count+ns
    if (doingNull && !hypothesis$effect$world$worldOn) {
      expectedResult$nullresult<-multipleAnalysis(ns,hypothesisNull,design,evidence,expectedResult$nullresult)
      expectedResult$nullcount<-expectedResult$nullcount+ns
    }
    if (autoShow) print(showExpected(expectedResult,showType=showType))
  }

  expectedResult<-c(list(type="expected"),expectedResult)
  setBrawRes("expected",expectedResult)
  return(expectedResult)
}

