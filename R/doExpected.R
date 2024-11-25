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
    df1=rbind(r1$df1,r2$df1),
    iv.mn=rbind(r1$iv.mn,r2$iv.mn),
    iv.sd=rbind(r1$iv.sd,r2$iv.sd),
    iv.sk=rbind(r1$iv.sk,r2$iv.sk),
    iv.kt=rbind(r1$iv.kt,r2$iv.ky),
    dv.mn=rbind(r1$dv.mn,r2$dv.mn),
    dv.sd=rbind(r1$dv.sd,r2$dv.sd),
    dv.sk=rbind(r1$dv.sk,r2$dv.sk),
    dv.kt=rbind(r1$dv.kt,r2$dv.ky),
    rd.mn=rbind(r1$rd.mn,r2$rd.mn),
    rd.sd=rbind(r1$rd.sd,r2$rd.sd),
    rd.sk=rbind(r1$rd.sk,r2$rd.sk),
    rd.kt=rbind(r1$rd.kt,r2$rd.ky)
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
    rIV=b,pIV=b,rpIV=b,roIV=b,poIV=b,nval=b,noval=b,df1=b,
    iv.mn=b,iv.sd=b,iv.sk=b,iv.kt=b,
    dv.mn=b,dv.sd=b,dv.sk=b,dv.kt=b,
    rd.mn=b,rd.sd=b,rd.sk=b,rd.kt=b
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
doExpected <- function(nsims=10,expectedResult=braw.res$expected,hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=makeEvidence(),
                         doingNull=FALSE,inSteps=FALSE,autoShow=braw.env$autoShow,showType="Basic") {

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
  
  if (inSteps && autoShow) {
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

