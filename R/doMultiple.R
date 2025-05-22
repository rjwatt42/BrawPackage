##################################################################################    
# EXPECTED    

mergeMultiple<-function(r1,r2) {
  newResult<-list(
    rIV=rbind(r1$rIV,r2$rIV),
    pIV=rbind(r1$pIV,r2$pIV),
    rpIV=rbind(r1$rpIV,r2$rpIV),
    roIV=rbind(r1$roIV,r2$roIV),
    poIV=rbind(r1$poIV,r2$poIV),
    rFull=rbind(r1$rFull,r2$rFull),
    pFull=rbind(r1$pFull,r2$pFull),
    nval=rbind(r1$nval,r2$nval),
    noval=rbind(r1$noval,r2$noval),
    df1=rbind(r1$df1,r2$df1),
    sem=rbind(r1$sem,r2$sem),
    aic=rbind(r1$aic,r2$aic),
    aicNull=rbind(r1$aicNull,r2$aicNull),
    iv.mn=rbind(r1$iv.mn,r2$iv.mn),
    iv.sd=rbind(r1$iv.sd,r2$iv.sd),
    iv.sk=rbind(r1$iv.sk,r2$iv.sk),
    iv.kt=rbind(r1$iv.kt,r2$iv.ky),
    dv.mn=rbind(r1$dv.mn,r2$dv.mn),
    dv.sd=rbind(r1$dv.sd,r2$dv.sd),
    dv.sk=rbind(r1$dv.sk,r2$dv.sk),
    dv.kt=rbind(r1$dv.kt,r2$dv.ky),
    er.mn=rbind(r1$er.mn,r2$er.mn),
    er.sd=rbind(r1$er.sd,r2$er.sd),
    er.sk=rbind(r1$er.sk,r2$er.sk),
    er.kt=rbind(r1$er.kt,r2$er.ky)
  )
  colnames(newResult$sem)<-colnames(r2$sem)
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
resetMultiple<-function(nsims=0,evidence,multipleResult=NULL){
  
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
    rFull=b,pFull=b,
    aic=b,aicNull=b,sem=matrix(NA,nsims,8),
    iv.mn=b,iv.sd=b,iv.sk=b,iv.kt=b,
    dv.mn=b,dv.sd=b,dv.sk=b,dv.kt=b,
    er.mn=b,er.sd=b,er.sk=b,er.kt=b
  )
  newResult<-c(newResult,list(
    rIV2=b,pIV2=b,rIVIV2DV=b,pIVIV2DV=b,
    r=list(direct=bm,unique=bm,total=bm),
    p=list(direct=bm,unique=bm,total=bm)
  )
  )
  newNullResult<-newResult

  if (!is.null(multipleResult)) {
    newResult<-mergeMultiple(multipleResult$result,newResult)
    count<-multipleResult$count
    newNullResult<-mergeMultiple(multipleResult$nullresult,newNullResult)
    nullcount<-multipleResult$nullcount
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
#' @returns multipleResult object
#' @examples
#' multipleResult<-doMultiple(nsims=100,multipleResult=NULL,hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
#'                              doingNull=FALSE,inSteps=FALSE,autoShow=braw.env$autoShow,showType="Basic")
#' @seealso showMultiple() and reportMultiple())
#' @export
doMultiple <- function(nsims=10,multipleResult=NA,hypothesis=braw.def$hypothesis,design=braw.def$design,evidence=braw.def$evidence,
                         doingNull=FALSE,inSteps=FALSE,autoShow=braw.env$autoShow,showType="Basic") {

  if (length(multipleResult)==1 && is.na(multipleResult)) {
      if (identical(hypothesis,braw.res$multiple$hypothesis) &&
          identical(design,braw.res$multiple$design) &&
          identical(evidence,braw.res$multiple$evidence)
      )   
        multipleResult<-braw.res$multiple
      else 
        multipleResult<-NULL
  } 
  if (nsims==0) multipleResult<-NULL
  
  if (evidence$metaAnalysis$On) {
    if (is.null(multipleResult)) braw.res$metaMultiple<-NULL
    if (!is.null(multipleResult$fixed)) metaMultiple<-multipleResult
    else                                metaMultiple<-braw.res$metaMultiple
    metaMultiple<-doMetaMultiple(nsims=nsims,metaMultiple=metaMultiple,metaAnalysis=evidence$metaAnalysis,
                             hypothesis=hypothesis,design=design,evidence=evidence)
    if (autoShow) print(showMetaMultiple(metaMultiple))
    return(metaMultiple)
  }
  
  # if (nsims>0)
    multipleResult<-c(resetMultiple(nsims,evidence,multipleResult),
                      list(hypothesis=hypothesis,
                           design=design,
                           evidence=evidence)
    )
  #
  if (doingNull && !hypothesis$effect$world$worldOn) {
    hypothesisNull<-hypothesis
    hypothesisNull$effect$rIV<-0
    # catch up - make enough null results to match results
    if (multipleResult$nullcount<multipleResult$count) {
      ns<-multipleResult$count-multipleResult$nullcount
      multipleResult$nullresult<-multipleAnalysis(ns,hypothesisNull,design,evidence,multipleResult$nullresult)
      multipleResult$nullcount<-multipleResult$nullcount+ns
    }
  }
  
  if (inSteps && autoShow) {
    min_ns<-floor(log10(nsims/100))
    min_ns<-max(0,min_ns)
    ns<-10^min_ns
  } else
    ns<-nsims

  nsims<-nsims+multipleResult$count
  while (multipleResult$count<nsims) {
    if (multipleResult$count/ns>=10) ns<-ns*10
    if (multipleResult$count+ns>nsims) ns<-nsims-multipleResult$count
    multipleResult$result<-multipleAnalysis(ns,hypothesis,design,evidence,multipleResult$result)
    multipleResult$count<-multipleResult$count+ns
    if (doingNull && !hypothesis$effect$world$worldOn) {
      multipleResult$nullresult<-multipleAnalysis(ns,hypothesisNull,design,evidence,multipleResult$nullresult)
      multipleResult$nullcount<-multipleResult$nullcount+ns
    }
    if (autoShow) print(showMultiple(multipleResult,showType=showType))
  }

  multipleResult<-c(list(type="multiple"),multipleResult)
  if (multipleResult$count>0)
    setBrawRes("multiple",multipleResult)
  return(multipleResult)
}

