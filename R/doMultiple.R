##################################################################################    
# EXPECTED    


#' ex6ract multiple samples 
#' 
#' @returns multipleResult object
#' @examples
#' newResult<-extractMultiple(mres1,n)
#' @export
extractMultiple<-function(r1,n=NULL) {
  if (is.null(n)) return(r1)
  newResult<-list(
    rIV=cbind(r1$rIV[n,]),
    pIV=cbind(r1$pIV[n,]),
    rpIV=cbind(r1$rpIV[n,]),
    roIV=cbind(r1$roIV[n,]),
    poIV=cbind(r1$poIV[n,]),
    rFull=cbind(r1$rFull[n,]),
    pFull=cbind(r1$pFull[n,]),
    nval=cbind(r1$nval[n,]),
    noval=cbind(r1$noval[n,]),
    df1=cbind(r1$df1[n,]),
    sem=cbind(r1$sem[n,]),
    AIC=cbind(r1$AIC[n,]),
    AICnull=cbind(r1$AICnull[n,]),
    iv.mn=cbind(r1$iv.mn[n,]),
    iv.sd=cbind(r1$iv.sd[n,]),
    iv.sk=cbind(r1$iv.sk[n,]),
    iv.kt=cbind(r1$iv.kt[n,]),
    dv.mn=cbind(r1$dv.mn[n,]),
    dv.sd=cbind(r1$dv.sd[n,]),
    dv.sk=cbind(r1$dv.sk[n,]),
    dv.kt=cbind(r1$dv.kt[n,]),
    er.mn=cbind(r1$er.mn[n,]),
    er.sd=cbind(r1$er.sd[n,]),
    er.sk=cbind(r1$er.sk[n,]),
    er.kt=cbind(r1$er.kt[n,])
  )
  colnames(newResult$sem)<-colnames(r1$sem)
  if (!is.null(r1$rIV2)) {
    newResult<-c(newResult,list(
      rIV2=cbind(r1$rIV2[n,]),
      pIV2=cbind(r1$pIV2[n,]),
      rIVIV2DV=cbind(r1$rIVIV2DV[n,]),
      pIVIV2DV=cbind(r1$pIVIV2DV[n,]),
      r=list(direct=rbind(r1$r$direct[n,]),
             unique=rbind(r1$r$unique[n,]),
             total=rbind(r1$r$total[n,])
      ),
      p=list(direct=rbind(r1$p$direct[n,]),
             unique=rbind(r1$p$unique[n,]),
             total=rbind(r1$p$total[n,])
      )
    )
    )
  }
  return(newResult)
}
#' merge multiple samples 
#' 
#' @returns multipleResult object
#' @examples
#' multipleResult<-mergeMultiple(mres1,mres2)
#' @export
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
    AIC=rbind(r1$AIC,r2$AIC),
    AICnull=rbind(r1$AICnull,r2$AICnull),
    iv.mn=rbind(r1$iv.mn,r2$iv.mn),
    iv.sd=rbind(r1$iv.sd,r2$iv.sd),
    iv.sk=rbind(r1$iv.sk,r2$iv.sk),
    iv.kt=rbind(r1$iv.kt,r2$iv.kt),
    dv.mn=rbind(r1$dv.mn,r2$dv.mn),
    dv.sd=rbind(r1$dv.sd,r2$dv.sd),
    dv.sk=rbind(r1$dv.sk,r2$dv.sk),
    dv.kt=rbind(r1$dv.kt,r2$dv.kt),
    er.mn=rbind(r1$er.mn,r2$er.mn),
    er.sd=rbind(r1$er.sd,r2$er.sd),
    er.sk=rbind(r1$er.sk,r2$er.sk),
    er.kt=rbind(r1$er.kt,r2$er.kt)
  )
  colnames(newResult$sem)<-colnames(r2$sem)
  # if (!is.null(r1$rIV2) && !all(is.na(r1$rIV2))) {
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
  # }  
  
  return(newResult)
}
# function to clear 
resetMultiple<-function(nsims=0,evidence,multipleResult=NULL){
  
  if (nsims>0) {
    b<-matrix(NA,nsims,1)
      bm<-matrix(NA,nsims,sum(evidence$AnalysisTerms))
  } else {
    b<-matrix(NA,1,1)
    bm<-matrix(NA,1,sum(evidence$AnalysisTerms))
  }
  newResult<-list(
    rIV=b,pIV=b,rpIV=b,roIV=b,poIV=b,nval=b,noval=b,df1=b,
    rFull=b,pFull=b,
    AIC=b,AICnull=b,sem=matrix(NA,nsims,8),
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
                         doingNull=FALSE,inSteps=FALSE,autoShow=braw.env$autoShow,showType="Basic",onlyReplication=FALSE) {

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
    
    oldResult<-NULL
    if (onlyReplication) {
      if (!is.null(braw.res$result$ResultHistory$original)) oldResult<-braw.res$result$ResultHistory$original
      else oldResult<-braw.res$result
      oldResult$design$Replication$On<-TRUE
    }
    else 

  if (doingNull && !hypothesis$effect$world$On) {
    hypothesisNull<-hypothesis
    hypothesisNull$effect$rIV<-0
    # catch up - make enough null results to match results
    if (multipleResult$nullcount<multipleResult$count) {
      ns<-multipleResult$count-multipleResult$nullcount
      multipleResult$nullresult<-multipleAnalysis(ns,hypothesisNull,design,evidence,multipleResult$nullresult,onlyReplication=onlyReplication)
      multipleResult$nullcount<-multipleResult$nullcount+ns
    }
  }
  
  if (nsims>0) {
  if (inSteps && autoShow) {
    min_ns<-floor(log10(nsims/100))
    min_ns<-max(0,min_ns)
    ns<-10^min_ns
  } else
    ns<-nsims
  if (braw.env$timeLimit<Inf) ns<-1
  } else {
    ns<-0
  }

  nsims<-nsims+multipleResult$count
  time.at.start<-Sys.time()
  while (multipleResult$count<nsims && (Sys.time()-time.at.start)<braw.env$timeLimit) {
    # if (multipleResult$count/ns>=10 && ) ns<-ns*10
    if (multipleResult$count+ns>nsims) ns<-nsims-multipleResult$count
    multipleResult$result<-multipleAnalysis(ns,hypothesis,design,evidence,multipleResult$result,onlyReplication=onlyReplication,oldResult=oldResult)
    multipleResult$count<-multipleResult$count+ns
    if (doingNull && !hypothesis$effect$world$On) {
      multipleResult$nullresult<-multipleAnalysis(ns,hypothesisNull,design,evidence,multipleResult$nullresult,onlyReplication=onlyReplication,oldResult=oldResult)
      multipleResult$nullcount<-multipleResult$nullcount+ns
    }
    if (autoShow) print(showMultiple(multipleResult,showType=showType))
  }

  multipleResult<-c(list(type="multiple"),multipleResult)
  # if (multipleResult$count>0)
    setBrawRes("multiple",multipleResult)
  return(multipleResult)
}

