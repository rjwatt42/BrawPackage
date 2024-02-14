##################################################################################    
# EXPECTED    

mergeExpected<-function(r1,r2) {
  newResult<-list(
    rpIV=rbind(r1$rpIV,r2$rpIV),
    rIV=rbind(r1$rIV,r2$rIV),
    pIV=rbind(r1$pIV,r2$pIV),
    roIV=rbind(r1$roIV,r2$roIV),
    poIV=rbind(r1$poIV,r2$poIV),
    nval=rbind(r1$nval,r2$nval),
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
resetExpected<-function(nsims=0,expectedResult=NULL){
  
  if (nsims>0) {
    b<-matrix(NA,nsims,1)
    bm<-matrix(NA,nsims,3)
  } else {
    b<-NULL
    bm<-NULL
  }
  newResult<-list(
    rpIV=b,rIV=b,pIV=b,roIV=b,poIV=b,nval=b,df1=b
  )
  newResult<-c(newResult,list(
    rIV2=b,pIV2=b,rIVIV2DV=b,pIVIV2DV=b,
    r=list(direct=bm,unique=bm,total=bm),
    p=list(direct=bm,unique=bm,total=bm)
  )
  )

  if (!is.null(expectedResult)) {
    newResult<-mergeExpected(expectedResult$result,newResult)
    count<-expectedResult$count
  } else {
    count<-0
  }

  list(result=newResult,
       count=count,
       nsims=nsims+count)
}


makeExpected <- function(nsims,hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),expectedResult=NULL,doingNull=FALSE) {
  
  if (doingNull && !hypothesis$effect$world$worldOn) {
    hypothesis$effect$world$worldOn<-TRUE
    hypothesis$effect$world$populationNullp<-0.5
  }
  expectedResult<-c(list(hypothesis=hypothesis,
                         design=design,
                         evidence=evidence),
                    resetExpected(nsims,expectedResult)
  )
  
  min_ns<-floor(log10(nsims/100))
  min_ns<-max(0,min_ns)
  ns<-10^min_ns
  n_cycles<-ceil(nsims/ns)

  if (ns>0) {
    for (ci in 1:n_cycles) {
      newCount<-expectedResult$count+ns
      expectedResult$result<-multipleAnalysis(ns,hypothesis,design,evidence,expectedResult$result)
      expectedResult$count<-newCount
    }
  }
  
  if (autoShow) print(showExpected(expectedResult,type="Basic"))
  return(expectedResult)
}

