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
  if (!is.null(IV2)) {
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
  newNullResult<-newResult
  
  if (!is.null(expectedResult)) {
    newResult<-mergeExpected(expectedResult$result,newResult)
    newNullResult<-mergeExpected(expectedResult$nullresult,newNullResult)
    count<-expectedResult$count
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


makeExpected <- function(nsims,hypothesis,design,evidence,expectedResult=NULL) {
  
  expectedResult<-c(list(hypothesis=hypothesis,
                         design=design,
                         evidence=evidence),
                    resetExpected(nsims,expectedResult)
  )
  
  min_ns<-floor(log10(expectedResult$nsims/100))
  ns<-10^min_ns
  n_cycles<-ceil(expectedResult$nsims/ns)
  
  if (ns>0) {
    for (ci in 1:n_cycles) {
      newCount<-expectedResult$count+ns
      expectedResult$result<-multipleAnalysis(hypothesis,ns,expectedResult$result,sigOnly=evidence$sigOnly)
      expectedResult$count<-newCount
    }
  }
  
    # wind up
    if (effect$world$worldOn && is.element(expected$type,c("NHSTErrors","FDR"))){
      nulls<-expectedResult$result$rpIV==0
      expectedResult$nullresult$rpIV<-expectedResult$result$rpIV[nulls]
      expectedResult$nullresult$roIV<-expectedResult$result$roIV[nulls]
      expectedResult$nullresult$rIV<-expectedResult$result$rIV[nulls]
      expectedResult$nullresult$pIV<-expectedResult$result$pIV[nulls]
      expectedResult$nullresult$nval<-expectedResult$result$nval[nulls]
      expectedResult$nullresult$df1<-expectedResult$result$df1[nulls]
      
      expectedResult$result$rpIV<-expectedResult$result$rpIV[!nulls]
      expectedResult$result$roIV<-expectedResult$result$roIV[!nulls]
      expectedResult$result$rIV<-expectedResult$result$rIV[!nulls]
      expectedResult$result$pIV<-expectedResult$result$pIV[!nulls]
      expectedResult$result$nval<-expectedResult$result$nval[!nulls]
      expectedResult$result$df1<-expectedResult$result$df1[!nulls]
      
      expectedResult$count<-sum(!is.na(expectedResult$result$rIV))
      expectedResult$nullcount<-sum(!is.na(expectedResult$nullresult$rIV))
    }

  return(expectedResult)
}

