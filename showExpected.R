

showExpected<-function(expectedResult=makeExpected(100),type="Basic",dimension="1D",orientation="vert",
                       showType="direct",showTheory=TRUE
) {
  if (is.numeric(expectedResult)) expectedResult=makeExpected(expectedResult)
  fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)
  fullResult<-c(fullResult,list(hypothesis=expectedResult$hypothesis,
                                design=expectedResult$design,
                                evidence=expectedResult$evidence)
  )
  g<-showInference(fullResult,type=type,dimension=dimension,orientation=orientation,
                showType=showType,showTheory=showTheory
  ) 
  g
}

