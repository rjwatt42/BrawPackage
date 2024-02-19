

showExpected<-function(expectedResult=makeExpected(100),showType="Basic",
                       dimension="1D",orientation="vert",
                       effectType="direct",showTheory=TRUE
) {
  if (is.numeric(expectedResult)) expectedResult=makeExpected(expectedResult)
  
  if (is.element(showType,c("NHSTErrors","FDR"))) {
    if (expectedResult$nullcount<expectedResult$count) {
      expectedResult<-makeExpected(0,expectedResult,doingNull=TRUE)
    }
  }

  if (expectedResult$hypothesis$effect$world$worldOn) {
    fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)
  } else {
    switch (showType,
            "NHSTErrors"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
            "FDR"={fullResult<-mergeExpected(expectedResult$result,expectedResult$nullresult)},
            "e1"={fullResult<-expectedResult$nullresult},
            "e2"={fullResult<-expectedResult$result},
            {fullResult<-expectedResult$result}
    )
  }
  fullResult<-c(fullResult,list(hypothesis=expectedResult$hypothesis,
                                design=expectedResult$design,
                                evidence=expectedResult$evidence)
  )
  g<-showInference(fullResult,showType=showType,dimension=dimension,orientation=orientation,
                effectType=effectType,showTheory=showTheory
  ) 
  g<-g+ggtitle(paste0("Expected: ",format(expectedResult$count),"  "))+theme(plot.title=element_text(face='plain', size=8, hjust=0.9))
  g
}

