

showExpected<-function(expectedResult=makeExpected(100),type="Basic",dimension="1D",orientation="vert",
                       showType="direct",Theory=showTheory
) {
  if (is.numeric(expectedResult)) expectedResult=makeExpected(expectedResult)
  oldshowTheory<-showTheory
  showTheory<<-Theory
  g<-showInference(expectedResult$result,type=type,dimension=dimension,orientation=orientation,
                showType=showType
  ) 
  showTheory<<-oldshowTheory
  g
}

