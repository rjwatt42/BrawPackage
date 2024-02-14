animateExpected<-function(ns=1,n_cycles=20,expectedResult=NULL) {
  
  for (i in 1:n_cycles) {
    expectedResult<-makeExpected(ns,expectedResult=expectedResult); 
    print(showExpected(expectedResult))
    }
  
}
