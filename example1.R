
hypothesis1<-makeHypothesis(effect=makeEffect(rIV=0.3))
expectedResult1<-makeExpected(nsim=1000,hypothesis=hypothesis1,
                              autoShow=TRUE)

expectedResult2<-makeExpected(nsim=1000,hypothesis=hypothesis1,doingNull=TRUE,
                              autoShow=TRUE,type="NHSTErrors")

hypothesis2<-getHypothesis("Psych")
design2<-getDesign("Psych")
expectedResult3<-makeExpected(nsim=1000,hypothesis=hypothesis2,design=design2,
                              autoShow=TRUE)

showExpected(expectedResult3,type="NHSTErrors")

evidence<-makeEvidence(shortHand = TRUE)

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis2,design2,evidence,
                           autoShow = TRUE, Explore_show = "p(sig)")

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis2,design2,evidence,doingNull=TRUE,
                           autoShow = TRUE, Explore_show = "NHSTErrors")


exploreResult<-makeExplore(1000,type="SampleSize",hypothesis = hypothesis1,
                           autoShow = TRUE, Explore_show="p(sig)")

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis = hypothesis1,doingNull=TRUE,
                           autoShow = TRUE, Explore_show="NHSTErrors")

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis = hypothesis2,design=design2,doingNull=TRUE,
                           autoShow = TRUE, Explore_show="NHSTErrors")
