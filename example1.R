
hypothesis1<-makeHypothesis(effect=makeEffect(rIV=0.3))
expectedResult1<-makeExpected(nsim=1000,hypothesis=hypothesis1,
                              autoShow=TRUE)

expectedResult2<-makeExpected(nsim=1000,hypothesis=hypothesis1,doingNull=TRUE,
                              autoShow=TRUE,type="NHSTErrors")

exploreResult3<-makeExplore(1000,type="SampleSize",hypothesis = hypothesis1,
                           autoShow = TRUE, Explore_show="p(sig)")

exploreResult4<-makeExplore(1000,type="SampleSize",hypothesis = hypothesis1,doingNull=TRUE,
                           autoShow = TRUE, Explore_show="NHSTErrors")


hypothesis2<-getHypothesis("Psych")
design2<-getDesign("Psych")
evidence2<-makeEvidence(shortHand = TRUE)
expectedResult5<-makeExpected(nsim=1000,hypothesis=hypothesis2,design=design2,
                              autoShow=TRUE)


exploreResult<-makeExplore(1000,type="SampleSize",hypothesis2,design2,evidence2,
                           autoShow = TRUE, Explore_show = "p(sig)")

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis2,design2,doingNull=TRUE,
                           autoShow = TRUE, Explore_show="NHSTErrors")

exploreResult<-makeExplore(1000,type="SampleSize",hypothesis2,design2,doingNull=TRUE,
                           min_n=10,max_n=1000,xlog=TRUE,
                           autoShow = TRUE, Explore_show="NHSTErrors")
