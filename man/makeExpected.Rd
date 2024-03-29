% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeExpected.R
\name{makeExpected}
\alias{makeExpected}
\title{make multiple samples with analysis}
\usage{
makeExpected(
  nsims = 100,
  expectedResult = NULL,
  hypothesis = makeHypothesis(),
  design = makeDesign(),
  evidence = makeEvidence(),
  doingNull = FALSE,
  autoShow = FALSE,
  showType = "Basic"
)
}
\value{
expectedResult object
}
\description{
make multiple samples with analysis
}
\examples{
expectedResult<-makeExpected(nsims=100,expectedResult=NULL,hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
                             doingNull=FALSE,autoShow=FALSE,showType="Basic")
}
\seealso{
showExpected() and reportExpected())
}
