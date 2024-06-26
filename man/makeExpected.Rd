% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doExpected.R
\name{doExpected}
\alias{doExpected}
\title{make multiple samples with analysis}
\usage{
doExpected(
  nsims = 10,
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
expectedResult<-doExpected(nsims=100,expectedResult=NULL,hypothesis=makeHypothesis(),design=makeDesign(),evidence=makeEvidence(),
                             doingNull=FALSE,autoShow=braw.env$autoShow,showType="Basic")
}
\seealso{
showExpected() and reportExpected())
}
