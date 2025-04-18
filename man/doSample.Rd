% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doSample.R
\name{doSample}
\alias{doSample}
\title{make a simulated sample}
\usage{
doSample(
  hypothesis = braw.def$hypothesis,
  design = braw.def$design,
  autoShow = braw.env$autoShow
)
}
\value{
a sample object
}
\description{
make a simulated sample
}
\examples{
sample<-doSample(hypothesis=makeHypothesis(),design=makeDesign(),autoShow=braw.env$autoShow)
}
\seealso{
showSample() reportSample()
}
