% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showSystem.R
\name{showWorldSampling}
\alias{showWorldSampling}
\title{show the prediction corresponding to a hypothesis & design}
\usage{
showWorldSampling(
  hypothesis = braw.def$hypothesis,
  design = braw.def$design,
  sigOnly = FALSE,
  plotArea = c(0, 0, 1, 1),
  autoShow = braw.env$autoShow,
  g = NULL
)
}
\value{
ggplot2 object - and printed
}
\description{
show the prediction corresponding to a hypothesis & design
}
\examples{
showWorldSampling(hypothesis=makeHypothesis(),design=makeDesign(),sigOnly=FALSE)
}
