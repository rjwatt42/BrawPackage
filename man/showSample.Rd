% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showSample.R
\name{showSample}
\alias{showSample}
\title{show a simulated sample}
\usage{
showSample(
  sample = braw.res$result,
  marginals = FALSE,
  plotArea = c(0, 0, 1, 1)
)
}
\value{
ggplot2 object - and printed
}
\description{
show a simulated sample
}
\examples{
showSample(sample=doSample(),marginals=FALSE)
}
