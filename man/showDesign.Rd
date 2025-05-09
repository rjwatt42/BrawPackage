% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showSystem.R
\name{showDesign}
\alias{showDesign}
\title{show a design object}
\usage{
showDesign(
  design = braw.def$design,
  hypothesis = braw.def$hypothesis,
  plotArea = c(0, 0, 1, 1),
  autoShow = FALSE,
  g = NULL
)
}
\value{
ggplot2 object - and printed
}
\description{
show a design object
}
\examples{
showDesign(design=makeDesign())
}
