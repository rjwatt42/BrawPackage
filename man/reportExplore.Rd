% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportExplore.R
\name{reportExplore}
\alias{reportExplore}
\title{report the estimated population characteristics from varying parameter}
\usage{
reportExplore(
  exploreResult,
  showType = "r",
  whichEffect = "All",
  effectType = "All"
)
}
\arguments{
\item{showType}{"r","p","n","w", "p(sig)" \cr
"NHST", "Hits","FDR;FMR"}
}
\value{
ggplot2 object - and printed
}
\description{
report the estimated population characteristics from varying parameter
}
\examples{
showExplore(exploreResult=doExplore(),
                       showType="r",
                       whichEffect="All",effectType="All")
}
