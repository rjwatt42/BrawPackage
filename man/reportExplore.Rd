% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportExplore.R
\name{reportExplore}
\alias{reportExplore}
\title{report the estimated population characteristics from varying parameter}
\usage{
reportExplore(
  exploreResult = braw.res$explore,
  showType = "rs",
  whichEffect = "All",
  effectType = "all",
  quantileShow = 0.5,
  reportStats = "Medians",
  returnDataFrame = FALSE
)
}
\arguments{
\item{showType}{"rs","p","n","ws", "p(sig)" \cr
"NHST", "Hits", "Misses"}
}
\value{
ggplot2 object - and printed
}
\description{
report the estimated population characteristics from varying parameter
}
\examples{
showExplore(exploreResult=doExplore(),
                       showType="rs",
                       whichEffect="All",effectType="all",
                       quantileShow=0.5,reportStats="Medians"
                       )
}
