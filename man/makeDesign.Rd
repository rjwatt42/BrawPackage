% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSystem.R
\name{makeDesign}
\alias{makeDesign}
\title{make a design}
\usage{
makeDesign(
  sN = 42,
  sMethod = makeSampling("Random"),
  sNRand = FALSE,
  sNRandK = 2,
  sIV1Use = "Between",
  sIV2Use = "Between",
  sWithinCor = 0.5,
  sBudgetOn = FALSE,
  sNBudget = 1000,
  sRangeOn = FALSE,
  sIVRange = c(-3, 3),
  sDVRange = c(-3, 3),
  sDependence = 0,
  sOutliers = 0,
  sCheating = "None",
  sCheatingAttempts = 5,
  sCheatingLimit = "Fixed",
  sCheatingBudget = 1000,
  Replication = makeReplication()
)
}
\arguments{
\item{sMethod}{sampling method object}

\item{sIV1Use}{"Between","Within"}

\item{sCheating}{"None","Grow","Prune","Replace","Retry","Add"}

\item{sCheatingLimit}{"Fixed","Budget"}
}
\value{
a design object
}
\description{
make a design
}
\examples{
makeDesign(sN=42, sMethod=makeSampling("Random") ,sNRand=FALSE,sNRandK=2, 
           sBudgetOn=FALSE,sNBudget=1000,
           sIV1Use="Between",sIV2Use="Between",  sWithinCor=0.5,
           
           sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
           sDependence=0, sOutliers=0,
           
           sCheating="None", sCheatingAttempts=5,
           sCheatingLimit="Fixed", sCheatingBudget=1000,
           
           Replication=makeReplication()
)
}
\seealso{
[showDesign()]
}
