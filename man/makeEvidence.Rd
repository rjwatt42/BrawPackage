% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSystem.R
\name{makeEvidence}
\alias{makeEvidence}
\title{make an evidence definition}
\usage{
makeEvidence(
  shortHand = FALSE,
  sigOnly = FALSE,
  AnalysisTerms = FALSE,
  rInteractionOnly = TRUE,
  ssqType = "Type3",
  caseOrder = "AsStated",
  llr = list(e1 = c(), e2 = 0),
  useAIC = "AIC",
  doSEM = FALSE,
  Welch = FALSE,
  Transform = "None",
  McFaddens = TRUE,
  minRp = 0,
  prior = makeWorld(TRUE, "Uniform", "r"),
  metaAnalysis = makeMetaAnalysis()
)
}
\arguments{
\item{ssqType}{"Type1","Type2","Type3"}

\item{caseOrder}{"Alphabetic","AsFound","Frequency"}

\item{Transform}{"None","Log","Exp"}
}
\description{
make an evidence definition
}
\examples{
makeEvidence(shortHand=FALSE,sigOnly=FALSE,
             AnalysisTerms=TRUE,rInteractionOnly=TRUE,ssqType="Type3",
             caseOrder="Alphabetic",
             llr=list(e1=c(),e2=0),
             useAIC="AIC",
             doSEM=FALSE,
             Welch=FALSE,Transform="None",
             prior=makeWorld(TRUE,"Uniform","r")
             metaAnalysis=makeMetaAnalysis()
             )
}
