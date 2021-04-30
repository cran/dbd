\name{hrsRcePred}
\alias{hrsRcePred}
\docType{data}
\title{
    Horse race prediction data.
}
\description{
    Counts of correct predictions of the outcomes of 10 harness
    races made by \dQuote{experts} and \dQuote{non-experts}.
}
\usage{hrsRcePred}
\format{
  A data frame with 30 observations on the following 4 variables.
  \describe{
    \item{\code{sbjType}}{A character vector with entries \code{"NonXpert"}
    and \code{"Expert"}, which classifies the \dQuote{subjects} (the
    people making the predictions of the race outcomes).}
    \item{\code{subject}}{An integer vector indexing the subjects.  (Not
    of any real consequence.)}
    \item{\code{top1}}{An integer vector giving the counts of correct predictions
    of the winners of 10 harness races.}
    \item{\code{top3}}{An integer vector giving the counts of correct predictions
    of the top three horses (\dQuote{win/place/show} in 10 harness races.}
  }
}
\details{
   In Ceci and Liker (1986) it is stated that subjects were
   classified as \dQuote{experts} and \dQuote{nonexperts} based on
   their ability to predict post-time odds on the basis of factual
   information about horses.

   It appears that the counts in \code{top1} and \code{top3} pertain
   to the \emph{same} 10 races, but this is not completely clear.
}
\source{
   These data are taken from the paper cited in the first of the
   two given in the \bold{References} below.  They were provided
   by a generous email correspondent who prefers to remain anonymous.
}
\references{
Ceci, S. J. and Liker, J. K. (1986).  A day at the races: A study
of IQ, expertise, and cognitive complexity.  \emph{Journal of Experimental
Psychology, General} \bold{115}, pp. 255 -- 266.

Ceci, S. J. and Liker, J. K. (1988).  Stalking the IQ-expertise
relation: When the critics go fishing.  \emph{Journal of Experimental
Psychology, General} \bold{117}, pp. 96 -- 100.
}

\examples{
    X     <- hrsRcePred
    top1e <- X[X$sbjType=="Expert","top1"]
    top1n <- X[X$sbjType=="NonXpert","top1"]
    top3e <- X[X$sbjType=="Expert","top3"]
    top3n <- X[X$sbjType=="NonXpert","top3"]
    dbfit1e <- mleDb(top1e,ntop=10,zeta=TRUE)
    dbfit1n <- mleDb(top1n,ntop=10,zeta=TRUE)
    dbfit3e <- mleDb(top3e,ntop=10,zeta=TRUE)
    dbfit3n <- mleDb(top3n,ntop=10,zeta=TRUE)
\dontrun{ # Takes too long for CRAN
    set.seed(42) # To get repeatable Monte Carlo p-values.
    print(gof(dbfit1e,obsd=top1e,MC=TRUE,maxit=10000)$pval) # 0.41
    print(gof(dbfit1n,obsd=top1n,MC=TRUE)$pval) # 0.75
    print(gof(dbfit3e,obsd=top3e,MC=TRUE)$pval) # 0.37
    print(gof(dbfit3n,obsd=top3n,MC=TRUE,maxit=8000)$pval) # 0.36
}
}
\keyword{datasets}
