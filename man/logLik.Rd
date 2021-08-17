\name{logLik}
\alias{logLik.mleDb}
\alias{logLik.mleBb}
\title{
   Retrieve the (maximised) log likelihood from an \code{"mleDb"} or
   an \code{"mleBb"} object.
}
\description{
   Extract the log likelihood attribute an object of class
   \code{"mleDb"} or \code{"mleBb"}.  I.e. obtain the maximum log
   likelihood in respect of the estimation of the parameters of a
   db or beta-binomial distribution.
}
\usage{
   \method{logLik}{mleDb}(object, ...)
   \method{logLik}{mleBb}(object, ...)
}
\arguments{
  \item{object}{
   An object of class \code{"mleDb"} as returned by \code{\link{mleDb}()}
   or of class \code{"mleBb"} as returned by \code{\link{mleBb}()}.
}
  \item{\dots}{
   Not used.
}
}
\value{
   An object of class \code{"logLik"}, which consists of
   a numeric scalar equal to the maximum log likelihood for the parameters
   of a db or beta-binomial distribution.  It has an attribute \code{"df"}
   equal to 2.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{mleDb}()}
  \code{\link{mleBb}()}
}
\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
fitz <- lapply(X,function(x){mleDb(x$y,ntop=5)})
sapply(fitz,logLik)
X     <- hrsRcePred
top1e <- X[X$sbjType=="Expert","top1"]
fit   <- mleBb(top1e,10)
logLik(fit)
}
\keyword{ utilities }
