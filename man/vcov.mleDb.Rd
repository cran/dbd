\name{vcov.mleDb}
\alias{vcov.mleDb}
\title{
   Retrieve the covariance matrix from an \code{"mleDb"} object.
}
\description{
   Extract the covariance matrix attribute an object of class
   \code{"mleDb"}.  I.e. obtain the estimated covariance matrix
   of the maximum likelihood estimates of the parameters of a
   db distribution.
}
\usage{
   \method{vcov}{mleDb}(object, ...)
}
\arguments{
  \item{object}{
   An object of class \code{"mleDb"} as returned by \code{\link{mleDb}()}.
}
  \item{\dots}{
   Not used.
}
}
\details{
   The estimated covariance matrix is the inverse of the hessian
   of the negative log likelihood. (This may also be referred to
   as the observed Fisher information --- the Fisher information
   evaluated at the maximum likelihood estimates of the parameters).
}
\value{
   A two-by-two positive definite (with any luck!) numeric matrix.
   It is an estimate of the covariance matrix of the parameter estimates.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{vcov.mleBb})}
  \code{\link{mleDb}()}
}
\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
fitz <- lapply(X,function(x){mleDb(x$y,ntop=5)})
lapply(fitz,vcov)
}
\keyword{ utilities }
