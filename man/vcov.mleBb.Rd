\name{vcov.mleBb}
\alias{vcov.mleBb}
\title{
   Retrieve the covariance matrix from an \code{"mleBb"} object.
}
\description{
   Extract the covariance matrix attribute an object of class
   \code{"mleBb"}.  I.e. obtain the estimated covariance matrix
   of the maximum likelihood estimates of the parameters of a
   beta binomial distribution.
}
\usage{
   \method{vcov}{mleBb}(object, ...)
}
\arguments{
  \item{object}{
   An object of class \code{"mleBb"} as returned by \code{\link{mleBb}()}.
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
  \code{\link{vcov.mleDb})}
  \code{\link{mleBb}()}
}
\examples{
X     <- hrsRcePred
top1e <- X[X$sbjType=="Expert","top1"]
fit   <- mleBb(top1e,size=10)
vcov(fit)
}
\keyword{ utilities }
