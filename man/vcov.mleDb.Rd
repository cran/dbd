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
   The covariance matrix attribute may not exist (may be \code{NULL})
   if the argument \code{covmat} of \code{mleDb()} was set to
   \code{FALSE}.  In this case the value returned by this function
   is \code{NA}.

   The estimated covariance matrix is the inverse of the hessian
   of the negative log likelihood. (This may also be referred to
   as the Fisher information, evaluated at the maximum likelihood
   estimates of the parameters.)
}

\value{
   A two-by-two positive definite (with any luck!) numeric matrix,
   or \code{NA} (see \bold{Details}.  When a matrix is returned it
   is an estimate of the covariance matrix of the parameter estimates.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{mleDb}()}
}
\examples{
if(requireNamespace("hmm.discnp")) {
    X    <- hmm.discnp::SydColDisc
    X$y  <- as.numeric(X$y)
    X    <- split(X,f=with(X,interaction(locn,depth)))
    fitz <- lapply(X,function(x){mleDb(x$y,ntop=5)})
    lapply(fitz,vcov)
}
}
\keyword{ utilities }
