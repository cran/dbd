\name{aHess}
\alias{aHess}
\title{
   Analytic hessian.
}
\description{
   Compute the hessian of the \bold{negative} log likelihood of
   a db or beta binomial distribution from an analytic expression
   for this quantity.
}
\usage{
aHess(object,x)
}
\arguments{
  \item{object}{
  An object of class \code{"mleDb"} or \code{"mleBb"} as
  returned by the function \code{\link{mleDb}()} or the function
  \code{\link{mleBb}()}.
}
  \item{x}{
  A numeric vector of observations appropriate for the model that
  was fitted to produce \code{object}.  Needed only if \code{object}
  is of class \code{"mleBb"}; the hessian for the db distribution
  depends only upon the parameters and not upon the data. 
}
}
\details{
  This function is essentially the same as the \code{\link{finfo}()}
  functions and differs from it only in that it is designed to
  act up \code{"mleDb"} or \code{"mleBb"} objects, from which
  (estimates of) the relevant parameters are extracted.
}
\value{
   A two-by-two positive definite (with any luck!) numeric matrix.
   Its inverse is an estimate of the covariance matrix of the
   parameter estimates.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
   \code{\link{nHess}()}
   \code{\link{finfo}()}
   \code{\link{mleDb}()}
}

\examples{
X   <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X   <- split(X,f=with(X,interaction(locn,depth)))
x   <- X[[19]]$y
fit <- mleDb(x, ntop=5)
H   <- aHess(fit)
print(solve(H)) # Equal to ...
print(vcov(fit))
X     <- hrsRcePred
top1e <- X[X$sbjType=="Expert","top1"]
fit   <- mleBb(top1e,size=10)
H     <- aHess(fit,x=top1e)
print(solve(H)) # Equal to ...
print(vcov(fit))
}

\concept{ hessian }
\concept{ covariance estimation }
\concept{ inference }
