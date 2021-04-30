\name{finfo}
\alias{finfo}
\title{
    Fisher information.
}
\description{
    Compute the Fisher information for a db distribution given
    the parameters of that distribution.  A specified number of
    observations \code{ndata} must be supplied.  The inverse of the
    Fisher information is an estimate of the covariance matrix of
    the parameter estimates.
}
\usage{
finfo(alpha, beta, ntop, zeta = FALSE, ndata)
}
\arguments{
  \item{alpha}{
  See \code{\link{ddb}()}.
}
  \item{beta}{
  See \code{\link{ddb}()}.
}
  \item{ntop}{
  See \code{\link{ddb}()}.
}
  \item{zeta}{
  See \code{\link{ddb}()}.
}
  \item{ndata}{
  The number of observations for which the Fisher information
  is being determined.
}
}
\details{
   This function differs from \code{\link{aHess}()} in that its
   arguments are prescribed \dQuote{individually} rather than
   being extracted from an \code{"mleDb"} object.  This allows
   \code{finfo()} to be applied to \dQuote{true} parameters (where
   these are known) rather than estimated ones.

   Note that the number of observations must be supplied explicitly
   as \code{ndata} whereas for \code{aHess()} this number is
   extracted from the \code{object} argument.

   This function in effect calculates the \emph{expected}
   information, since the information matrix does not depend on the
   parameters.
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
  \code{\link{aHess}()}
  \code{\link{nHess}()}
  \code{\link{mleDb}()}
}
\examples{
print(finfo(0.6,0.3,5,FALSE,54))
if(requireNamespace("hmm.discnp")) {
    X   <- hmm.discnp::SydColDisc
    X$y <- as.numeric(X$y)
    X   <- split(X,f=with(X,interaction(locn,depth)))
    x   <- X[[19]]$y
    fit <- mleDb(x, ntop=5)
    alpha <- unname(fit["alpha"])
    beta  <- unname(fit["beta"])
    ntop  <- attr(fit,"ntop")
    zeta  <- attr(fit,"zeta")
    ndat  <- ndata(fit)
    print(finfo(alpha,beta,ntop,zeta,ndat))
    print(aHess(fit)) # Same as above.
}
X     <- hrsRcePred
top1e <- X[X$sbjType=="Expert","top1"]
fit <- mleDb(top1e,ntop=10,zeta=TRUE)
print(finfo(alpha=unname(fit["alpha"]),beta=unname(fit["beta"]),
            ntop=10,zeta=TRUE,ndata=ndata(fit)))
# Gives the expected Fisher info evaluated at the maximum
# likelihood parameter estimates.
print(aHess(fit)) # Same as above.
}

\concept{ Fisher information }
\concept{ covariance estimation }
\concept{ inference }
