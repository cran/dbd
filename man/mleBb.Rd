\name{mleBb}
\alias{mleBb}
\title{
    Maximum likelihood estimation of the parameters of a beta binomial
    distribution.
}
\description{
    Calculates maximum likelihood estimates of the \code{m} and
    \code{s} parameters of a beta binomial distribution.  Calls upon
    \code{\link{optim}()} with the \code{"L-BFGS-B"} method.
}
\usage{
    mleBb(x, size, par0, maxit=1000, covmat=TRUE, useGinv=FALSE)
}
\arguments{
  \item{x}{
  Integer vector of counts to which a beta binomial distribution
  is to be fitted.  Missing values are allowed.  (These are discarded
  before the data are analysed.)
}
  \item{size}{
  Integer scalar specifying the upper limit of the \dQuote{support}
  of the beta binomial distribution under consideration.  The support
  is the set of integers \code{{0, 1, \dots, size}}. (The values
  of \code{x} may sometimes be considered to be the number of
  \dQuote{successes} in \code{size} trials.
}
  \item{par0}{
  Optional starting values for the iterative estimation procedure.
  A vector with entries \code{m} and \code{s}.  Ideally this vector
  should be named; if not it is \emph{assumed} that the entries
  are in the order \code{m}, \code{s}.  If not supplied starting
  values are calculated using \code{\link{meBb}()}.
}
  \item{maxit}{
  Integer scalar.  The maximum number of iterations to be undertaken
  by \code{\link{optim}()}.  What happens if this number is exceeded
  depends on the value of \code{options()[["maxitErrorOrWarning"]]}.
  This may be \code{"error"} (in which case an error is thrown if
  \code{maxit} is exceeded) or \code{"warning"} (in which case a
  warning is issued).  The values is set equal to \code{"error"} at
  startup.  It may be switched, from on possibility to the other,
  by means of the function \code{\link{set.eow}()}.
}
  \item{covmat}{
  Logical scalar.  Should the covariance matrix of the parameter
  estimates be calculated?  In simulation studies, in which the
  covariance matrix is not of interest, calculations might be
  speeded up a bit by setting \code{covmat=FALSE}.
}
  \item{useGinv}{
  Logical scalar.  Should the \code{ginv()} (generalised inverse)
  function from the \code{MASS} package be used to calculate a
  surrogate covariance matrix if the hessian is numerically singular?
  This is probably not advisable; the possibility of using the
  generalised inverse is provided for the sake of completeness.
  \emph{Caveat utilitor}.  This argument is ignored if \code{covmat}
  is \code{FALSE}.
}
}

\details{
  This function is provided so as to give a convenient means of
  comparing the fit of a beta binomial distribution with that of
  the discretised Beta (db) distribution which is the focus of
  this package.
}

\value{
   An object of class \code{"mleBb"} which is a vector of length two.
   Its first entry \code{m} is the estimate of the (so-called) success
   probability \code{m}; its second entry \code{s} is the estimate of the
   overdispersion parameter \code{s}.  It has a number of attributes:
  \itemize{
     \item \code{"size"} The value of the \code{size} argument.
     \item \code{"log.like"} The (maximised) value of the log likelihood
     of the data.
     \item \code{"covMat"} An estimate of the (\eqn{2 \times 2}{2 x 2})
     covariance matrix of the parameter estimates.  This is formed
     as the inverse of the hessian (of the negative log likelihood)
     calculated by \code{\link{aHess}()}.
     \item \code{ndata} The number of \emph{non-missing} values
     in the data set for which the likelihood was maximised,
     i.e. \code{sum(!is.na(x))}.
  }
}
   
\references{
  Bruce Swihart and Jim Lindsey (2020). rmutil: Utilities for Nonlinear
  Regression and Repeated Measurements Models. R package version 1.1.4.
  https://CRAN.R-project.org/package=rmutil

  Wikipedia, \url{https://en.wikipedia.org/wiki/Beta-binomial_distribution}
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
    \code{\link{mleDb}()}
    \code{\link{optim}()}
    \code{\link{aHess}()}
    \code{\link{vcov.mleBb}()}
    \code{\link{hrsRcePred}}
    \code{\link{visRecog}}
}
\examples{
if(require(hmm.discnp)) {
   X <- hmm.discnp::Downloads
   f <- mleBb(X,15)
}
set.seed(42)
X <- c(rbinom(20,10,0.3),rbinom(20,10,0.7))
f <- mleBb(X,10)
g <- mleDb(X,10,TRUE)
print(attr(f,"log.like"))
print(attr(g,"log.like")) # Not much difference.
dbfit5  <- with(visRecog,mleDb(tot5,20,TRUE))
print(vcov(dbfit5))
# See the help for data sets "hrsRcePred" and "visRecog" for
# other examples.
}

\keyword{ utilities }
