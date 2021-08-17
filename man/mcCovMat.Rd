\name{mcCovMat}
\alias{mcCovMat}
\alias{mcCovMat.mleDb}
\alias{mcCovMat.mleBb}
\alias{mcCovMat.Dbdpars}
\alias{mcCovMat.Bbdpars}
\alias{mcCovMat.default}
\title{
    Monte Carlo estimation of a covariance matrix.
}
\description{
    Calculate an estimate of the covariance matrix for the parameter
    estimates of a db or beta binomial distribution via simulation.
}

\usage{
mcCovMat(object, nsim = 100, seed=NULL, maxit=1000)
\method{mcCovMat}{mleDb}(object, nsim = 100, seed=NULL, maxit=1000)
\method{mcCovMat}{mleBb}(object, nsim = 100, seed=NULL, maxit=1000)
\method{mcCovMat}{Dbdpars}(object, nsim = 100, seed=NULL, maxit=1000)
\method{mcCovMat}{Bbdpars}(object, nsim = 100, seed=NULL, maxit=1000)
\method{mcCovMat}{default}(object, nsim = 100, seed=NULL, maxit=1000)
}
\arguments{
  \item{object}{
  An object of class either \code{"mleDb"}, \code{"mleBb"},
  \code{Dbdpars} or \code{Bbdpars}.  In the first two cases such an
  object would be returned by the function \code{\link{mleDb}()} or
  by \code{\link{mleBb}()}.  In the second two cases such an object
  would be returned by the function \code{\link{makeDbdpars}()}
  or by \code{\link{makeBbdpars}()}.
}
  \item{nsim}{
  Integer scalar.  The number of simulations to be used to produce
  the Monte Carlo estimate of the covariance matrix.
}
  \item{seed}{
  Integer scalar.  The seed for the random number generator.  If not
  specified it is randomly sampled from the sequence \code{1:1e5}.
}
  \item{maxit}{
  Integer scalar.  The maximum number of iterations to be undertaken
  by \code{\link{optim}()} when fitting models to the simulated data.
}
}
\details{
    The procedure is to simulate \code{nsim} data sets, all of
    the same size.  This will be the size of the data set to which
    \code{object} was fitted), in the case of the \code{"mleDb"} and
    \code{"mleBb"} methods, and will be the value of the \code{ndata}
    argument supplied to the \dQuote{\code{make}} function in the
    case of the \code{"Dbdpars"} and \code{"Bbdpars"} methods. The
    simulations are from models determined by the parameter value
    contained in \code{object}.

    From each such simulated data, parameter estimates are obtained.
    The covariance matrix of these latter parameter estimates
    (adjusted for the fact that the true parameters are known in
    a simulation) is taken to be the required covariance matrix
    estimated.

    The default method simply throws an error.
}
\value{
   A two-by-two positive definite (with any luck!) numeric matrix.
   It is an estimate of the covariance matrix of the parameter estimates.

   It has an attribute \code{"seed"} which is the seed that was used
   for the random number generator.  This is either the value of the
   argument \code{seed} or (if this argument was left \code{NULL}) the
   value that was randomly sampled from \code{1:1e5}.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{link{aHess}()}
  \code{link{nHess}()}
  \code{link{vcov.mleDb}()}
  \code{link{vcov.mleBb}()}
}
\examples{
X   <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X   <- split(X,f=with(X,interaction(locn,depth)))
x   <- X[[19]]$y
fit <- mleDb(x, ntop=5)
set.seed(42)
CM.m <- mcCovMat(fit,nsim=500) # Lots of simulations!
CM.a <- vcov(fit)
CM.n <- solve(nHess(fit,x))
cat("Monte Carlo:\n\n")
print(CM.m)
cat("Analytic:\n\n")
print(CM.a)
cat("Numeric:\n\n")
print(CM.n)
X     <- hrsRcePred
top1e <- X[X$sbjType=="Expert","top1"]
fit   <- mleBb(top1e,size=10)
CM.m  <- mcCovMat(fit,nsim=500) # Lots of simulations!
CM.a  <- vcov(fit)
CM.n  <- solve(nHess(fit,top1e))
cat("Monte Carlo:\n\n")
print(CM.m)
cat("Analytic:\n\n")
print(CM.a)
cat("Numeric:\n\n")
print(CM.n)
}

\concept{ covariance estimation }
\concept{ inference }
