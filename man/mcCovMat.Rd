\name{mcCovMat}
\alias{mcCovMat}
\title{
    Monte Carlo estimation of a covariance matrix.
}
\description{
    Calculate an estimate of the covariance matrix for the parameter
    estimates of a db distribution via simulation.
}

\usage{
mcCovMat(object, nsim = 100, seed=NULL, maxit=1000)
}
\arguments{
  \item{object}{
  An object of class \code{"mleDb"} as returned by the function
  \code{\link{mleDb}()} or an object of class \code{"Dbdpars"},
  possibly as created by \code{\link{makeDbdpars}()}.
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
    The procedure is to simulate \code{nsim} data sets (all of the
    same size.  If \code{object} is of class \code{"mleDb"} then this
    size is the same as that of the data set to which \code{object} was fitted).
    If \code{object} is of class \code{"Dbdpars"} then this
    size is the \code{"ndata"} component of \code{object}.
    The simulation is based on the parameter estimates, or specified
    values, contained in \code{object}.

    From each such simulated data set, parameter estimates are obtained.
    The covariance matrix of these latter parameter estimates is
    taken to be the required covariance matrix estimated.
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
  \code{\link{aHess}()}
  \code{\link{nHess}()}
  \code{\link{vcov.mleDb}()}
  \code{\link{makeDbdpars}()}
}
\examples{
\dontrun{ # Takes too long for CRAN.
    if(requireNamespace("hmm.discnp")) {
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
    }
    X     <- hrsRcePred
    top1e <- X[X$sbjType=="Expert","top1"]
    fit   <- mleDb(top1e,ntop=10,zeta=TRUE)
    CM.m  <- mcCovMat(fit,nsim=500,maxit=5000)
    CM.a  <- vcov(fit)
    CM.n  <- solve(nHess(fit,top1e))
    cat("Monte Carlo:\n\n")
    print(CM.m)
    cat("Analytic:\n\n")
    print(CM.a)
    cat("Numeric:\n\n")
    print(CM.n)
    obj <- makeDbdpars(alpha=2.5,beta=4,ntop=10,zeta=TRUE,ndata=30)
    CM.m <- mcCovMat(obj,nsim=500)
    CM.f <- solve(do.call(finfo,obj))
    cat("Monte Carlo:\n\n")
    print(CM.m)
    cat("From Fisher info:\n\n")
    print(CM.f)
}
}

\concept{ covariance estimation }
\concept{ inference }
