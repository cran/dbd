\name{mleDb}
\alias{mleDb}
\title{
    Maximum likelihood estimates of db parameters.
}
\description{
    Calculates maximum likelihood estimates of the \code{alpha} and
    \code{beta} parameters of a db distribution.  Calls upon
    \code{\link{optim}()} with the \code{"BFGS"} method.
}
\usage{
mleDb(x, ntop, zeta=FALSE, par0=NULL, UB=10, maxit=1000,
      covmat=TRUE, useGinv=FALSE)
}
\arguments{
  \item{x}{
  A random sample from the db distribution whose parameters are
  being estimated.  Missing values are \emph{allowed}.
}
  \item{ntop}{
  The \code{ntop} parameter of the db distribution whose parameters are
  being estimated.  I.e. it is the maximum possible value of the distribution,
  whose values are integers between 1 and \code{ntop}, or between 0 and
  \code{ntop} if \code{zeta} (see below) is \code{TRUE}.
}
  \item{zeta}{
  See \code{\link{ddb}()}.
}
  \item{par0}{
  Optional starting values for the iterative estimation procedure.
  A vector with entries \code{alpha} and \code{beta}.  Ideally this
  vector should be named; if not it is \emph{assumed} that the
  entries are in the order \code{alpha}, \code{beta}.  If not
  supplied starting values are calculated using \code{\link{meDb}()}.
}
  \item{UB}{
  Positive numeric scalar, providing an upper bound on the starting
  values used by \code{mleDb()}.  It appears that if these starting
  values are too large (it is not clear \emph{how} large) then \code{optim()}
  will throw an error.  This bound is ignored if \code{par0} is
  supplied.
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
  The \code{ntop} and \code{zeta} parameters must be supplied; they
  are not formally estimated (although the choice of \code{ntop}
  may be influenced by the data --- see below).  The parameter
  \code{zeta} has a default value, \code{FALSE}.

  If the generating mechanism from which the observed data \code{x}
  arose has a (known) theoretical least upper bound, then \code{ntop}
  should probably be set equal to this upper bound.  If the data
  are theoretically unbounded, then \code{ntop} should probably
  be set equal to \code{1 + max(x)}.  In this case \eqn{\Pr(X =
  \textrm{ntop})}{Pr(X=ntop)} should probably be interpreted
  as \eqn{\Pr(X \geq \textrm{ntop})}{Pr(X >= ntop)}.  Otherwise
  \code{ntop} should should probably be set equal to \code{max(x)}.
  The choice depends on circumstances and is up to the user.

  Missing values are removed from \code{x} before it is passed to
  \code{optim()}.  (Note that \code{ddb()} doesn't mind missing
  values but returns missing values when evaluated at them.
  This in turn produces a missing value for the log likelihood.)

  In previous versions of this package (0.1-17 and earlier)
  \code{optim()} was called with method \code{"L-BFGS-B"}.
  The change was made possible by the fact that, with the new
  \dQuote{direct} version of \code{\link{ddb}()}, it is no longer
  necessary to bound the parameters away from (above) zero.
}

\value{
  An object of class \code{"mleDb"}.  Such an object consists of a
  named vector with entries \code{"alpha"} and \code{"beta"}, which
  are the estimates of the corresponding parameters.  It has a
  number of attributes:
  \itemize{
  \item \code{"ntop"} The value of the \code{ntop} argument.
  \item \code{"zeta"} The value of the \code{zeta} argument.
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

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
   \code{\link{ddb}}
   \code{\link{meDb}()}
   \code{\link{optim}()}
   \code{\link{aHess}()}
   \code{\link{vcov.mleDb}()}
}

\examples{
set.seed(42)
x <- rdb(500,3,5,2)
ests <- mleDb(x,2)  # Bad!  Mind you, 2 is a "bad" value for ntop!
                    # Hessian is singular; covMat is NA.
# Get much better results using true parameter values
# as starting values; pity we can't do this in real life!
ests <- mleDb(x,2,par0=c(alpha=3,beta=5))
x <- rdb(500,3,5,20)
ests <- mleDb(x,20) # Pretty good.
print(vcov(ests))

# Binomial, n = 10, p = 0.3.
set.seed(42)
x   <- rbinom(1000,10,0.3)
fit <- mleDb(x,10,zeta=TRUE)
print(vcov(fit))
p1  <- dbinom(0:10,10,0.3)
p2  <- dbinom(0:10,10,mean(x)/10)
p3  <- table(factor(x,levels=0:10))/1000
plot(fit,obsd=x,legPos=NULL,ylim=c(0,max(p1,p2,p3,
     ddb(0:10,fit[1],fit[2],10,zeta=TRUE))))
lines(0.2+(0:10),p1,col="orange",type="h",ylim=c(0,max(p1,p2)))
lines(0.3+(0:10),p2,col="green",type="h")
legend("topright",lty=1,col=c("red","blue","orange","green"),
       legend=c("db","observed","true binomial","fitted binomial"),bty="n")
print(attr(fit,"log.like")) # -1778.36
print(sum(dbinom(x,10,mean(x)/10,log=TRUE))) # -1777.36
# Slightly better fit with only one estimated parameter,
# but then binomial is the true distribution, so you'd
# kind of expect a better fit.
print(sum(dbinom(x,10,0.3,log=TRUE))) # -1778.37

# Poisson mean = 5
set.seed(42)
x    <- rpois(1000,5)
fit  <- mleDb(x,14,zeta=TRUE) # max(x) = 13, take ntop = 1+13
print(vcov(fit))
p1   <- c(dpois(0:13,5),1-ppois(13,5))
lhat <- mean(x)
p2   <- c(dpois(0:13,lhat),1-ppois(13,lhat))
plot(fit,obsd=x,legPos=NULL,ylim=c(0,max(p1,p2,p3,
     ddb(0:14,fit[1],fit[2],14,zeta=TRUE))))
lines(0.2+0:14,p1,col="orange",type="h")
lines(0.3+(0:14),p2,col="green",type="h")
legend("topright",lty=1,col=c("red","blue","orange","green"),
       legend=c("db","observed","true Poisson","fitted Poisson"),bty="n")
print(attr(fit,"log.like")) # -2198.594
print(sum(dpois(x,lhat,log=TRUE))) # -2197.345
# Slightly better fit with only one estimated parameter,
# but then Poisson is the true distribution, so you'd
# kind of expect a better fit.
print(sum(dpois(x,5,log=TRUE))) # -2198.089
}
