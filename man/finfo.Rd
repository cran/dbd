\name{finfo}
\alias{finfo}
\title{
    Fisher information.
}
\description{
    Compute the Fisher information for a db distribution or
    a beta binomial distribution given the parameters of that
    distribution.  In the case of the db distribution a specified
    number of observations must be supplied.  In the case of
    the beta binomial distribution the actual observations must
    be supplied.  The inverse of the Fisher information is an
    estimate of the covariance matrix of the parameter estimates.
}
\usage{
finfo(distr=c("db","betabinom"),alpha, beta, ntop, ndata,
      zeta = FALSE, x, m, s, size)
}
\arguments{
  \item{distr}{
  Text string specifying which distribution to consisder.
  May be abbreviated (e.g. to \code{"d"} or \code{"b"}).
}
  \item{alpha}{
  See \code{\link{ddb}()}.  Ignored if \code{distr} is \code{"betabinom"}.
}
  \item{beta}{
  See \code{\link{ddb}()}.  Ignored if \code{distr} is \code{"betabinom"}.
}
  \item{ntop}{
  See \code{\link{ddb}()}.  Ignored if \code{distr} is \code{"betabinom"}.
}
  \item{ndata}{
  The number of observations for which the Fisher
  information is being determined.  Ignored if \code{distr}
  is \code{"betabinom"}; must be supplied if \code{distr}
  is \code{"db"}.
}
  \item{zeta}{
  See \code{\link{ddb}()}.  Ignored if \code{distr} is \code{"betabinom"}.
}
  \item{x}{
  A numeric vector of observations appropriate for the model
  under consideration.  Ignored if \code{distr} is \code{"db"}; the
  Fisher information for the db distribution depends only upon the
  parameters and not upon the data.  Must be supplied if \code{distr}
  is \code{"betabinom"}.
}
\item{m}{
  A numeric scalar, between 0 and 1, which may be interpreted
  as the \dQuote{success} probability.  (See the help for
  \code{dbetabinom()}.) Ignored if \code{distr} is \code{"db"}.
}
  \item{s}{
  Numeric scalar, greater than 0.  The overdispersion parameter of
  the distribution. (See the help for \code{dbetabinom()}.) Ignored
  if \code{distr} is \code{"db"}.
}
  \item{size}{
  Integer scalar specifying the upper limit of the \dQuote{support}
  of the betabinom distribution under consideration.  The support
  is the set of integers \code{{0, 1, \dots, size}}.  (See the help
  for \code{dbetabinom()}.) Ignored if \code{distr} is \code{"db"}.
}
}
\details{
   This function differs from \code{\link{aHess}()} in that its
   arguments are prescribed \dQuote{individually} rather than being
   extracted from an \code{"mleDb"} or \code{"mleBb"} object.
   This allows \code{finfo()} to be applied to \dQuote{true}
   parameters (where these are known) rather than estimated ones.

   Note that if \code{distr} is \code{"db"}, the number
   of observations must be supplied explicitly, whereas for
   \code{aHess()} this number is extracted from the \code{object}
   argument.  If \code{distr} is \code{"betabinom"} then a vector
   of actual observations must be supplied.

   If \code{distr} is \code{"db"} then \code{finfo()} in effect
   calculates the \emph{expected} information, since the information
   matrix does not depend on the parameters.  This is not the case
   if \code{distr} is \code{"betabinom"}.  If the parameters
   supplied are the maximum likelihood estimates based on the
   supplied vector of observations \code{x}, then the value returned
   by \code{finfoBb()} is the \emph{observed} Fisher information.
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
  \code{link{aHess}()}
  \code{link{nHess}()}
  \code{link{mleDb}()}
  \code{link{mleBb}()}
}
\examples{
print(finfo(alpha=0.6,beta=0.3,ntop=5,ndat=54))
X   <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X   <- split(X,f=with(X,interaction(locn,depth)))
x   <- X[[19]]$y
fit <- mleDb(x, ntop=5)
alpha <- fit["alpha"]
beta  <- fit["beta"]
ntop  <- attr(fit,"ntop")
zeta  <- attr(fit,"zeta")
ndat  <- ndata(fit)
print(finfo(alpha=alpha,beta=beta,ntop=ntop,ndat=ntop,zeta=zeta))
print(aHess(fit)) # Same
X     <- hrsRcePred
top1e <- X[X$sbjType=="Expert","top1"]
fit   <- mleBb(top1e,size=10)
print(finfo(distr="b",x=top1e,m=fit["m"],s=fit["s"],
            size=10)) # Observed Fisher info.
print(aHess(fit,x=top1e)) # Same
}

\concept{ Fisher information }
\concept{ covariance estimation }
\concept{ inference }
