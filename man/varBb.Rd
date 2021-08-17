\name{varBb}
\alias{varBb}
\alias{varBb.mleBb}
\alias{varBb.default}
\title{
    Variance of a beta binomial distribution.
}
\description{
    Calculate the variance of a random variable having a beta binomial
    distribution.
}
\usage{
varBb(mo,...)
\method{varBb}{mleBb}(mo,...)
\method{varBb}{default}(mo, s, size, ...)
}
\arguments{
  \item{mo}{
  For the \code{"mleBb"} method this argument is an object of class
  \code{"mleBb"} as returned by \code{\link{mleBb}()}.  For the
  default method it is a numeric scalar, between 0 and 1, playing the
  role of \code{m} (which may be interpreted as the \dQuote{success}
  probability).  (See the help for \code{dbetabinom()}.)
}
  \item{s}{
  Numeric scalar, greater than 0.  The overdispersion parameter
  of the distribution. (See the help for \code{dbetabinom()}.)
}
  \item{size}{
  Integer scalar specifying the upper limit of the \dQuote{support}
  of the beta binomial distribution under consideration.  The support
  is the set of integers \code{{0, 1, \dots, size}}.  (See the help
  for \code{dbetabinom()}.)
}
  \item{\dots}{
  Not used.
}
}
\details{
   For the \code{"mleBb"} method, the single argument should really
   be called (something like) \dQuote{\code{object}} and for the
   default method the first argument should be called \code{m}.
   However the argument lists must satisfy the restrictions that
   \dQuote{\emph{A method must have all the arguments of the generic,
   including \dots if the generic does.}} and \dQuote{\emph{A method
   must have arguments in exactly the same order as the generic.}}

   For the \code{"mleBb"} method, the values of \code{m} and \code{s}
   are obtained from \code{mo}, and \code{size} is extracted from
   the attributes of \code{mo}.

   The variance of a beta binomial distribution is readily calculable
   \dQuote{by hand}.  These functions are provided for convenience
   and to preserve parallelism with the db distribution.
}
\value{
Numeric scalar equal to the variance of a beta binomial distributed
random variable with the given parameters.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
    \code{\link{varDb}()}
    \code{\link{expValDb}()}
    \code{\link{expValBb}()}
}
\examples{
   varBb(0.7,0.1,15)
   varBb(0.7,400,15)
   X   <- hmm.discnp::Downloads
   fit <- mleBb(X,size=15)
   varBb(fit)
}
\keyword{math}
\keyword{univar}
\concept{variance}
