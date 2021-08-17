\name{varDb}
\alias{varDb}
\alias{varDb.mleDb}
\alias{varDb.default}
\title{
    Variance of a db distribution.
}
\description{
    Calculate the variance of a random variable having a db
    distribution.
}
\usage{
varDb(ao,...)
\method{varDb}{mleDb}(ao,...)
\method{varDb}{default}(ao, beta, ntop, zeta=FALSE,...)
}
\arguments{
  \item{ao}{
  For the \code{"mleDb"} method this argument is an object of class
  \code{"mleDb"} as returned by \code{\link{mleDb}()}.  For the
  default method it is a numeric scalar playing the role of
  \code{alpha} (see \code{\link{ddb}()}).
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
  \item{\dots}{
  Not used.
}
}
\details{
   For the \code{"mleDb"} method, the single argument should really
   be called (something like) \dQuote{\code{object}} and for the
   default method the first argument should be called \code{alpha}.
   However the argument lists must satisfy the restrictions that
   \dQuote{\emph{A method must have all the arguments of the generic,
   including \dots if the generic does.}} and \dQuote{\emph{A method
   must have arguments in exactly the same order as the generic.}}

   For the \code{"mleDb"} method, the values of \code{alpha} and
   \code{beta} are obtained from \code{ao}, and \code{ntop}, and
   \code{zeta} are extracted from the attributes of \code{ao}.

   The variance of a db distribution is theoretically intractable
   but is readily calculable numerically as \deqn{\sum (x - \mu)^2
   \times \Pr(X=x)}{sum((x-mu) * Pr(X=x))}, where \eqn{\mu}{mu}
   is the expected value of the given distribution.
}
\value{
Numeric scalar equal to the variance of a db distributed
random variable with the given parameters.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{ddb}()} \code{\link{expValDb}()}
}
\examples{
   varDb(3,4,15)
   varDb(3,4,15,TRUE)
   X   <- hmm.discnp::Downloads
   fit <- mleDb(X,ntop=15,zeta=TRUE)
   varDb(fit)
}
\keyword{math}
\keyword{univar}
\concept{variance}
