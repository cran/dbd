\name{simulateDbd}
\alias{simulateDbd}
\alias{simulate.mleDb}
\alias{simulate.Dbdpars}
\title{
    Simulate data from a fitted db distribution.
}
\description{
    Simulate one or more data sets from a db
    distribution.  The parameters may be equal to those obtained
    from fitting such a distibution to data using \code{mleDb()}.
    Alternatively they may be specified in an
    object of class \code{"Dbdpars"}.
}
\usage{
   \method{simulate}{mleDb}(object, nsim = 1, seed = NULL, ...,
                             ndata = NULL, drop = TRUE)
   \method{simulate}{Dbdpars}(object, nsim = 1, seed = NULL, ...,
                             ndata = NULL, drop = TRUE)
}
\arguments{
  \item{object}{
  An object of class \code{"mleDb"} as returned by \code{\link{mleDb}()},
  or of
  class \code{"Dbdpars"}.  Objects of this latter class may be conveniently
  constructed using the function \code{\link{makeDbdpars}()}.
}
  \item{nsim}{
  The number of data sets to simulate.
}
  \item{seed}{
  Integer scalar specifying the seed for random number generation.
  If \code{seed} it is not supplied it is created by sampling
  from \code{1:1e5}.
}
  \item{\dots}{
  Not used.
}
  \item{ndata}{
  Integer vector specifying the lengths of the data sets to
  be simulated.  If it is of length less than \code{nsim} it is
  \dQuote{recycled} to provide a vector of length \code{nsim}.
  If is longer than \code{nsim}, then only the first \code{nsim}
  entries are used and the others are ignored.  If \code{ndata}
  is not supplied it is taken to be equal to the \code{"ndata"}
  attribute of \code{object} (i.e. the length of the data set from
  which the parameters in \code{object} were estimated.
}
  \item{drop}{
  Logical scalar; if \code{TRUE} and if \code{nsim==1} then
  this function simply returns the simulated data set (an
  integer vector) rather than a list of length 1 whose sole
  entry is that data set.  If \code{nsim>1} then \code{drop}
  is ignored.
}
}

\details{
  The actual simulation is done by \code{\link{rdb}()}.
}

\value{
  A list, of length \code{nsim}, whose entries are integer
  vectors, the length of of the \code{i}th entry being equal to
  \code{ndata[i]}.  If \code{nsim==1} and if \code{drop} is
  \code{TRUE}, then the value is simply an integer vector
  (of length \code{ndata[1]}).

  The returned value has an attribute \code{"seed"} equal to the
  value of the seed that was set before the generation of the
  samples was initiated.  This attribute is equal to the value of
  the \code{seed} argument if this was supplied
}

\seealso{
  \code{\link{simulate}()}
  \code{\link{rdb}()}
  \code{\link{makeDbdpars}()}
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\examples{
if(requireNamespace("hmm.discnp")) {
    X   <- hmm.discnp::Downloads
    fit <- mleDb(X,ntop=15,zeta=TRUE)
    s1  <- simulate(fit)
    s2  <- simulate(fit,nsim=5) # All data sets of length 267.
    s3  <- simulate(fit,nsim=5,ndata=100*(2:6))
}
}
\keyword{datagen}
