\name{makeDbdpars}
\alias{makeDbdpars}
\title{
    Create an object of class \code{"Dbdpars"}.
}
\description{
    Create an object of class \code{"Dbdpars"} which may be used
    as an argument of the \code{simulate()} function.
}
\usage{
makeDbdpars(alpha, beta, ntop, zeta, ndata)
}
\arguments{
  \item{alpha}{
  The first \dQuote{shape} parameter of the db distribution.
}
  \item{beta}{
  The second \dQuote{shape} parameter of the db distribution.
}
  \item{ntop}{
  Integer scalar, strictly greater than 1.  The maximum possible
  value of the db distribution.
}
  \item{zeta}{
  Logical scalar.  Should zero origin indexing be used?
  I.e. should the range of values of the distribution be taken to
  be \code{{0,1,2,...,ntop}} rather than \code{{1,2,...,ntop}}?
  Setting \code{zeta=TRUE} may be appropriate for example when the
  values of the distribution are to be interpreted as counts.
}
  \item{ndata}{
  Integer vector specifying the lengths of the data sets to
  be simulated.  If it is of length less than the \code{nsim}
  argument of \code{simulate()} (e.g. if it is a scalar) then it is
  \dQuote{recycled} to provide a vector of length \code{nsim}.  If is
  longer than \code{nsim}, then only the first \code{nsim} entries
  are used and the others are ignored.  If the argument \code{ndata}
  of the \code{simulate()} function is supplied then the \code{ndata}
  component specified here is ignored by \code{simulate()}.
}
}
\value{
  An object of class \code{"Dbdpars"} which is a list with
  components \code{alpha}, \code{beta}, \code{ntop}, \code{zeta}
  and \code{ndata}.  The entries of this list are simply the
  corresponding function arguments.
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{simulate.Dbdpars}()}
}
\examples{
   obj1  <- makeDbdpars(alpha=2,beta=3,ntop=20,zeta=TRUE,ndata=500)
   obj2  <- makeDbdpars(alpha=0.2,beta=0.25,ntop=20,zeta=FALSE,ndata=30*(1:10))
   sdat1 <- simulate(obj1,nsim=100)
   sdat2 <- simulate(obj2,nsim=100)
   sdat3 <- simulate(obj2,nsim=10)
   sdat4 <- simulate(obj2,nsim=100,ndata=100*(2:6)) # The ndata component of
                                                    # obj2 is ignored.
}
\keyword{ utilities }
