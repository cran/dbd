\name{makeBbdpars}
\alias{makeBbdpars}
\title{
    Create an object of class \code{"Bbdpars"}.
}
\description{
    Create an object of class \code{"Bbdpars"} which may be used
    as an argument of the \code{simulate()} function.
}
\usage{
makeBbdpars(m, s, size, ndata)
}
\arguments{
  \item{m}{
  Numeric scalar between 0 and 1.  May be interpreted as a
  \dQuote{success probability}.

}
  \item{s}{
  Numeric scalar, greater than 0.  The overdispersion parameter
  of the beta binomial distribution.  Note that if overdispersion
  is defined to equal the ratio of the variance of the data to
  the corresponding \dQuote{binomial variance} (i.e.
  the actual variance over \code{m*(1-m)*size}) the overdispersion
  tends to 0 as \code{s} tends to infinity and to \code{size} as
  \code{s} tends to 0.
}
  \item{size}{
  Integer scalar specifying the upper limit of the \dQuote{support}
  of the beta binomial distribution under consideration.  The support
  is the set of integers \code{{0, 1, \dots, size}}.
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
  An object of class \code{"Bbdpars"} which is a list with components
  \code{m}, \code{s}, \code{size} and \code{ndata}.  The entries
  of this list are simply the corresponding function arguments.
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{simulate.Bbdpars}()}
}
\examples{
   obj1  <- makeBbdpars(m=0.35,s=0.3,size=20,ndata=500)
   obj2  <- makeBbdpars(m=0.85,s=1.7,size=20,ndata=30*(1:10))
   \dontrun{
       sdat1 <- simulate(obj1,nsim=100)
       sdat2 <- simulate(obj2,nsim=100)
   }
   sdat3 <- simulate(obj2,nsim=10)
   \dontrun{
       sdat4 <- simulate(obj2,nsim=100,ndata=100*(2:6)) # The ndata component of
                                                    # obj2 is ignored.
   }
}
\keyword{ utilities }
