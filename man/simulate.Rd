\name{simulate}
\alias{simulate.mleDb}
\alias{simulate.mleBb}
\alias{simulate.Dbdpars}
\alias{simulate.Bbdpars}
\title{
    Simulate data from a db or beta binomial distribution.
}
\description{
    Simulate one or more data sets from a db or beta binomial
    distribution.  The parameters of the distribution may be
    equal to those obtained from fitting the distibution to data,
    using \code{mleDb()} or \code{mleBb()}.  They may also be
    specified by the user via the function \code{makeDbdpars()}
    or \code{makeBbdpars()}.

}
\usage{
   \method{simulate}{mleDb}(object, nsim = 1, seed = NULL, ...,
                             ndata = NULL, drop = TRUE)
   \method{simulate}{mleBb}(object, nsim = 1, seed = NULL, ...,
                             ndata = NULL, drop = TRUE)
   \method{simulate}{Dbdpars}(object, nsim = 1, seed = NULL, ...,
                             ndata = NULL, drop = TRUE)
   \method{simulate}{Dbdpars}(object, nsim = 1, seed = NULL, ...,
                             ndata = NULL, drop = TRUE)
}
\arguments{
  \item{object}{
  An object of class \code{"mleDb"} as returned by
  \code{\link{mleDb}()}, or of class \code{"mleBb"} as returned by
  \code{\link{mleBb}()}, or of class \code{"Dbdpars"} as returned
  by \code{\link{makeDbdpars}()} or of class \code{"Bbdpars"}
  as returned by \code{\link{makeBbdpars}()}.
}
  \item{nsim}{
  The number of data sets to simulate.
}
  \item{seed}{
  Integer vector of seeds for random number generation.  It should
  be of length either 1 or \code{nsim}.  If it is of length less
  than \code{nsim} then \code{set.seed(seed[1])} is called and
  \code{seed} is replaced by a vector of seeds of length \code{nsim}
  which is created by sampling from \code{1:1e5}.  Note that in this
  case all entries but the first of \code{seed} are \emph{ignored}.
  If it is longer than \code{nsim}, then only the first \code{nsim}
  entries are used and the others are ignored.  If \code{seed}
  it is not supplied it is created by sampling \code{nsim} values
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
  The actual simulation is done by \code{\link{rdb}()} or by the
  \code{rbetabinom()} function from the \code{rmutil} package.
}

\value{
  A list, of length \code{nsim}, whose entries are integer
  vectors, the length of of the \code{i}th entry being equal to
  \code{ndata[i]}.  Each entry has an attribute \code{"seed"} which
  is the random number generation seed that was used in the generation
  of the data in that entry.  If \code{nsim==1} and if \code{drop} is
  \code{TRUE}, then the value is simply an integer vector
  (of length \code{ndata[1]}).
}

\seealso{
  \code{\link{simulate}()} \code{\link{rdb}()}
}
\examples{
   X   <- hmm.discnp::Downloads
   fit <- mleDb(X,ntop=15,zeta=TRUE)
   s1  <- simulate(fit)
   s2  <- simulate(fit,nsim=5) # All data sets of length 267.
   s3  <- simulate(fit,nsim=5,ndata=100*(2:6))
   obj <- makeDbdpars(alpha=2,beta=3,ntop=20,zeta=TRUE,ndata=500)
   s4  <- simulate(obj,nsim=5,seed=1:5)
}
\keyword{datagen}
