\name{exactMeDb}
\alias{exactMeDb}
\title{
   Exact moment estimates for the db distribution.
}
\description{
   Attempts to calculate \dQuote{exact} moment estimates of the
   parameters of a db distribution.  This is done by minimising
   the sum of squared differences between the sample mean and variance
   (\code{xbar} and \code{s2}) and the theoretical mean and variance.
   Calls upon \code{\link{optim}()} with the \code{"BFGS"} method.
}
\usage{
exactMeDb(x, ntop, zeta=FALSE, par0 = NULL, maxit = 1000)
}
%- maybe also 'usage' for other objects documented here.
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
  supplied starting values are calculated using the undocumented function
  \code{meDb()}.
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
}
\details{
  This function is really an \dQuote{intellectual curiosity}.  The
  results produced may be compared with those produced via maximum
  likelihood (using \code{\link{mleDb}()}) which in theory should
  be \dQuote{better}.  Since numerical optimisation has to be applied
  to calculate the \dQuote{exact} moment estimates, there is no
  real saving in terms of computation cost.
}
\value{
  An object of class \code{"exactMeDb"}.  Such an object consists
  of a named vector with entries \code{"alpha"} and \code{"beta"},
  which are the \dQuote{exact} moment estimates of the corresponding
  parameters.  It has a number of attributes:
  \itemize{
      \item \code{"ntop"} The value of the \code{ntop} argument.
      \item \code{"zeta"} The value of the \code{zeta} argument.
      \item \code{"minSqDiff"} The (minimised) value of the sum of
      the squared differences between the sample mean and variance
      (\code{xbar} and \code{s2}) and the theoretical mean and
      variance.  Ideally this minimised value should be zero.
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
   \code{\link{mleDb}()}
   \code{\link{expValDb}()}
   \code{\link{varDb}()}
   \code{\link{optim}()}
}

\examples{
set.seed(42)
x <- rdb(500,3,5,2)
eMom <- exactMeDb(x,ntop=2,zeta=FALSE)
eMle <- mleDb(x,ntop=2)

# Get much better results using true parameter values
# as starting values; pity we can't do this in real life!
eMom <- exactMeDb(x,ntop=2,zeta=FALSE,par0=c(alpha=3,beta=5))
eMle <- mleDb(x,2,par0=c(alpha=3,beta=5))

# Larger ntop value
x <- rdb(500,3,5,20)
eMom <- exactMeDb(x,ntop=20,zeta=FALSE)
eMle <- mleDb(x,ntop=20)

# Binomial, n = 10, p = 0.3.
set.seed(42)
x    <- rbinom(1000,10,0.3)
eMom <- exactMeDb(x,ntop=10,zeta=TRUE)
eMle <- mleDb(x,ntop=10,zeta=TRUE)
p1   <- dbinom(0:10,10,0.3)
p2   <- dbinom(0:10,10,mean(x)/10)
p3   <- table(factor(x,levels=0:10))/1000
p4   <- ddb(0:10,alpha=eMom["alpha"],beta=eMom["beta"],ntop=10,zeta=TRUE)
plot(eMle,obsd=x,legPos=NULL,ylim=c(0,max(p1,p2,p3,p4)))
lines(0.2+(0:10),p1,col="orange",type="h",ylim=c(0,max(p1,p2)))
lines(0.3+(0:10),p2,col="green",type="h")
legend("topright",lty=1,col=c("red","blue","orange","green","black"),
       legend=c("dbMle","observed","true binomial","fitted binomial","dbMom"),bty="n")
}
\concept{ estimation }
