\name{gof}
\alias{gof}
\title{
   Goodness of fit test for db distributions.
}
\description{
   Either a chi-squared or a Monte Carlo test of goodness
   of fit of a db distribution.
}
\usage{
   gof(object,obsd,...,test=TRUE,MC=FALSE,nsim=99,maxit=1000)
}
\arguments{
  \item{object}{
  An object of class \code{"mleDb"} as returned by the function
  \code{\link{mleDb}()}.
}
  \item{obsd}{
  The data to which \code{object} was fitted.
}
  \item{...}{
  Not used.
}
  \item{test}{
  Logical scalar.  Should a hypothesis test be carried out?  If \code{test}
  is \code{FALSE} then only the test statistic is returned.  This argument
  is present so as to facilitate the calculations used in effecting a
  Monte Carlo test, by allowing \code{gof()} to recursively call itself.
}
  \item{MC}{
  Logical scalar.  Should a Monte Carlo test be used rather than a
  chi-squared test?
}
  \item{nsim}{
  The number of simulated replicates on which the Monte Carlo test is
  to be based.  Ignored if \code{MC} is \code{FALSE}.
}
  \item{maxit}{
  Integer scalar.  The maximum number of iterations to be undertaken
  by \code{\link{optim}()} when fitting models to the simulated data.
  Ignored if \code{MC} is \code{FALSE}.
}
}
\details{

  The test statistic is calculated as \deqn{\sum((O-E)^2/E)}{Sum((O-E)^2/E)}
  where \eqn{O} means \dQuote{observed} and \eqn{E} means \dQuote{expected}.
  If the mean of \eqn{E} is less than 5 or if any of the entries of \eqn{E}
  are less than 1, then the chi-squared test is invalid and a warning to this
  effect is issued.  In this case the expected values are returned as an
  attribute of the value returned by \code{gof()}.  The foregoing applies
  of course only if a chi-squared test (as opposed to a Monte Carlo test)
  is being used.

  The degrees of freedom for the chi-squared test are \code{length(E) - 3}.
  The value 3 is equal to 2 (for the number of parameters estimated) plus
  1 (for the costraint that the probabilities of the values sum to 1).

  If it were actually true that, under the null hypothesis, the
  observed test statistic and those calculated from simulated
  data are \emph{exchangeable}, the Monte Carlo test would
  be \emph{exact}.  However the real data are distributed as
  \eqn{f(x,\theta)}{f(x,theta)} whereas the simulated data
  are distributed as \eqn{f(x,\hat{\theta})}{f(x,theta.hat)}
  where \eqn{\hat{\theta}}{theta.hat} is the estimate of
  \eqn{\theta}{theta} based on the observed data.  Consequently the
  observed test statistic and simulated test statistics are
  \dQuote{not quite} exchangeable.  Nevertheless it appears that
  in practice the Monte Carlo test is very close to being exact.

  The meaning of \dQuote{exact} here is that if the null hypothesis
  is true then, over the set of instances of collecting the data
  \bold{and} simulating the required replicates, the \eqn{p}-value
  is uniformly distributed on the set \eqn{\{1/N, 2/N, \ldots,
  (N-1)/N, 1\}} where \eqn{N} is equal to \code{nsim}.
}

\section{Notes}{
  The Monte Carlo \eqn{p}-value is calculated as
  \code{(m+1)/(nsim+1)} where \code{m} is the number of simulated
  statistics which greater than or equal to the observed statistic
  (computed from the \dQuote{real} data.

  The \emph{smallest} that the Monte Carlo
  \eqn{p}-value can be is \code{1/(nsim + 1)}, e.g. 0.01 when
  \code{nsim} is 99.  For \dQuote{finer distinctions} you must use
  larger values of \code{nsim}, such as 999 or 9999.

  The \eqn{p}-value is \emph{random}; if you repeat the test (with
  the same data) you may well get a different \eqn{p}-value.
  Resist the temptation to repeat the test until you get a
  \eqn{p}-value that you like!!!  This invalidates your inference!
}
\value{
  A list with components
  \item{stat}{The test statistic.}
  \item{pval}{The p-value of the test.}
  \item{degFree}{The degrees of freedom of the chi-squared test.}

  The last component is present only if a chi-squared test (rather
  than a Monte Carlo test) is used.

  If a chi-squared test is used and turns out to be invalid, then
  the returned value has an attribute \code{"expVals"}, consisting
  of the (problematic) expected values.
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
  \code{\link{mleDb}()}
}
\examples{
X <- hmm.discnp::Downloads
f <- mleDb(X,15,TRUE)
tst <- gof(f,X) # Gives warning that the chi-squared test is invalid.
set.seed(42)
tst2 <- gof(f,X,MC=TRUE)
# The p-value is 0.03 so we reject the adequacy of the fit at the 0.05
# significance level.  Note that the p-value that we get, when the
# random number generator seed is set equal to 42, is very similar in
# value to the p-value (0.0347) from the "invalid" chi-squared test.
}
\keyword{ htest }
