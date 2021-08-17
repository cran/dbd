\name{gof}
\alias{gof}
\alias{gof.mleDb}
\alias{gof.mleBb}
\title{
   Goodness of fit test for db and beta binomial distributions.
}
\description{
   Either a chi-squared or a Monte Carlo test of goodness
   of fit of a db distribution.
}
\usage{
gof(object, obsd, ...)
\method{gof}{mleDb}(object,obsd,...,test=TRUE,MC=FALSE,seed=NULL,
                    nsim=99,maxit=1000,verb=FALSE)
\method{gof}{mleBb}(object,obsd,...,test=TRUE,MC=FALSE,seed=NULL,
                    nsim=99,maxit=1000,verb=FALSE)
}
\arguments{
  \item{object}{
  An object of class \code{"mleDb"} or \code{"mleBb"} as returned by the function
  \code{\link{mleDb}()} or by \code{\link{mleBb}()}.
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
  chi squared test?
}
  \item{seed}{
  Integer scalar.  The seed for the random number generator used
  when \code{MC} is \code{TRUE}.  If not supplied, \code{seed} is
  created by sampling one integer from \code{1:1e5}.  This argument
  is ignored if \code{MC} is \code{FALSE}.
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
  \item{verb}{
  Logical scalar.  Should rudimentary \dQuote{progress reports} be
  issued during the course of the simulations invoked by the Monte
  Carlo test procedure?  Ignored if \code{MC} is \code{FALSE}.
}
}
\details{

  The function \code{gof()} is a generic function with two methods,
  \code{gof.mleDb()} and \code{gof.mleBb()}.

  The test statistic is calculated as \deqn{\sum((O-E)^2/E)}{Sum((O-E)^2/E)}
  where \eqn{O} means \dQuote{observed} and \eqn{E} means \dQuote{expected}.
  If the mean of \eqn{E} is less than 5 or if any of the entries of \eqn{E}
  is less than 1, then the chi squared test is invalid and a warning to this
  effect is issued.  In this case the expected values are returned as an
  attribute of the value returned by \code{gof()}.  The foregoing applies
  of course only if a chi squared test (as opposed to a Monte Carlo test)
  is being used.

  The degrees of freedom for the chi squared test are \code{length(E) - 3}.
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

\section{Remark on the Examples}{
In the \bold{Examples}, db and beta binomial distributions are
fitted to the \emph{Parsonnet scores} from the \code{cardiacsurgery}
data set which comes from the \code{spcadjust} package.  It is not
completely clear what the value of \code{ntop} (db distribution)
or \code{size} (beta binomial distribution) should be.  The data
are not actually counts, and in particular they are not counts
of successes out of a given number (\dQuote{\code{size}}) of trials.
In the event I chose to use the value 71, the maximium value of the
Parsonnet scores, for the value of both \code{ntop} and \code{size}.
This was the value chosen for use as \code{size} by Wittenberg
(2021) when he fitted a beta binomial distribution to these data.
}

\value{
  A list with components
  \item{stat}{The test statistic.}
  \item{pval}{The p-value of the test.}
  \item{degFree}{The degrees of freedom of the chi squared test.}

  The last component is present only if a chi squared test (rather
  than a Monte Carlo test) is used.

  If a chi squared test is used and turns out to be invalid, then
  the returned value has an attribute \code{"expVals"}, consisting
  of the (problematic) expected values.

  If a Monte Carlo test is used the returned value has an attribute
  \code{"seed"} which is equal to the \code{seed} argument or to the
  random value selected to replace it if the \code{seed} argument was
  not supplied.
}

\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\references{
  Philipp Wittenberg (2021). Modeling the patient mix for
  risk-adjusted CUSUM charts.  To appear in \emph{Statistical
  Methods in Medical Research}.

  Axel Gandy and Jan Terje Kvaloy (2013). Guaranteed
  conditional performance of control charts via bootstrap
  methods. \emph{Scandinavian Journal of Statistics} \bold{40},
  pp. 647--668.  (Reference for \code{spcadjust} package.)
}

\seealso{
  \code{\link{mleDb}()}
}
\examples{
X <- hmm.discnp::Downloads
f <- mleDb(X,15,TRUE)
tst1 <- gof(f,X) # Gives warning that the chi squared test is invalid.
tst2 <- gof(f,X,MC=TRUE,seed=42)
# The p-value is 0.03 so we reject the adequacy of the fit at the 0.05
# significance level.  Note that the p-value that we get, when the
# random number generator seed is set equal to 42, is very similar in
# value to the p-value (0.0347) from the "invalid" chi squared test.
#
\dontrun{ # Takes too long.
if(requireNamespace("spcadjust")) {
    data("cardiacsurgery", package = "spcadjust")
    xxx  <- cardiacsurgery$Parsonnet
    fit1 <- mleDb(xxx,ntop=71,zeta=TRUE)
    g1   <- gof(fit1,obsd=xxx,MC=TRUE,verb=TRUE,seed=42)
    fit2 <- mleBb(xxx,size=71)
    g2   <- gof(fit2,obsd=xxx,MC=TRUE,verb=TRUE,seed=17)
}
}
}
\keyword{ htest }
