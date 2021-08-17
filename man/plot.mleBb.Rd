\name{plot.mleBb}
\alias{plot.mleBb}
\title{
    Plot a maxium likelihood fit to data from a beta binomial distribution.
}
\description{
    Creates a plot of type \code{"h"} of the probabilities of each
    possible \code{x} value of a beta binomial distribution where
    the probabilities are calculated on the basis of parameters
    estimated by the function \code{mleBb()}.  If \code{obsd}
    is supplied it also superimposes/juxtaposes vertical lines
    representing the observed proportions.
}
\usage{
\method{plot}{mleBb}(x, ..., plot = TRUE, col.fit = "red", col.obsd = "blue",
              tikx = NULL, xlim=NULL, ylim=NULL, xlab = NULL,
              ylab = NULL, obsd = NULL, incr = NULL,
              main = "", legPos = "topright")
}
\arguments{
  \item{x}{
   An object of class \code{"mleBb"} as returned by the function
   \code{\link{mleBb}()}
}
  \item{\dots}{
  Not used.
}
  \item{plot}{
  Logical scalar; should a plot be produced (or should the function
  simply return a data frame consisting of the relevant values)?
}
  \item{col.fit}{
  The colour for the (vertical) lines corresponding to the
  \dQuote{fitted} probabilities, i.e. the probabilities calculated
  from the fitted parameters.
}
  \item{col.obsd}{
  The colour for the (vertical) lines corresponding to the
  \dQuote{observed} probabilities (proportions), i.e. the
  probabilities calculated by tabulating the data (from which the
  parameters were estimated.  Ignored if \code{obsd} is not supplied.
}
  \item{tikx}{
  (Optional) vector of locations of the tick marks on the \code{x}-axis.
}
  \item{xlim}{A numeric vector of length 2 specifying the limits
  of the \code{x}-axis.  Defaults to \code{c(0,size)} Note that
  \ code{size} is extracted from argument \code{x}.

}
  \item{ylim}{A numeric vector of length 2 specifying the limits
  of the \code{y}-axis.  There is a \dQuote{sensible} default.
}
  \item{xlab}{
  A label for the \eqn{x}-axis; defaults to \code{x}.
}
  \item{ylab}{
  A label for the \eqn{y}-axis; defaults to \code{probability}.
}
  \item{obsd}{
  The data set from which the parameters were estimated, i.e. from
  which \code{x} was obtained.  (Optional.)
}
  \item{incr}{
  Numeric scalar; defaults to 0.1 if \code{size} (extracted from
  \code{x}) is less than 20 and to 0.5 otherwise.  This number should
  be non-zero and less than 1 in absolute value.  (One would usually
  want it to be positive, but it could conceiveably be set to a
  small negative value.) It gives the value of the \dQuote{increment}
  or \dQuote{shift} that separates the vertical lines representing
  the fitted probabilities and those representing the observed
  proportions (calculated from \code{obsd}).  Ignored if \code{obsd}
  is not supplied.
}
  \item{main}{
  A main title for the plot; defaults to the empty string.
}
  \item{legPos}{
  A list with components \code{x} and \code{y}, or a text string,
  specifying the placement of the legend.  See \code{\link{legend}()}
  for details.   A legend is plotted only if \code{obsd} is
  specified, whence \code{legPos} is otherwise ignored.  The plotting
  of a legend may be suppressed (even when \code{obsd} is supplied)
  by setting \code{legPos=NULL}.
}
}
\value{
  A data frame with numeric columns \code{x}, \code{p} and
  possibly \code{po}.  The \code{x} column consists of the integers
  from 0 to \code{size}.  The \code{p} column consists of the
  appropriate probabilities of the \code{x} values, calculated by
  \code{dbetabinom()} from the \code{rmutil} package.  The \code{po}
  column is present only if \code{obsd} is supplied and consists
  of the observed proportions.  The value is returned invisibly.
  A plot is produced as a side-effect if \code{plot} is \code{TRUE}.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\section{Note}{
   This function calls \code{plotBb()} to do the heavy lifting.
}

\section{Warning}{
   It is up to the user to make sure that the \code{obsd} argument,
   if specified, is indeed the data set from which the object \code{x}
   was calculated.
}

\seealso{
   \code{\link{mleBb}()}
   \code{\link{plotBb}()}
}
\examples{
   if(require(hmm.discnp)) {
      xxx <- hmm.discnp::Downloads
      fit <- mleBb(xxx,size=14)
      plot(fit)
      plot(fit,obsd=xxx)
      plot(fit,obsd=xxx,legPos=list(x=3,y=0.25))
      plot(fit,obsd=xxx,legPos=NULL) # No legend is plotted.
   }
   set.seed(42)
   yyy <- rbinom(300,10,0.7)
   fit <- mleBb(yyy,size=10)
   plot(fit,obsd=yyy,legPos="topleft")
}

\keyword{ hplot }
