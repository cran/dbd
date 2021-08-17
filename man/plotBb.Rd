\name{plotBb}
\alias{plotBb}
\title{
    Plot a beta binomial distribution.
}
\description{
    Plots the probabilities of a specified beta binomial distributon.
}
\usage{
plotBb(m, s, size, ..., plot = TRUE, tikx = NULL, xlim = NULL,
        ylim = NULL, xlab = NULL, ylab = NULL, main = "")

}
\arguments{
  \item{m}{
  Numeric scalar between 0 and 1.  May be interpreted as the
  \dQuote{success probability}.
}
  \item{s}{
  Numeric scalar, greater than 0.  The overdispersion parameter
  of the distribution.
}
  \item{size}{
  Integer scalar specifying the upper limit of the \dQuote{support}
  of the beta binomial distribution under consideration.  The support
  is the set of integers \code{{0, 1, \dots, size}}. 
}
  \item{\dots}{
  Extra arguments that are passed to the \code{plot()} function.
}
  \item{plot}{
  Logical scalar; should a plot be produced (or should the function
  simply return a data frame consisting of the relevant values)?
}
  \item{tikx}{
  (Optional) vector of locations of the tick marks on the \code{x}-axis.
}
  \item{xlim}{
  The \code{x}-limits of the plot.  (See \code{\link{plot.default}()}.)
}
  \item{ylim}{
  The \code{y}-limits of the plot.  (See \code{\link{plot.default}()}.)
}
  \item{xlab}{
  A label for the \code{x}-axis. (See \code{\link{plot.default}()}.)
}
  \item{ylab}{
  A label for the \code{y}-axis. (See \code{\link{plot.default}()}.)
}
  \item{main}{
  An overall title for the plot. (See \code{\link{plot.default}()};
  see also \code{\link{title}()}.)
}
}
\value{
  A data frame with numeric columns \code{x} and \code{p}.  The
  \code{x} column consists of the integers from 0 to \code{size}.
  The \code{p} column consists of the appropriate probabilities of
  the \code{x} values, calculated by \code{dbetabinom()} from the
  \code{rmutil} package.  The value is returned invisibly.  A plot
  is produced as a side-effect if \code{plot} is \code{TRUE}.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
   \code{\link{plot.mleBb}()}
   \code{\link{plotDb}()}
   \code{\link{plot.mleDb}()}
}
\examples{
plotBb(0.7,3,14,main="An exempular plot")
plotBb(0.3,3,14,col="red",xlab="count",main="A communist plot")
plotBb(0.1,10,14,col="blue",main="A royal plot")
plotBb(0.5,20,14,col="green",main="An ecological plot")
plotBb(0.5,20,14,xlim=c(0,15))
plotBb(0.5,20,14,xlim=c(0,15),tikx=3*(0:5))
}

\keyword{ hplot }
