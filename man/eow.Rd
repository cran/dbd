\name{eow}
\alias{set.eow}
\alias{get.eow}
\title{
    Set or query the value of the \code{"maxitErrorOrWarn"} option.
}
\description{
    Chooses (\code{set.eow()}) or queries (\code{get.eow()}) the
    reaction to \code{maxit} being exceeded in \code{mleDb()}.
    The possible reactions are to throw
    an error or to issue a warning.  The choice is effected
    by calling \code{set.eow()} which sets the value of
    \code{options()[["maxitErrorOrWarning"]]}.  The current choice
    is revealed by \code{get.eow()}.  This choice is set equal to
    \code{"error"} at startup.
}
\usage{
set.eow(eow = c("error", "warn"))
get.eow()
}
\arguments{
  \item{eow}{
  Character string that specifies the reaction to \code{maxit}
  being exceeded in \code{mleDb()}.  May be abbreviated.
}
}
\section{Note}{
  It seems unlikely that you would want to change the option from
  the value that is set at startup.  This function is provided
  \dQuote{just in case}.
}
\value{
  No value is returned by \code{set.eow()}.  the value of
  \code{"maxitErrorOrWarn"} in \code{options()}.
  The function \code{get.eow()} returns the current value of
  \code{options[["maxitErrorOrWarn"]]}.

}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
   \code{\link{mleDb}()}
   \code{\link{options}()}
}
\examples{
    get.eow()    # Is "error" at startup.
    set.eow("w") # Changes the option from "error" to "warning".
    set.eow("e") # Changes it back again.
}
\keyword{ utilities }
