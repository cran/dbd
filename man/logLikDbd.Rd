\name{logLikDbd}
\alias{logLik.mleDb}
\title{
   Retrieve the (maximised) log likelihood from an \code{"mleDb"} object.
}
\description{
   Extract the log likelihood attribute an object of class
   \code{"mleDb"}.  I.e. obtain the maximum log likelihood in
   respect of the estimation of the parameters of a db distribution.
}
\usage{
   \method{logLik}{mleDb}(object, ...)
}
\arguments{
  \item{object}{
   An object of class \code{"mleDb"} as returned by \code{\link{mleDb}()}.
}
  \item{\dots}{
   Not used.
}
}
\value{
   An object of class \code{"logLik"}.  Such an object consists of
   a numeric scalar equal to the maximum of the log likelihood of
   the model being fitted.  In this instance the maximum is taken
   over the parameters of a db distribution which was fitted to
   a specified data set.  The object has an attribute \code{"df"}
   (\dQuote{degrees of freedom}) equal to the number of estimated
   parameters, in this instance 2.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
  \code{\link{mleDb}()}
}
\examples{
if(requireNamespace("hmm.discnp")) {
    X    <- hmm.discnp::SydColDisc
    X$y  <- as.numeric(X$y)
    X    <- split(X,f=with(X,interaction(locn,depth)))
    fitz <- lapply(X,function(x){mleDb(x$y,ntop=5)})
    sapply(fitz,logLik)
    X     <- hrsRcePred
    top1e <- X[X$sbjType=="Expert","top1"]
    fit   <- mleDb(top1e,ntop=10,zeta=TRUE)
    logLik(fit)
}
}
\keyword{ utilities }
