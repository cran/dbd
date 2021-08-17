\name{ndata}
\alias{ndata}
\title{
    Retrieve the \code{"ndata"} attribute of an \code{"mleDb"}
    object.
}
\description{
    Retrieve the number of (non-missing) values in the data set
    to which an \code{"mleDb"} object was fitted.
}
\usage{
ndata(object)
}
\arguments{
  \item{object}{
   An object of class \code{"mleDb"} as returned by \code{\link{mleDb}()}.
}
}
\value{
   Integer scalar equal to the number of (non-missing) values in
   the data set to which \code{object} was fitted.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
   \code{\link{mleDb}()}
}
\examples{
X <- hmm.discnp::SydColDisc
X$y <- as.numeric(X$y)
X <- split(X,f=with(X,interaction(locn,depth)))
fitz <- lapply(X,function(x){mleDb(x$y,ntop=5)})
sapply(fitz,ndata)
}
\keyword{ utilities }
