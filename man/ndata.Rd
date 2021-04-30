\name{ndata}
\alias{ndata}
\title{
    Retrieve the \code{"ndata"} attribute of an \code{"mleDb"} object.
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
if(requireNamespace("hmm.discnp")) {
    X    <- hmm.discnp::SydColDisc
    X$y  <- as.numeric(X$y)
    X    <- split(X,f=with(X,interaction(locn,depth)))
    fitz <- lapply(X,function(x){mleDb(x$y,ntop=5,covmat=FALSE)})
    sapply(fitz,ndata)
    \dontrun{ # Takes too long for CRAN.
        Y    <- hmm.discnp::ccprSim
        Ypr  <- lapply(Y,function(x){x[,2]})
        OK   <- sapply(Ypr,function(x){sum(!is.na(x)) > 0})
        Ypr  <- Ypr[OK]
    # Need the try() in the following since mleDb() falls over on some of
    # the data sets; not clear why.
        fitz <- lapply(Ypr,function(x){try(mleDb(x,ntop=4,zeta=TRUE,
                       covmat=FALSE),silent=TRUE)})
        ok   <- sapply(fitz,function(x){!inherits(x,"try-error")})
        fitz <- fitz[ok]
        sapply(fitz,ndata)
    }
}
set.seed(42)
obj <- makeDbdpars(alpha=0.2,beta=0.25,ntop=20,zeta=TRUE,
                 ndata=sample(200:600,100))
Y   <- simulate(obj,nsim=100)
fitz <- lapply(Y,mleDb,ntop=20,zeta=TRUE,covmat=FALSE)
sapply(fitz,ndata)
}
\keyword{ utilities }
