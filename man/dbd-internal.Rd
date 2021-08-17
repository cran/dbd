\name{dbd-internal}
\alias{grad}
\alias{gradDb}
\alias{gradBb}
\alias{hess}
\alias{hessDb}
\alias{hessBb}
\alias{mcCovMatEngine}
\alias{meDb}
\alias{meBb}
\title{Internal Verdis functions.}
\description{
  Internal Verdis functions.
}
\usage{
grad(x, distr=c("db","betabinom"),gpar)
gradDb(x,gpar)
gradBb(x,gpar)
hess(x, distr=c("db","betabinom"),hpar)
hessDb(hpar)
hessBb(x,hpar)
mcCovMatEngine(fitz,par0,seed)
meDb(x, ntop)
meBb(x, size, warn=FALSE)
}
\details{
  These functions are auxiliary and are not intended to be called
  by the user.
}
\keyword{internal}
