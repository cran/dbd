finfo <- function(alpha,beta,ntop,zeta=FALSE,ndata) {
#
# Fisher information (equal to the hessian of the *negative*
# log likelihood) for the db distribution.
#
    hpar  <- c(alpha=alpha,beta=beta,ntop=ntop,zeta=zeta,ndata=ndata)
    hess(hpar)
}
