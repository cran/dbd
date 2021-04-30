nHess <- local({
#
# Numerical (finite differencing based) approximation to
# the hessian, as effected by optimHess().
#
# Note that the following function calculates the negative log
# likelihood as *minimised* by mleDb().  Hence the (estimated)
# covariance matrix is the inverse of the (approximate) hessian
# returned by this function and not of the negative of this
# approximate hessian.
#

Fun <- function(par,x,ntop,zeta){
    -sum(log(ddb(x=x[!is.na(x)],alpha=par[1],
                 beta=par[2],ntop=ntop,zeta=zeta)))
}

function(object,x,silent=TRUE) {
    if(!inherits(object,"mleDb")) {
        whinge <- paste0("Argument \"object\" must be of class \"mleDb\".\n")
        stop(whinge)
    }
    parz <- as.vector(object)
    names(parz) <- c("alpha","beta")
    ntop <- attr(object,"ntop")
    zeta <- attr(object,"zeta")
    hiss <- try(optimHess(parz,fn=Fun,ntop=ntop,zeta=zeta,x=x),
                    silent=silent)
    if(inherits(hiss,"try-error")) {
        whinge <- paste0("Function optimHess failed.  Perhaps try\n",
                         " using functions aHess() or mcCovMat().\n")
        stop(whinge)
    }
    H           <- hiss
    pnms        <- names(parz)
    dimnames(H) <- list(pnms,pnms)
    H
}
})
