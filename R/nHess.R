nHess <- local({
#
# Numerical (finite differencing based) approximation to
# the hessian, as effected by optimHess().
#
# Note that the two following functions calculate the negative log
# likelihood as *minimised* by mleDb() or mleBb().  Hence the (estimated)
# covariance matrix is the inverse of the (approximate) hessian
# returned by this function and *not* of the negative of this
# approximate hessian.
#

FunDb <- function(par,x,ntop,zeta){
    -sum(log(ddb(x=x[!is.na(x)],alpha=par[1],
                 beta=par[2],ntop=ntop,zeta=zeta)))
}

FunBb <- function(par,x,size){
    -sum(log(rmutil::dbetabinom(y=x[!is.na(x)],m=par[1],
                 s=par[2],size=size)))
}

function(object,x,silent=TRUE) {
    parz <- as.vector(object)
    if(inherits(object,"mleDb")) {
        names(parz) <- c("alpha","beta")
        ntop <- attr(object,"ntop")
        zeta <- attr(object,"zeta")
        hiss <- try(optimHess(parz,fn=FunDb,ntop=ntop,zeta=zeta,x=x),
                    silent=silent)
    } else if(inherits(object,"mleBb")) {
        if(missing(x)) {
            whinge <- paste0("Argument \"x\" must be supplied when \"object\"",
                             "  is of class \"mleBb\".\n")
            stop(whinge)
        }
        names(parz) <- c("m","s")
        size <- attr(object,"size")
        hiss <- try(optimHess(parz,fn=FunBb,size=size,x=x),
                    silent=silent)
    } else {
        whinge <- paste0("Argument \"object\" must be either of class \"mleDb\"\n",
                         "  or of class \"mleBb\".\n")
        stop(whinge)
    }
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
