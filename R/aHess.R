aHess <- function(object,x) {
    parz  <- as.vector(object)
    if(inherits(object,"mleDb")) {
        distr <- "db"
        hpar <- c(alpha=parz[1],beta=parz[2],ntop= attr(object,"ntop"),
                   zeta = attr(object,"zeta"), ndata = attr(object,"ndata"))
    } else if(inherits(object,"mleBb")) {
        if(missing(x)) {
            whinge <- paste0("Argument \"x\" must be supplied when \"object\"",
                             " is of class \"mleBb\".\n")
            stop(whinge)
        }
        distr <- "betabinom"
        hpar <- c(m=parz[1],s=parz[2],size = attr(object,"size"),
                   ndata = attr(object,"ndata"))
    } else {
        whinge <- paste0("Argument \"object\" must be either of class \"mleDb\"\n",
                         "  or of class \"mleBb\".\n")
        stop(whinge)
    }
    rslt  <- hess(x,distr,hpar)
    rslt
}
