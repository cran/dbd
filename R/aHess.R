aHess <- function(object) {
    parz  <- as.vector(object)
    if(!inherits(object,"mleDb")) {
        whinge <- paste0("Argument \"object\" must be of class \"mleDb\".\n")
        stop(whinge)
    }
    hpar <- c(alpha=parz[1],beta=parz[2],ntop= attr(object,"ntop"),
              zeta = attr(object,"zeta"), ndata = attr(object,"ndata"))
    rslt  <- hess(hpar)
    rslt
}
