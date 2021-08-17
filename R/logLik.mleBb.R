logLik.mleBb <- function(object,...) {
    rslt <- attr(object,"log.like")
    class(rslt) <- "logLik"
    attr(rslt,"df") <- 2
}
