set.eow <- function(eow=c("error","warn")) {
    eow <- match.arg(eow)
    options(maxitErrorOrWarn=eow)
    invisible()
}

get.eow <- function() {
    options()[["maxitErrorOrWarn"]]
}
