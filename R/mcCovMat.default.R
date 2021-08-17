mcCovMat.default <- function(object,nsim=100,seed=NULL,maxit=1000) {
    whinge <- paste("Argument \"object\" must be of class either \"mleDb\",
                    \"mleBb\", \"Dbdpars\" or \"Bbdpars\".\n")
    stop(whinge)
}
