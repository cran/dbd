varDb.mleDb <- function(ao,...) {
    alpha  <- ao["alpha"]
    beta   <- ao["beta"]
    ntop   <- attr(ao,"ntop")
    zeta   <- attr(ao,"zeta")
    nbot <- 0+!zeta
    x    <- nbot:ntop
    mu   <- expValDb(alpha,beta,ntop,zeta)
    sum((x-mu)^2 * ddb(x,alpha,beta,ntop,zeta))
}
