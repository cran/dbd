varDb.default <- function(ao,beta,ntop,zeta=FALSE,...) {
    nbot <- 0+!zeta
    x    <- nbot:ntop
# Note that "ao" is really "alpha"!
    mu   <- expValDb(ao,beta,ntop,zeta)
    sum((x-mu)^2 * ddb(x,ao,beta,ntop,zeta))
}
