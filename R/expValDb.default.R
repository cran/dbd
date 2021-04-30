expValDb.default <- function(ao,beta,ntop,zeta=FALSE,...) {
    nbot <- 0+!zeta
    x <- nbot:ntop
# Note that "ao" is really "alpha"!
    sum(x*ddb(x,ao,beta,ntop,zeta))
}
