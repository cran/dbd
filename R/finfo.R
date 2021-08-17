finfo <- function(distr=c("db","betabinom"),alpha,beta,
                  ntop,ndata,zeta=FALSE,x,m,s,size) {
#
# If distr is "db" this function returns the (expected) Fisher information.
# This is equal to the hessian of the *negative* log likelihood, which in
# this instance does not depend on the data and hence is its own expected
# value.
# If distr is "betabinom" this function returns the hessian
# of the *negative* log likelihood, evaluated at the observations x.
# If the supplied parameters are the maximum likelihood estimates
# based upon x, thne this is the *observed* Fisher information.
#
    distr <- match.arg(distr)
    switch(EXPR=distr,
        db={
            hpar <- c(alpha=alpha,beta=beta,ntop=ntop,zeta=zeta,ndata=ndata)
            hessDb(hpar)
        },
        betabinom={
            hpar <- c(m=m,s=s,size=size)
            hessBb(x,hpar)
        }
    )
}
