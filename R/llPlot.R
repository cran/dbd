llPlot <- local({

Fun <- function(par,x,distr=c("db","betabinom"),ntop,zeta,size){
#
# Note that this functon plots the log likelihood surface over which
# a *maximum* would be sought in order to find the parameter estimates,
# and not the negative log likelihood surface which is *minimised* by
# the mleDb() and mleBb() functions.
#
    switch(EXPR=distr,
        db = sum(ddb(x=x[!is.na(x)],alpha=par[1],
                     beta=par[2],ntop=ntop,zeta=zeta,log=TRUE)),
        betabinom = sum(rmutil::dbetabinom(y=x[!is.na(x)],size=size,m=par[1],s=par[2],
                        log=TRUE))
    ) 
}

function(x,distr=c("db","betabinom"),ntop,zeta,size,alim=NULL,
         blim=NULL,ngrid=c(100,100),plotType=c("persp","contour","none"),
         theta=-30,phi=40,...) {
#
# Explore the log likelihood surface.
#
    distr = match.arg(distr)

    if(distr=="betabinom") {
# Get the rmutil package.
    if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")
}
    if(is.null(alim)) {
        alim <- if(distr=="db") c(0,10) else c(0,1)
    }
    if(is.null(blim)) {
        blim <- if(distr=="db") c(0,10) else c(0,100)
    }
    if(length(ngrid)==1) ngrid <- rep(ngrid,2)
    v1   <- seq(alim[1],alim[2],length=ngrid[1])
    v2   <- seq(blim[1],blim[2],length=ngrid[2])
    dxy  <- if(distr=="db") {
                expand.grid(alpha=v1,beta=v2)
            } else {
                expand.grid(m=v1,s=v2)
            }
    fxy  <- apply(as.matrix(dxy),1,Fun,x=x,distr=distr,ntop=ntop,
                  zeta=zeta,size=size)
    z    <- matrix(fxy,nrow=ngrid[1])
    rslt <- list(x=v1,y=v2,z=z,dxy=dxy,fxy=fxy)
    pT   <- match.arg(plotType)
    switch(EXPR=pT,
        persp = {
            persp(x=v1,y=v2,z,theta=theta,phi=phi,...)
            return(invisible(rslt))
        },
        contour = {
            contour(x=v1,y=v2,z,...)
            return(invisible(rslt))
        },
        none = return(rslt)
    )
}
})
