llPlot <- local({

Fun <- function(par,x,ntop,zeta){
#
# Note that this function plots the log likelihood surface over which
# a *maximum* would be sought in order to find the parameter estimates,
# and not the negative log likelihood surface which is *minimised* by
# the mleDb() and mleBb() functions.
#
    x <- x[!is.na(x)]
    sum(ddb(x=x,alpha=par[1],beta=par[2],ntop=ntop,zeta=zeta,log=TRUE))
}

function(x,ntop,zeta,alim=NULL,blim=NULL,ngrid=c(100,100),
         plotType=c("persp","contour","none"),theta=-30,phi=40,...) {
#
# Explore the log likelihood surface.
#
    eps     <- sqrt(.Machine$double.eps)

    if(is.null(alim)) {
        alim <- c(0,10)
    }
    if(is.null(blim)) {
        blim <- c(0,10)
    }
    if(length(ngrid)==1) ngrid <- rep(ngrid,2)
    v1   <- seq(alim[1],alim[2],length=ngrid[1])
    v2   <- seq(blim[1],blim[2],length=ngrid[2])
    dxy  <- expand.grid(x=v1,y=v2)
    fxy  <- apply(as.matrix(dxy),1,Fun,x=x,ntop=ntop,zeta=zeta)
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
