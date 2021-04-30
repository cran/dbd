mcCovMat <- function(object,nsim=100,seed=NULL,maxit=1000) {
if(is.null(seed)) seed <- sample(1:1e5,1)
set.seed(seed)
vnms <- c("alpha","beta")
if(inherits(object,"mleDb")) {
    par0        <- as.vector(object)
    names(par0) <- vnms
    mu          <- par0
    simdat      <- simulate(object,nsim=nsim)
    fitz        <- lapply(simdat,mleDb,ntop=attr(object,"ntop"),
                          zeta=attr(object,"zeta"),par0=par0,maxit=maxit,
                          covmat=FALSE)
} else if(inherits(object,"Dbdpars")) {
    par0   <- unlist(object[1:2])
    mu     <- par0
    simdat <- simulate(object,nsim=nsim)
    fitz   <- lapply(simdat,mleDb,ntop=object[["ntop"]],
                          zeta=object[["zeta"]],par0=par0,maxit=maxit,
                          covmat=FALSE)
} else {
    whinge <- paste0("Argument \"object\" must be either of class \"mleDb\"",
                     " or of class \"Dbdpars\".\n")
    stop(whinge)
}

simpar <- matrix(unlist(fitz),byrow=TRUE,ncol=2)
xbar   <- apply(simpar,2,mean)
rslt   <- var(simpar)
rslt   <- (nsim-1)*rslt/nsim + (xbar-mu)%*%t(xbar-mu)
rownames(rslt) <- colnames(rslt) <- vnms
attr(rslt,"seed") <- seed
rslt
}
