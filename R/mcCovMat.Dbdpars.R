mcCovMat.Dbdpars <- function(object,nsim=100,seed=NULL,maxit=1000) {
    if(is.null(seed)) seed <- sample(1:1e5,1)
    set.seed(seed)
    par0   <- unlist(object[1:2])
    simdat <- simulate(object,nsim=nsim)
    fitz   <- lapply(simdat,mleDb,ntop=object[["ntop"]],
                     zeta=object[["zeta"]],par0=par0,maxit=maxit,
                     covmat=FALSE)
    mcCovMatEngine(fitz,par0,seed)
}
