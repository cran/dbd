mcCovMat.mleDb <- function(object,nsim=100,seed=NULL,maxit=1000) {
    if(is.null(seed)) seed <- sample(1:1e5,1)
    set.seed(seed)
    par0   <- as.vector(object)
    distr  <- "db"
    simdat <- simulate(object,nsim=nsim)
    fitz   <- lapply(simdat,mleDb,ntop=attr(object,"ntop"),
                     zeta=attr(object,"zeta"),par0=par0,maxit=maxit,
                     covmat=FALSE)
    mcCovMatEngine(fitz,par0,seed)
}
