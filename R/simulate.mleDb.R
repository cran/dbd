simulate.mleDb <- function(object,nsim=1,seed=NULL,...,ndata=NULL,drop=TRUE) {
alpha <- object["alpha"]
beta  <- object["beta"]
ntop  <- attr(object,"ntop")
zeta  <- attr(object,"zeta")
if(is.null(ndata)) {
    ndata <- attr(object,"ndata")
}
ndata <- rep(ndata,length=nsim)
if(is.null(seed)) {
    seed <- sample(1:1e5,nsim)
} else {
    if(length(seed) < nsim) {
        set.seed(seed[1])
        seed <- sample(1:1e5,nsim)
    }
}
rslt <- vector("list",nsim)
for(i in 1:nsim) {
    set.seed(seed[i])
    tres <- rdb(n=ndata[i],alpha,beta,ntop,zeta)
    attr(tres,"seed") <- seed[i]
    rslt[[i]] <- tres
}
if(nsim==1 & drop) rslt <- rslt[[1]]
rslt
}
