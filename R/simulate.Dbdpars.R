simulate.Dbdpars <- function(object,nsim=1,seed=NULL,...,ndata=NULL,drop=TRUE) {
alpha <- object$alpha
beta  <- object$beta
ntop  <- object$ntop
zeta  <- object$zeta
if(is.null(ndata)) {
    ndata <- object$ndata
}
ndata <- rep(ndata,length=nsim)
if(is.null(seed)) {
    seed <- sample(1:1e5,1)
}
set.seed(seed)
rslt <- vector("list",nsim)
for(i in 1:nsim) {
    tres <- rdb(n=ndata[i],alpha=alpha,beta=beta,ntop=ntop,zeta=zeta)
    rslt[[i]] <- tres
}
if(nsim==1 & drop) rslt <- rslt[[1]]
attr(rslt,"seed") <- seed
rslt
}
