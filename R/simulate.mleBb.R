simulate.mleBb <- function(object,nsim=1,seed=NULL,...,ndata=NULL,drop=TRUE) {
# Get the rmutil package.
if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")

m <- object["m"]
s <- object["s"]
size  <- attr(object,"size")
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
    tres <- rmutil::rbetabinom(n=ndata[i],size=size,m=m,s=s)
    attr(tres,"seed") <- seed[i]
    rslt[[i]] <- tres
}
if(nsim==1 & drop) rslt <- rslt[[1]]
rslt
}
