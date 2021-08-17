gof.mleBb <- function(object,obsd,...,test=TRUE,MC=FALSE,seed=NULL,
                      nsim=99,maxit=1000,verb=FALSE) {
#
# Goodness of fit test for the beta-binomial distribution.
#
# Get the rmutil package.
if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")

size <- attr(object,"size")
xi   <- 0:size
ndat <- sum(!is.na(obsd))
E    <- rmutil::dbetabinom(y=xi,size,object[1],object[2])*ndat
O    <- table(factor(obsd,levels=xi))
stat <- sum((O-E)^2/E)
if(!test) return(stat)
if(MC) {
    if(is.null(seed)) seed <- sample(1:1e5,1)
    set.seed(seed)
    if(verb) {
        cmpr <- numeric(nsim)
        for(i in 1:nsim) {
            simdat <- simulate(object,nsim=1)
            fitz   <- mleBb(simdat,size=size,maxit=maxit)
            cmpr[i] <- gof.mleBb(fitz,simdat,test=FALSE)
            cat(i,"")
            if(i%%10 == 0) cat("\n")
       }
       if(nsim%%10 != 0) cat("\n")
    } else {
        simdat <- simulate(object,nsim=nsim)
        fitz   <- lapply(simdat,function(sd,size,maxit){mleBb(sd,
                                size=size,maxit=maxit)},
                                size=size,maxit=maxit)
        cmpr   <- sapply(1:nsim,function(k,fitz,obsd){gof.mleBb(fitz[[k]],
                            obsd[[k]],test=FALSE)},fitz=fitz,obsd=simdat)
    }
    pval   <- (1+sum(cmpr >= stat))/(1+nsim)
    rslt   <- list(stat=stat,pval=pval)
    attr(rslt,"seed") <- seed
    return(rslt)
}
esmall <- mean(E) < 5 | any(E < 1)
if(esmall) {
    whinge <- paste0("Expected values are small; chi squared test is invalid.\n",
                     "These expected values are available as the attribute",
                     " \"expVals\" of the\n","returned value.\n")
    warning(whinge)
}
dfr  <- length(E) - 3
pval <- pchisq(stat,dfr,lower.tail=FALSE)
rslt <- list(stat=stat,pval=pval,degFree=dfr)
if(esmall) attr(rslt,"expVals") <- E
rslt
}
