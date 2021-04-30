gof<- function(object,obsd,...,test=TRUE,MC=FALSE,nsim=99,maxit=1000) {
#
# Goodness of fit test for the db distribution.
#
if(!inherits(object,"mleDb"))
    stop("Argument \"object\" must be of class \"mleDb\".\n")
ntop <- attr(object,"ntop")
zeta <- attr(object,"zeta")
nbot <- 0+!zeta
xi   <- nbot:ntop
ndat <- sum(!is.na(obsd))
E    <- ddb(xi,object[1],object[2],ntop=ntop,zeta=zeta)*ndat
O    <- table(factor(obsd,levels=xi))
stat <- sum((O-E)^2/E)
if(!test) return(stat)
if(MC) {
    simdat <- simulate(object,nsim=nsim)
    fitz   <- lapply(simdat,function(sd,ntop,zeta,maxit){mleDb(sd,
                            ntop=ntop,zeta=zeta,maxit=maxit)},
                            ntop=ntop,zeta=zeta,maxit=maxit)
    cmpr   <- lapply(1:nsim,function(k,fitz,obsd){gof(fitz[[k]],
                            obsd[[k]],test=FALSE)},fitz=fitz,obsd=simdat)
    pval   <- (1+sum(cmpr >= stat))/(1+nsim)
    return(list(stat=stat,pval=pval))
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
