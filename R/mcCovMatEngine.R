mcCovMatEngine <- function(fitz,par0,seed) {
    simpar <- matrix(unlist(fitz),byrow=TRUE,ncol=2)
    nsim   <- nrow(simpar)
    xbar   <- apply(simpar,2,mean)
    rslt   <- var(simpar)
    rslt   <- (nsim-1)*rslt/nsim + (xbar-par0)%*%t(xbar-par0)
    rownames(rslt) <- colnames(rslt) <- names(par0)
    attr(rslt,"seed") <- seed
    rslt
}
