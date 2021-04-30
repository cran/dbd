makeDbdpars <- function(alpha,beta,ntop,zeta,ndata) {
rslt <- list(alpha=alpha,beta=beta,ntop=ntop,zeta=zeta,ndata=ndata)
class(rslt) <- "Dbdpars"
rslt
}
