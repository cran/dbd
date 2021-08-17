makeBbdpars <- function(m,s,size,ndata) {
    rslt <- list(m=m,s=s,size=size,ndata=ndata)
    class(rslt) <- "Bbdpars"
    rslt
}
