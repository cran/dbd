plot.mleDb <- function(x,...,plot=TRUE,col.fit="red",col.obsd="blue",tikx=NULL,
                        xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,
                        obsd=NULL,incr=NULL,main="",legPos="topright") {

ntop <- attr(x,"ntop")
zeta <- attr(x,"zeta")

# Set or check on the value of incr.
if(is.null(incr)) {
    incr <- if(ntop < 20) 0.1 else 0.5
} else {
    if(incr==0 | abs(incr) >= 1)
       stop("Inappropriate value for \"incr\".\n")
}
if(!is.null(obsd)) {
   nbot <- 0+!zeta
   xi   <- nbot:ntop
   po   <- table(factor(obsd,levels=xi))
   po   <- po/sum(po)
   uyli <- max(po) # upper y limit increment
} else {
   uyli <- NULL
}
rslt <- plotDb(alpha=x[1],beta=x[2],ntop=ntop,zeta=zeta,plot=plot,
               tikx=tikx, xlim=xlim,ylim=ylim,main=main,col=col.fit,
               xlab=xlab,ylab=ylab,uyli=uyli)
if(!is.null(obsd)) {
    if(plot) {
        lines(incr+rslt[,"x"],po,type="h",col=col.obsd)
        if(!is.null(legPos)) {
            legend(legPos,lty=1,col=c(col.fit,col.obsd),
                   legend=c("fitted","observed"),bty="n")
        }
    }
    rslt[,"po"] <- po
}
invisible(rslt)
}
