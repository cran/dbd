plot.mleBb <- function(x,...,plot=TRUE,col.fit="red",col.obsd="blue",
                       tikx=NULL,xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,
                       obsd=NULL,incr=NULL,main="",legPos="topright") {

# Get the rmutil package.
if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")

size <- attr(x,"size")

# Set or check on the value of incr.
if(is.null(incr)) {
    incr <- if(size < 20) 0.1 else 0.5
} else {
    if(incr==0 | abs(incr) >= 1)
       stop("Inappropriate value for \"incr\".\n")
}

if(!is.null(obsd)) {
   xi   <- 0:size
   po   <- table(factor(obsd,levels=xi))
   po   <- po/sum(po)
   uyli <- max(po) # upper y limit increment
} else {
   uyli <- NULL
}
rslt <- plotBb(m=x[1],s=x[2],size=size,plot=plot,tikx=tikx,
               xlim=xlim,ylim=ylim,main=main,col=col.fit,
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
