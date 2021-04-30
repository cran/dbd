plot.mleDb <- function(x,...,col.fit="red",col.obsd="blue",
                        xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,
                        obsd=NULL,main="",legPos="topright") {
ntop <- attr(x,"ntop")
zeta <- attr(x,"zeta")
nbot <- 0+!zeta
xi   <- nbot:ntop
p    <- ddb(xi,x[1],x[2],ntop=ntop,zeta=zeta)
if(!is.null(obsd)) {
   po <- table(factor(obsd,levels=xi))
   po <- po/sum(po)
   incr <- 0.5
} else {
   po <- NULL
   incr <- 0
}
if(is.null(xlim)) xlim <- c(nbot,ntop)
if(is.null(ylim)) ylim <- c(0,max(p,po))
plotDb(alpha=x[1],beta=x[2],ntop=ntop,zeta=zeta,
        xlim=xlim,ylim=ylim,main=main,col=col.fit,
        xlab=xlab,ylab=ylab)
if(!is.null(obsd)) {
    lines(0.1+xi,po,type="h",col=col.obsd)
    if(!is.null(legPos)) {
        legend(legPos,lty=1,col=c(col.fit,col.obsd),
               legend=c("fitted","observed"),bty="n")
    }
}
}
