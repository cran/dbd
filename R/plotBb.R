plotBb <- local({

rbox <- function() {
    if(is.null(dev.list()))
        stop("No device open.\n")
    uuu <- par()$usr
    lines(c(uuu[1],uuu[1]),c(0,uuu[4]),lwd=2)
    lines(c(uuu[1],uuu[2]),c(uuu[4],uuu[4]),lwd=2)
    lines(c(uuu[2],uuu[2]),c(0,uuu[4]),lwd=2)
    lines(c(uuu[1],uuu[2]),c(0,0),lwd=1)
    invisible()
}

function(m,s,size,...,plot=TRUE,tikx=NULL,xlim=NULL,ylim=NULL,
         xlab=NULL,ylab=NULL,main="") {
# Get the rmutil package.
if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")

xi   <- 0:size
p    <- rmutil::dbetabinom(y=xi,size=size,m=m,s=s)
if(plot) {
    if(is.null(tikx)) {
        tikx <- pretty(0:size)
        if(tikx[length(tikx)] > size)  tikx <- tikx[-length(tikx)]
    }
    if(is.null(xlim)) xlim <- c(0,size)
    if(is.null(ylim)) ylim <- c(0,max(p,list(...)[["uyli"]]))
    if(is.null(xlab)) xlab <- "x"
    if(is.null(ylab)) ylab <- "probability"
    dotz <- list(...)
    dotz[["uyli"]] <- NULL
    arghs <- list(x=xi,y=p,type="h",xlim=xlim,ylim=ylim,
                  main=main,xlab=xlab,ylab=ylab,axes=FALSE)
    arghs <- c(arghs,dotz)
    do.call(plot.default,arghs)
    axis(side=2,lwd=0,lwd.ticks=1)
    axis(side=1,lwd=0,pos=0,at=tikx)
    rbox()
}
rslt <- data.frame(x=xi,p=p)
invisible(rslt)
}
})
