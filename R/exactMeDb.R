exactMeDb <- local({
objFun <- function(par,xbar,s2,ntop,zeta) {
    theoMean <- expValDb(par[1],par[2],ntop,zeta)
    theoVar  <- varDb(par[1],par[2],ntop,zeta)
    (xbar-theoMean)^2 + (s2-theoVar)^2
}

function(x,ntop,zeta=FALSE,par0=NULL,maxit=1000) {
#
# Calculate the "exact" moment estimates of the alpha and beta
# parameters of db distribution using optim().
#
    xbar <- mean(x,na.rm=TRUE)
    s2   <- var(x,na.rm=TRUE)
    if(is.null(par0)) {
        par0 <- meDb(x,ntop)
    } else {
        if(length(par0) != 2) {
            whinge <- paste0("Argument \"par0\" is of length ",length(par0)," not 2.\n")
            stop(whinge)
        }
        if(is.null(names(par0))) names(par0) <- c("alpha","beta")
        # Hammer and hope!
    }
    temp <- optim(par0,fn=objFun,method="BFGS",
                  ntop=ntop,zeta=zeta,xbar=xbar,s2=s2,
                  control=list(maxit=maxit))
    conv <- temp$convergence
    if(!(conv %in% c(0,1))) {
        whinge <- paste0("Exceptional convergence problem; \"convergence\" is ",
                          conv,";\n","  message: ",temp$message,".\n")
    }
    if(conv == 1) {
        whinge <- paste0("Maximum number of iterations ",maxit," exceeded.\n")
        eow <- options()[["maxitErrorOrWarn"]]
        if(eow=="error") {
            stop(whinge)
        } else if(eow=="warn") {
            warning(whinge)
        } else {
            stop("Value of options()[[\"maxitErrorOrWarn\"]] not recognised.\n")
        }
    }
    rslt <- temp[["par"]]
    names(rslt) <- c("alpha","beta")
    attr(rslt,"ntop") <- ntop
    attr(rslt,"zeta") <- zeta
    attr(rslt,"minSqDiff") <- temp$value
    ndata <- sum(!is.na(x))
    attr(rslt,"ndata") <- ndata
    class(rslt) <- "exactMeDb"
    rslt
}
})

