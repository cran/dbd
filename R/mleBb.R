mleBb <- local({

objFun <- function(pars,x,size) {
    -sum(rmutil::dbetabinom(y=x,size=size,m=pars[1],s=pars[2],log=TRUE))
}

gfun <- function(par,x,size){
                gpars <- c(par,size=size)

# Negative sign in the next line because optim() needs the gradient
# of the function that is is *minimising* which is the *negative*
# log likelihood.
                -grad(x=x[!is.na(x)],distr="betabinom",gpars)
}

function (x,size,par0=NULL,maxit=1000,covmat=TRUE,useGinv=FALSE) {
# Maximum likelihood estimation of the parameters m
# and s of the beta binomial distribution.
#
# Get the rmutil package.
if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")

# Get rid of missing values, if any.
x <- x[!is.na(x)]

# Get the moment estimates as starting values.
if(is.null(par0)) {
    par0 <- meBb(x,size)
} else {
    if(length(par0) != 2) {
        whinge <- paste0("Argument \"par0\" is of length ",length(par0)," not 2.\n")
        stop(whinge)
    }
    if(is.null(names(par0))) names(par0) <- c("m","s")
    # Hammer and hope!
}
eps  <- sqrt(.Machine$double.eps)
if(!is.finite(par0[1])) par0[1] <- mean(x)/size
if(!is.finite(par0[2])) par0[2] <- 100
if(par0[1] <= 0) par0[1] <- eps
if(par0[1] >= 1) par0[1] <- 1 - eps
if(par0[2] <= 0) par0[2] <- eps

# Maximise the likelihood.
temp <- optim(par0,fn=objFun,gr=gfun,method="L-BFGS-B",lower=c(eps,eps),
              upper=c(1-eps,Inf),control=list(maxit=maxit),
              x=x,size=size)
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
names(rslt) <- c("m","s")
attr(rslt,"size") <- size
attr(rslt,"log.like") <- -temp$value
if(covmat) {
    H <- hess(x,distr="betabinom",hpar=c(rslt,size=size))
    CM <- try(solve(H),silent=TRUE)
    if(inherits(CM,"try-error")) {
        if(useGinv) {
            requireNamespace("MASS")
            CM <- MASS::ginv(H)
            attr(rslt,"covMat") <- CM
            whinge <- paste0("Hessian appears to be singular. Setting\n",
                             "  the \"covMat\" attribute to be the\n",
                             "  generalised inverse of the hessian.\n",
                             "  Caution is advised.\n")
            warning(whinge)
        } else {
            attr(rslt,"covMat") <- NA
            whinge <- paste0("Hessian appears to be singular. Setting\n",
                         "  the \"covMat\" attribute to be NA.\n")
            warning(whinge)
        }
    } else {
        attr(rslt,"covMat") <- CM
    }
}
attr(rslt,"ndata") <- length(x)
class(rslt) <- "mleBb"
rslt
}
})
