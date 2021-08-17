mleDb <- local({
    objFun <- function(par,x,ntop,zeta){
                -sum(log(ddb(x=x[!is.na(x)],alpha=par[1],
                              beta=par[2],ntop=ntop,zeta=zeta)))
              }

    gfun <- function(par,x,ntop,zeta){
                gpar <- c(par,ntop=ntop,zeta=zeta)

# Negative sign in the next line because optim() needs the gradient
# of the function that is is *minimising* which is the *negative*
# log likelihood.
                -grad(x=x[!is.na(x)],distr="db",gpar)
            }

    hfun <- function(par,ndata,ntop,zeta){
                hpar <- c(alpha=par[1],beta=par[2],
                           ntop=ntop,zeta=zeta,ndata=ndata)
                hess(x=NULL,distr="db",hpar)
            }

function (x,ntop,zeta=FALSE,par0=NULL,UB=10,maxit=1000,
          covmat=TRUE,useGinv=FALSE) {
# Maximum likelihood estimation of the shape parameters alpha
# and beta of the db distribution.
    if(is.null(par0)) {
        par0 <- pmin(meDb(x,ntop),UB)
    } else {
        if(length(par0) != 2) {
            whinge <- paste0("Argument \"par0\" is of length ",length(par0)," not 2.\n")
            stop(whinge)
        }
        if(is.null(names(par0))) names(par0) <- c("alpha","beta")
        # Hammer and hope!
    }
    temp <- optim(par0,fn=objFun,gr=gfun,method="BFGS",
                  ntop=ntop,zeta=zeta,x=x,control=list(maxit=maxit))
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
    attr(rslt,"log.like") <- -temp$value
    ndata <- sum(!is.na(x))
    attr(rslt,"ndata") <- ndata
    if(covmat) {
        H  <- hess(x=NULL,distr="db",
                     hpar=c(rslt, ntop=ntop,zeta=zeta,ndata=ndata))
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
    class(rslt) <- "mleDb"
    rslt
}
})
