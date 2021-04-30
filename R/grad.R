grad <- function(x,gpar) {
#
# Set up auxiliary functions.
    h <- function(x,n,zeta){
        y <- (x+zeta)/(n+1+zeta)
        1/(y*(1-y))
    }

    T1 <- function(x,n,zeta) {
        log((x+zeta)/(n+1+zeta))
    }

    T2 <- function(x,n,zeta) {
        log(1-(x+zeta)/(n+1+zeta))
    }

# Preparation.
    if(length(gpar) != 4) {
        whinge <- paste0("Argument \"gpar\" is of length ",length(gpar), "not 4.\n")
        stop(whinge)
    }
    if(is.null(names(gpar))) { # Assume that the order is "alpha","beta","ntop","zeta".            names(gpar) <- c("alpha","beta","ntop","zeta")
    } else {
        if(!all(names(gpar) %in% c("alpha","beta","ntop","zeta"))) {
            ngp <- paste(names(gpar),collapse=", ")
            whinge <- paste0("The names of \"gpar\", i.e. ",ngp,
                             ", are incorrect.\n") 
            stop(whinge)
        }
    }

# The substance.
    x     <- x[!is.na(x)]
    alpha <- gpar["alpha"]
    beta  <- gpar["beta"]
    ntop  <- gpar["ntop"]
    zeta  <- gpar["zeta"]
    nbot  <- 0+!zeta
    iv    <- nbot:ntop
    av    <- h(iv,ntop,zeta)
    bv    <- T1(iv,ntop,zeta)
    cv    <- T2(iv,ntop,zeta)
    E     <- sum(av*exp(alpha*bv + beta*cv))
    dEdAlpha <- sum(av*bv*exp(alpha*bv + beta*cv))
    dEdBeta  <- sum(av*cv*exp(alpha*bv + beta*cv))
    dAdAlpha <- dEdAlpha/E
    dAdBeta  <- dEdBeta/E
    c(sum(T1(x,ntop,zeta)-dAdAlpha),sum(T2(x,ntop,zeta)-dAdBeta))
}
