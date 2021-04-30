hess <- function(hpar) {
#
# Set up auxiliary functions.
#
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
    if(length(hpar) != 5) {
        whinge <- paste0("Argument \"hpar\" is of length ",length(hpar), "not 5.\n")
        stop(whinge)
    }
    if(is.null(names(hpar))) { # Assume that the order is
                               # "alpha","beta","ntop","zeta","ndata"
            names(hpar) <- c("alpha","beta","ntop","zeta","ndata")
    } else {
        if(!all(names(hpar) %in% c("alpha","beta","ntop","zeta","ndata"))) {
            nhp <- paste(names(hpar),collapse=", ")
            whinge <- paste0("The names of \"hpar\", i.e. ",nhp,
                             ", are incorrect.\n")
            stop(whinge)
        }
    }


#
# The substance.
#
   alpha <- hpar["alpha"]
   beta  <- hpar["beta"]
   ntop  <- hpar["ntop"]
   zeta  <- hpar["zeta"]
   ndata <- hpar["ndata"]
   nbot <- 0+!zeta
   iv <- nbot:ntop
   av <- h(iv,ntop,zeta)
   bv <- T1(iv,ntop,zeta)
   cv <- T2(iv,ntop,zeta)
   E  <- sum(av*exp(alpha*bv + beta*cv))

   dEdAlpha <- sum(av*bv*exp(alpha*bv + beta*cv))
   dEdBeta  <- sum(av*cv*exp(alpha*bv + beta*cv))

   d2EdAlpha2 <- sum(av*bv^2*exp(alpha*bv + beta*cv))
   d2EdBeta2  <- sum(av*cv^2*exp(alpha*bv + beta*cv))
   d2EdAlphaDbeta <- sum(av*bv*cv*exp(alpha*bv + beta*cv)) 

   d2AdAlpha2 <- (d2EdAlpha2 - dEdAlpha^2/E)/E
   d2AdBeta2  <- (d2EdBeta2 - dEdBeta^2/E)/E
   d2AdAlphaDbeta <- (d2EdAlphaDbeta - dEdAlpha*dEdBeta/E)/E
#
# Note that d2AdAlpha2 is the *negative* of d2lldAlpha2, etc.,
# where "ll" means the log likelihood.  Consequently the expression
# for H gives the hessian of the negative log likelihood, and is
# (or should be!!!) positive definite.  Its inverse is an estimate
# of the covariance matrix of the parameter estimates.
#
   H <- ndata*matrix(c(d2AdAlpha2,d2AdAlphaDbeta,d2AdAlphaDbeta,d2AdBeta2),nrow=2)
   dimnames(H) <- list(c("alpha","beta"),c("alpha","beta"))
   H
}
