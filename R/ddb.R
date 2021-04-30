ddb <- function(x,alpha,beta,ntop,zeta=FALSE,log=FALSE) {
   nbot <- 0+!zeta
   xr   <- round(x)
   eps  <- sqrt(.Machine$double.eps)
   ok   <- ifelse(is.na(x),TRUE,abs(x-xr)<eps)
   if(any(!ok)) {
       warning("There are non-integer values in argument \"x\".\n")
   }
   aok  <- xr %in% c(nbot:ntop,NA) & ok
   xr[!aok] <- 0 # Cosmetic; to avoid spurious warnings.

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

   iv <- nbot:ntop
   av <- h(iv,ntop,zeta)
   bv <- T1(iv,ntop,zeta)
   cv <- T2(iv,ntop,zeta)
   A  <- log(sum(av*exp(alpha*bv + beta*cv)))
   
   if(log) {
       ifelse(aok,log(h(xr,ntop,zeta)) + alpha*T1(xr,ntop,zeta) +
                                        beta*T2(xr,ntop,zeta) - A,-Inf)
   } else {
       ifelse(aok,h(xr,ntop,zeta)*exp(alpha*T1(xr,ntop,zeta) +
                                      beta*T2(xr,ntop,zeta) - A),0)
   }
}
