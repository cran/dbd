expValDb.mleDb <- function(ao,...) {
   alpha  <- ao["alpha"]
   beta   <- ao["beta"]
   ntop   <- attr(ao,"ntop")
   zeta   <- attr(ao,"zeta")
   expValDb.default(alpha,beta,ntop,zeta) 
}
