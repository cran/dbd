pdb <- function(x,alpha,beta,ntop,zeta=FALSE) {
   nbot <- 0+!zeta
   m <- outer(x,nbot:ntop,">=")
   i <- apply(m,1,sum)
   p <- ddb(nbot:ntop,alpha,beta,ntop,zeta)
   cp <- c(0,cumsum(p))
   cp[i+1]
}
