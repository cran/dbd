rdb <- function(n,alpha,beta,ntop,zeta=FALSE) {
   nbot <- 0+!zeta
   x    <- runif(n)
   p    <- c(0,pdb(nbot:ntop,alpha,beta,ntop,zeta))
   m    <- outer(x,p,">")
   r    <- apply(m,1,function(x){max((1:length(x))[x])}) - zeta
   r
}
