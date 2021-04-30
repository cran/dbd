qdb <- function(p,alpha,beta,ntop,zeta=FALSE) {
   nbot <- 0+!zeta
   qp <- pdb(nbot:ntop,alpha,beta,ntop,zeta)
   m  <- outer(qp,p,"<")
   1 - zeta + apply(m,2,sum)
}
