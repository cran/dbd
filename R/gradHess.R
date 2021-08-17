grad <- function(x,distr=c("db","betabinom"),gpar) {
   distr <- match.arg(distr)
   switch(EXPR=distr,
       db = gradDb(x,gpar),
       betabinom = gradBb(x,gpar)
   )
}

hess <- function(x,distr=c("db","betabinom"),hpar) {
   distr <- match.arg(distr)
   switch(EXPR=distr,
       db = hessDb(hpar),
       betabinom = hessBb(x,hpar)
   )
}
