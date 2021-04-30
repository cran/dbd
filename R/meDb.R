meDb <- function (x,ntop) {
# Moment estimation (rough!) for the db distribution.
    xbar <- mean(x,na.rm=TRUE)
    s2   <- max(var(x,na.rm=TRUE),.Machine$double.eps^(0.25))
    n    <- ntop
    a    <- (n+1-xbar)/(1+xbar)
    alpha <- a*(n+2)^2/(s2*(a+1)^3) - 1/(a+1)
    beta  <- a*alpha
    c(alpha=alpha,beta=beta)
}
