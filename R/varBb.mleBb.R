varBb.mleBb <- function(mo,...) {
    m  <- mo["m"]
    s  <- mo["s"]
    size   <- attr(mo,"size")
    size*m*(1-m)*(s+size)/(s+1)
}
