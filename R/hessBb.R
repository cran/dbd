hessBb <- function (x,hpar) {
#
# This function was created by applying the Deriv() function from
# the Deriv package (with argument nderiv set equal to 2) to the
# *negative* log likelihood.  The expression returned is thus
# the hessian of the negative log likelihood and is (or should
# be!!!) positive definite. Its inverse is an estimate of the
# covariance matrix of the parameter estimates.  Note that the
# output Deriv() was modified by hand.  The argument list (and
# the initial processing thereof) was adjusted and the returned
# value was restructured as a 2 x 2 matrix with row and column
# names c("m","s").
#
     size <- hpar["size"]
     m    <- hpar["m"]
     s    <- hpar["s"]
    .e1 <- 1 - m
    .e2 <- m * s
    .e3 <- s * .e1
    .e4 <- .e2 + x
    .e6 <- .e3 + size - x
    .e7 <- trigamma(.e4)
    .e8 <- trigamma(.e2)
    .e9 <- trigamma(.e6)
    .e10 <- trigamma(.e3)
    .e11 <- .e1 * (.e9 - .e10)
    .e12 <- m * (.e7 - .e8)
    .e13 <- -sum(digamma(.e4) + digamma(.e3) + s * (.e12 - .e11) - 
        (digamma(.e2) + digamma(.e6)))
    .e15 <- trigamma(s + size)
    .e16 <- trigamma(s)
    H <- matrix(c(m = c(m = -sum(s^2 * (.e7 + .e9 - (.e8 + .e10))), s = .e13), 
                  s = c(m = .e13, s = -sum((.e11 + .e16 - .e15) * .e1 + 
                  m * (.e12 + .e16 - .e15)))),nrow=2)
    dimnames(H) <- list(c("m","s"),c("m","s"))
    H
}
