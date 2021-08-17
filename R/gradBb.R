gradBb <- function (x, gpar) {
#
# This function was created using the Deriv() function from the
# Deriv package.  The argument list (and the processing thereof)
# was then modified by hand.
#
# Preparation.
    if(length(gpar) != 3) {
        whinge <- paste0("Argument \"gpar\" is of length ",length(gpar), "not 3.\n")
        stop(whinge)
    }
    if(is.null(names(gpar))) { # Assume that the order is "m", "s", "size".
        names(gpar) <- c("m","s","size")
    } else {
       if(!all(names(gpar) %in% c("m","s","size"))) {
           ngp <- paste(names(gpar),collapse=", ")
           whinge <- paste0("The names of \"gpar\", i.e. ",ngp,
                            ", are incorrect.\n") 
           stop(whinge)
       }
    }

# The substance.
    m    <- gpar["m"]
    s    <- gpar["s"]
    size <- gpar["size"]
    .e1 <- 1 - m
    .e2 <- m * s
    .e3 <- s * .e1
    .e4 <- digamma(.e2 + x)
    .e5 <- digamma(.e2)
    .e6 <- digamma(.e3 + size - x)
    .e7 <- digamma(.e3)
    .e8 <- digamma(s + size)
    .e9 <- digamma(s)
    c(m = sum(s * (.e4 + .e7 - (.e5 + .e6))), s = sum(.e1 * (.e6 + 
        .e9 - (.e7 + .e8)) + m * (.e4 + .e9 - (.e5 + .e8))))
}
