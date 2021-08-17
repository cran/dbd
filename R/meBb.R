meBb <- function (x,size,warn=FALSE) {
# Moment estimation of the parameters m and s of the beta binomial
# distribution.

#
# Get the rmutil package.
if(!requireNamespace("rmutil"))
    stop("The package \"rmutil\" seems not to be available.\n")

# Get rid of missing values, if any.
x <- x[!is.na(x)]

# Calculate the moment estimates.
m1 <- mean(x)
m2 <- mean(x^2)
denom    <- size*(m2/m1 - m1 - 1) + m1
numAlpha <- size*m1 - m2
numBeta  <- (size - m1)*(size - m2/m1)
alphaHat <- numAlpha/denom
betaHat  <- numBeta/denom
sHat     <- alphaHat + betaHat
mHat     <- alphaHat/sHat

# Check that the estimates are legitimate, i.e. that sHat is greater
# than zero and mHat is between 0 and 1.
if(warn) {
    if(sHat <= 0 | mHat < 0 | mHat > 1) {
        whinge <- paste0("The moment estimates of the parameters are\n",
                         "  sHat = ",sHat," and mHat = ",mHat,".  The value of\n",
                         "  \"sHat\" should be strictly positive and\n",
                         "  the value of \"mHat\" should be between 0 and 1.\n")
        warning(whinge)
    }
}
c(m=mHat,s=sHat)
}
