vcov.mleDb <- function(object,...){
    v <- attr(object,"covMat")
    if(is.null(v)) v <- NA
    v
}
