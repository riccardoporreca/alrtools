fitdistrplus__datacens <- function (x, lbound, ubound = NULL) 
{
    supra <- max(x) + 1
    if (is.null(ubound)) 
        ubound <- supra
    if (ubound == Inf) 
        ubound <- supra
    left <- ifelse(x >= lbound, pmin(ubound, x), NA_real_)
    right <- ifelse(x < ubound, pmax(lbound, x), NA_real_)
    data.frame(left = left, right = right)
}
