QuarterName <- function (d) 
{
    m <- rep(1:4, each = 3)[as.integer(format(d, "%m"))]
    paste(format(d, "%Y"), "q", m, sep = "")
}
