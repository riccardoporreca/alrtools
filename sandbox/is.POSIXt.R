is.POSIXt <- function (x) 
{
    if ((class(x)[1] == "POSIXlt") || (class(x)[1] == "POSIXct")) 
        TRUE
    else FALSE
}
