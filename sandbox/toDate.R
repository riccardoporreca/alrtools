toDate <- function (colnum, df, format = "%d/%m/%Y") 
{
    if (is.Date(df[, colnum]) == TRUE) 
        return(df[, colnum])
    if (is.POSIXt(df[, colnum]) == TRUE) {
        return(as.Date(df[, colnum] + 3600))
    }
    if (is.numeric(df[, colnum]) == TRUE) {
        return(as.Date(df[, colnum], origin = "1899-12-30"))
    }
    return(as.Date(df[, colnum], format))
}
