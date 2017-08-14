copy.tab <- function (tabname, coltitle = "", ...) 
{
    df <- cbind(rownames(tabname), as.data.frame(tabname))
    colnames(df)[1] <- coltitle
    return(copy.table(df, ...))
}
