paste.table <- function () 
{
    f <- file(description = "clipboard", open = "r")
    df <- read.table(f, sep = "\t", header = TRUE)
    close(f)
    return(df)
}
