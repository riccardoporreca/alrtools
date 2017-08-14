copy.table <- function (obj, size = 2^14) 
{
    clip <- paste("clipboard-", size, sep = "")
    f <- file(description = clip, open = "w")
    write.table(obj, f, row.names = FALSE, sep = "\t")
    close(f)
}
