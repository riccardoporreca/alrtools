copy.table <- function (obj, row.names = FALSE, buffer.size = 128) 
{
    f <- file(description = paste("clipboard-", buffer.size, 
        sep = ""), open = "w")
    write.table(obj, f, row.names = row.names, sep = "\t")
    close(f)
}
