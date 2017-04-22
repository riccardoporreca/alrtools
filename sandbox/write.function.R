write.function <- function (f, file, name = NULL) 
{
    if (is.null(name)) {
        name <- as.character(match.call(expand.dots = FALSE)$f)
    }
    kode <- deparse(f)
    kode[1] <- paste0(name, " <- ", kode[1])
    cat(kode, file = file, sep = "\n")
    invisible(kode)
}
