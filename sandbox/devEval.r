devEval <- function (type = getOption("device"), expr, envir = parent.frame(), 
    name = "Rplot", tags = NULL, ..., ext = substitute(type), 
    filename = sprintf("%s.%s", paste(c(name, tags), collapse = ","), 
        ext), path = getOption("devEval/args/path", "figures/"), 
    field = NULL, onIncomplete = c("remove", "keep"), force = getOption("devEval/args/force", 
        TRUE)) 
{
    pathname <- Arguments$getWritablePathname(filename, path = path)
    fullname <- paste(c(name, tags), collapse = ",")
    fullname <- unlist(strsplit(fullname, split = ",", fixed = TRUE))
    fullname <- trim(fullname)
    fullname <- fullname[nchar(fullname) > 0]
    fullname <- paste(fullname, collapse = ",")
    if (!is.null(field)) {
        field <- Arguments$getCharacter(field)
    }
    onIncomplete <- match.arg(onIncomplete)
    force <- Arguments$getLogical(force)
    res <- list(type = type, name = name, tags = tags, fullname = fullname, 
        filename = filename, path = path, pathname = pathname)
    if (force || !isFile(pathname)) {
        done <- FALSE
        devNew(type, pathname, ...)
        on.exit({
            devDone()
            if (isPackageLoaded("R.archive")) {
                getArchiveOption <- archiveFile <- NULL
                if (getArchiveOption("devEval", FALSE)) archiveFile(pathname)
            }
            if (!done) {
                if (onIncomplete == "remove") {
                  if (isFile(pathname)) {
                    file.remove(pathname)
                  }
                }
            }
        }, add = TRUE)
        eval(expr, envir = envir)
        done <- TRUE
    }
    if (!is.null(field)) {
        res <- res[[field]]
    }
    res
}
