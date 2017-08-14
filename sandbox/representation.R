function (...) 
{
    value <- list(...)
    anames <- allNames(value)
    for (i in seq_along(value)) {
        ei <- el(value, i)
        if (!is.character(ei) || length(ei) != 1L) 
            stop(gettextf("element %d of the representation was not a single character string", 
                i), domain = NA)
    }
    includes <- as.character(value[!nzchar(anames)])
    if (anyDuplicated(includes)) 
        stop(gettextf("duplicate class names among superclasses: %s", 
            paste(.dQ(includes[duplicated(includes)]), collapse = ", ")), 
            domain = NA)
    slots <- anames[nzchar(anames)]
    if (anyDuplicated(slots)) 
        stop(gettextf("duplicated slot names: %s", paste(sQuote(slots[duplicated(slots)]), 
            collapse = "")), domain = NA)
    value
}
<bytecode: 0x0897d6b8>
<environment: namespace:methods>
