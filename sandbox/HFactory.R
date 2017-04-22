HFactory <- function (name, ignore.case = TRUE) 
{
    is.function <- base::is.function(name)
    name <- as.character(match.call(expand.dots = FALSE)$name)
    if (is.function) 
        name <- paste(name, "()", sep = "")
    if (ignore.case) 
        ic = "TRUE"
    else ic = "FALSE"
    s <- paste("\n             function(pattern = ''){\n             names(", 
        name, ")[grep(pattern, names(", name, "), ignore.case = ", 
        ic, ")]\n}", sep = "")
    return(eval(parse(text = s)))
}
