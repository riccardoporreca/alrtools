write.package <- function (pkg, folder) 
{
    if (pkg == "translations") 
        return(invisible(NULL))
    dir.create(folder, showWarnings = FALSE)
    fs <- ls(getNamespace(pkg), all = TRUE)
    for (f in fs) {
        g <- get(f, envir = getNamespace(pkg))
        f <- gsub(pattern = "[&]", replacement = "&amp;", f)
        f <- gsub(pattern = "[<]", replacement = "&lt;", f)
        f <- gsub(pattern = "[>]", replacement = "&gt", f)
        f <- gsub(pattern = "[/]", replacement = "&rs;", f)
        f <- gsub(pattern = "[\\]", replacement = "&ls;", f)
        f <- gsub(pattern = "[*]", replacement = "&star;", f)
        f <- gsub(pattern = "[?]", replacement = "&ques;", f)
        f <- gsub(pattern = "[\"]", replacement = "&quote;", 
            f)
        f <- gsub(pattern = "[|]", replacement = "&bar;", f)
        f <- gsub(pattern = "[:]", replacement = "&colon;", f)
        if (tolower(f) == "prn") 
            f <- "prn&blank;"
        p <- paste0(folder, "/", f, ".R")
        write.function(g, p, f)
    }
}
