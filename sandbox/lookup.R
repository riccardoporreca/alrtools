lookup <- function (data, ltable, by, drop = TRUE) 
{
    if (nrow(data) > 0) 
        data$ID_ <- 1:nrow(data)
    else data$ID_ <- integer(0)
    ln <- names(ltable)
    dn <- names(data)
    if (!missing(by)) {
        by_start <- paste0(by, "_start")
        ln <- ln[ln %in% c(by, by_start)]
        dn <- dn[dn %in% by]
    }
    lranges <- ln[grepl(pattern = "_start$", x = ln)]
    if (length(lranges) > 0) {
        dranges <- strtrim(lranges, nchar(lranges) - 6)
        for (i in 1:length(lranges)) {
            l <- lranges[i]
            d <- dranges[i]
            lv <- ltable[, l]
            dv <- data[, d]
            if (is.factor(lv)) 
                lv <- as.character(lv)
            if (is.factor(dv)) 
                dv <- as.character(dv)
            if (is.character(lv)) 
                lv <- as.Date(lv) %>% as.numeric
            if (is.character(dv)) 
                dv <- as.Date(dv) %>% as.numeric
            breaks <- c(Inf, lv) %>% unique %>% sort
            indexes <- cut(dv, breaks, right = FALSE) %>% as.integer
            data[, l] <- breaks[indexes]
            ltable[, l] <- lv
        }
    }
    ret <- ln[!ln %in% c(dn, lranges)]
    m <- merge(data, ltable, all.x = TRUE, sort = FALSE)[, c("ID_", 
        ret)]
    o <- order(m$ID_)
    m[o, ret, drop = drop]
}
