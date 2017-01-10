require(data.table)

lookup <- function(data, ltable, by, drop = TRUE) {

  data$ID_ <- 1:nrow(data)

  ln <- names(ltable)
  dn <- names(data)

  if (!missing(by)) {
    by_start <- paste0(by, '_start')
    ln <- ln[ln %in% c(by, by_start)]
    dn <- dn[dn %in% by]
  }

  lranges <- ln[grepl(pattern = '_start$', x = ln)]


  if (length(lranges) > 0) {
    dranges <- strtrim(lranges, nchar(lranges) - 6)

    for(i in 1:length(lranges)) {

      l <- lranges[i]
      d <- dranges[i]

      lv <- ltable[, l]
      dv <- data[, d]

      if (is.factor(lv)) lv <- as.character(lv)
      if (is.factor(dv)) dv <- as.character(dv)

      if (is.character(lv)) lv <- as.Date(lv) %>% as.numeric
      if (is.character(dv)) dv <- as.Date(dv) %>% as.numeric

      breaks <- c(Inf, lv) %>% unique %>% sort
      indexes <- cut(dv, breaks, right = FALSE) %>% as.integer
      data[, l] <- breaks[indexes]
      ltable[, l] <- lv
    }
  }

  ret <- ln[!ln %in% c(dn, lranges)]
  m <- merge(data, ltable, all.x = TRUE, sort = FALSE)[, c('ID_', ret)]
  o <- order(m$ID_)
  m[o, ret, drop = drop]

}

cartesian_join <- function(
  data, V, data_key = names(data), V_name = 'X', as.data.frame = TRUE) {

  a = data.table(V)
  names(a) <- V_name
  setkeyv(a, V_name)
  b = as.data.table(x = data)
  setkeyv(b, data_key)
  out <- a[, as.list(b), by = key(a)]

  if (as.data.frame) {
    return(as.data.frame(out))
  } else {
    return(out)
  }

}
