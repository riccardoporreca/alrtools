sqlExecFile <- function(file, connection_string) {

  sql <- readLines(con = file)
  sql <- gsub('^go[ ]*$', '', sql)
  sql <- paste0(sql, collapse = '\n')
  sql <- strsplit(sql, split = ';') %>% unlist

  con_str <- gsub('\n[ ]*', '', connection_string)
  con <- RODBC::odbcDriverConnect(con_str)

  on.exit({
    RODBC::odbcClose(con)
  })

  out <- list()
  for (i in 1:length(sql)) {
    exp <- sql[i]
    res <- RODBC::sqlQuery(con, exp, stringsAsFactors = FALSE)
    if (class(res) == 'data.frame') {
      out <- res
    }
  }

  return(out)

}

check <- function(v, ignore = logical(0), name = NULL, n = 30) {

  # Dates:
  if (class(v) == 'Date')
    v <- year(v)

  stopifnot(is.vector(v))
  stopifnot(is.vector(ignore))

  vname <- as.character(match.call(expand.dots = FALSE)$v)
  vname <- vname[length(vname)]
  if (is.null(name))
    name <- vname

  l_ignored <- v %in% ignore
  l_NA <- is.na(v)



  w <- v[!l_ignored & !l_NA]
  ignored <- v[l_ignored]
  nNA <- sum(l_NA)


  # Tabulate non-key character fields
  # Tabulate numeric fields with less than 30 values
  # Check range on the rest of the numeric fields
  # Count by year and range for date fields

  # Convert factors to character strings
  if (is.factor(w))
    v <- as.character(w)

  u <- unique(w)
  nu <- length(u)
  nw <- length(w)
  tv <- table(w)

  tdf <- data.frame(
    Value = names(tv),
    Count = unname(unclass(tv)),
    stringsAsFactors = FALSE
  )

  tdf <- tdf[order(tdf$Count, decreasing = TRUE), ]

  if (nrow(tdf) > n) {
    tdf_main <- tdf[1:n, ]
    tdf_other <- tdf[n:nrow(tdf), ]
    tdf_main$Value[n] <- "ALL OTHER"
    tdf_main$Count[n] <- sum(tdf_other$Count)
  } else {
    tdf_main <- tdf
    tdf_other <- tdf[FALSE, ]
  }

  tdf_main <- rbind(tdf_main, data.frame(
    Value = c('NA', 'IGNORED'),
    Count = c(nNA, sum(l_ignored)),
    stringsAsFactors = FALSE
  ))

  message(paste0('*** ', name, ' ***'))
  print(tdf_main, row.names = FALSE)

  message('\nSummary of all values:')
  print(summary(w))

  if (nrow(tdf_other) > 0) {
    message('\nSummary of "ALL OTHER" values:')
    print(summary(w[w %in% tdf_other$Value]))
  }

  invisible(tdf_main)

}

check_all <- function(data, n = 30, skip_keys = TRUE) {
  for (i in 1:length(data)) {
    d <- data[, i, drop = TRUE]
    nom <- names(data)[i]
    key <- !any(duplicated(d))
    if (!key | !skip_keys)
      check(d, name = nom, n = n)
  }
  invisible(NULL)
}

dot_relations <- function(data, ignore = '', name = NULL, check = TRUE) {

  if (is.null(name))
    name <- as.character(match.call(expand.dots = FALSE)$data)

  nom <- names(data)
  nom2dot <- gsub('(.*)[.].*$', '\\1', nom)
  stems <- unique(nom2dot[duplicated(nom2dot)])
  l <- stems %in% ignore
  stems_good <- stems[!l]

  if (check) {
    for (i in 1:length(stems_good)) {
      stem <- stems_good[i]
      cols <- nom[nom2dot == stem]
      check_dot_relation(data = data, cols = cols, name = name)
    }
  }

  invisible(stems_good)
}

check_dot_relation <- function(data, cols = character(0), name = NULL) {

  if (is.null(name))
    name <- as.character(match.call(expand.dots = FALSE)$data)

  stopifnot(length(cols) > 1)
  cdata <- data[, cols]

  key <- get_key(data)
  stopifnot(length(key) > 0)

  n <- length(cdata)
  noms <- names(cdata)

  for (i in 1:n) {
    cdata[[n + i]] <- is.na(cdata[[i]])
  }

  for (i in 1:(n - 1)) {
    cdata[[2*n + i]] <- ifelse(
      cdata[[n + i]] | cdata[[2 * n]],
      TRUE,
      cdata[[i]] == cdata[[n]]
    )
  }

  cdata[[3*n]] <- 1
  names(cdata) <- c(
    noms,
    paste0('is.na', 1:n),
    paste0('is.eq', 1:(n - 1)),
    'Count')

  agg_comp <- aggregate(
    x = cdata[, 'Count', drop = FALSE],
    by = cdata[, (n+1):(3*n-1)],
    FUN = sum
  )

  l_neq <- rowSums(
    cdata[, (2*n+1):(3*n-1), drop = FALSE]) < (n - 1)

  if (sum(l_neq) > 0) {

    agg_neq <- cdata %>%
      filter(l_neq) %>%
      group_by_at(1:n) %>%
      summarize(Count = sum(Count)) %>%
      arrange(desc(Count)) %>%
      as.data.frame

    m <- min(10, nrow(agg_neq))
    l_uniq <- !duplicated(cdata)
    sdata <- data[l_uniq, c(key, noms)]
    diffs <- (agg_neq %>% inner_join(sdata, by = cols))[1:m, ]

    diff_count <- sum(agg_neq$Count)
    diffs_count <- sum(diffs$Count)

  } else {
    agg_neq <- NULL
    diffs <- NULL
  }


  # Print results
  message(paste0('*** ', name,': ', paste0(cols, collapse = ', '),' ***'))
  print(agg_comp, row.names = FALSE)

  if (any(l_neq)) {
    message('\nSamples of Differences (',
            diffs_count,' of ', diff_count, ', ' ,
            round(diffs_count / diff_count, 2) * 100,'%):')
    print(diffs)
  }

  invisible(list(
    agg_comp = agg_comp,
    agg_neq = agg_neq,
    diffs = diffs,
    cols = cols,
    name = name,
    l_neq = l_neq
  ))

}

get_key <- function(data) {
  for(i in 1:ncol(data)) {
    v <- data[, i]
    if (all(!duplicated(v))) {
      n <- names(data)[i]
      if (is.null(n))
        n <- i
      return(n)
    }
  }
  warning('There is no single column key in data.')
}

convert_columns <- function(data) {
  m <- ncol(data)
  for (i in 1:m) {
    v <- data[[i]]
    w <- convert_column(v)
    data[[i]] <- w
  }
  return(data)
}

convert_column <- function(v, set_blank = NA) {

  stopifnot(length(class(v)) == 1)
  stopifnot(length(set_blank) == 1)

  # Preference, if possible to not lose data
  #   1. Date and logical
  #   2. integer
  #   3. numeric
  #   4. character

  n <- length(v)

  # if v is character, set blanks
  if (class(v) == 'character')
    v[grepl('^\\s$|^$', v)] <- set_blank

  nNA <- sum(is.na(v))

  # If v is logical or date, just return
  if (class(v) %in% c('logical', 'Date'))
    return(v)

  # Try to convert to date
  w <- rep(NA, n)
  try(w <- as.Date(v), silent = TRUE)
  if (sum(is.na(w)) == nNA)
    return(w)

  # If v is numeric or integer and all 0, 1 convert to logical
  if (class(v) %in% c('integer', 'numeric')) {
    nbit <- sum(v %in% c(0L, 1L, NA_integer_))
    if (nbit == n)
      return(as.logical(v))
  }

  # Check character for logical
  if (class(v) == 'character') {
    nbit <- sum(v %in% c('0', '1', 'TRUE', 'FALSE', 'T', 'F', NA))
    if (nbit == n) {
      v[v == '0'] <- 'FALSE'
      v[v == '1'] <- 'TRUE'
      return(as.logical(v))
    }
  }


  # if v is integer, return
  if (class(v) %in% c('integer', 'numeric'))
    return(v)


  # # if v is numeric, try to convert to integer then return
  # if (class(v) == 'numeric') {
  #   w <- as.integer(v)
  #   if (sum(is.na(w)) == nNA) {
  #     return(w)
  #   } else {
  #     return(v)
  #   }
  # }


  # # Try to convert character to integer
  # suppressWarnings(w <- as.integer(v))
  # if (sum(is.na(w)) == nNA)
  #   return(w)


  # Try to convert character to numeric
  suppressWarnings(w <- as.numeric(v))
  if (sum(is.na(w)) == nNA)
    return(w)


  # Return character
  return(v)

}

# Author: Adam L. Rich
# Date:   September 1, 2017
# Description:
#
#   Resources for reporting
#



require(BZLYUtil)
require(magrittr)
require(dplyr)
require(RODBC)
require(tidyr)



.servers <- read.csv('./_generic/databases.csv', stringsAsFactors = FALSE)
.catalogs <- .servers$CAT


sqlConnectionString <- function(db, env = 'PRO') {
  env <- left(env, 3)
  server <- .servers[.servers$CAT == db, env]
  cstr <- sprintf("
                  Driver={SQL Server};
                  Server={%s};
                  Database={%s};
                  TrustedConnection=yes", server, db)
  cstr <- gsub('\n[ ]*', '', cstr)
  return(cstr)
}


sqlExecFile <- function(file, connection_string) {

  sql <- readLines(con = file)
  sql <- gsub('^go[ ]*$', '', sql)
  sql <- paste0(sql, collapse = '\n')
  sql <- strsplit(sql, split = ';') %>% unlist

  con_str <- gsub('\n[ ]*', '', connection_string)
  con <- RODBC::odbcDriverConnect(con_str)

  on.exit({
    RODBC::odbcClose(con)
  })

  out <- list()
  for (i in 1:length(sql)) {
    exp <- sql[i]
    res <- RODBC::sqlQuery(con, exp, stringsAsFactors = FALSE)
    if (class(res) == 'data.frame') {
      out <- res
    }
  }

  return(out)

}


check <- function(v, ignore = logical(0), name = NULL, n = 30) {

  # Dates:
  if (class(v) == 'Date')
    v <- year(v)

  stopifnot(is.vector(v))
  stopifnot(is.vector(ignore))

  vname <- as.character(match.call(expand.dots = FALSE)$v)
  vname <- vname[length(vname)]
  if (is.null(name))
    name <- vname

  l_ignored <- v %in% ignore
  l_NA <- is.na(v)



  w <- v[!l_ignored & !l_NA]
  ignored <- v[l_ignored]
  nNA <- sum(l_NA)


  # Tabulate non-key character fields
  # Tabulate numeric fields with less than 30 values
  # Check range on the rest of the numeric fields
  # Count by year and range for date fields

  # Convert factors to character strings
  if (is.factor(w))
    v <- as.character(w)

  u <- unique(w)
  nu <- length(u)
  nw <- length(w)
  tv <- table(w)

  tdf <- data.frame(
    Value = names(tv),
    Count = unname(unclass(tv)),
    stringsAsFactors = FALSE
  )

  tdf <- tdf[order(tdf$Count, decreasing = TRUE), ]

  if (nrow(tdf) > n) {
    tdf_main <- tdf[1:n, ]
    tdf_other <- tdf[n:nrow(tdf), ]
    tdf_main$Value[n] <- "ALL OTHER"
    tdf_main$Count[n] <- sum(tdf_other$Count)
  } else {
    tdf_main <- tdf
    tdf_other <- tdf[FALSE, ]
  }

  tdf_main <- rbind(tdf_main, data.frame(
    Value = c('NA', 'IGNORED'),
    Count = c(nNA, sum(l_ignored)),
    stringsAsFactors = FALSE
  ))

  message(paste0('*** ', name, ' ***'))
  print(tdf_main, row.names = FALSE)

  message('\nSummary of all values:')
  print(summary(w))

  if (nrow(tdf_other) > 0) {
    message('\nSummary of "ALL OTHER" values:')
    print(summary(w[w %in% tdf_other$Value]))
  }

  invisible(tdf_main)

}


check_all <- function(data, n = 30, skip_keys = TRUE) {
  for (i in 1:length(data)) {
    d <- data[, i, drop = TRUE]
    nom <- names(data)[i]
    key <- !any(duplicated(d))
    if (!key | !skip_keys)
      check(d, name = nom, n = n)
  }
  invisible(NULL)
}


dot_relations <- function(data, ignore = '', name = NULL, check = TRUE) {

  if (is.null(name))
    name <- as.character(match.call(expand.dots = FALSE)$data)

  nom <- names(data)
  nom2dot <- gsub('(.*)[.].*$', '\\1', nom)
  stems <- unique(nom2dot[duplicated(nom2dot)])
  l <- stems %in% ignore
  stems_good <- stems[!l]

  if (check) {
    for (i in 1:length(stems_good)) {
      stem <- stems_good[i]
      cols <- nom[nom2dot == stem]
      check_dot_relation(data = data, cols = cols, name = name)
    }
  }

  invisible(stems_good)
}


check_dot_relation <- function(data, cols = character(0), name = NULL) {

  if (is.null(name))
    name <- as.character(match.call(expand.dots = FALSE)$data)

  stopifnot(length(cols) > 1)
  cdata <- data[, cols]

  key <- get_key(data)
  stopifnot(length(key) > 0)

  n <- length(cdata)
  noms <- names(cdata)

  for (i in 1:n) {
    cdata[[n + i]] <- is.na(cdata[[i]])
  }

  for (i in 1:(n - 1)) {
    cdata[[2*n + i]] <- ifelse(
      cdata[[n + i]] | cdata[[2 * n]],
      TRUE,
      cdata[[i]] == cdata[[n]]
    )
  }

  cdata[[3*n]] <- 1
  names(cdata) <- c(
    noms,
    paste0('is.na', 1:n),
    paste0('is.eq', 1:(n - 1)),
    'Count')

  agg_comp <- aggregate(
    x = cdata[, 'Count', drop = FALSE],
    by = cdata[, (n+1):(3*n-1)],
    FUN = sum
  )

  l_neq <- rowSums(
    cdata[, (2*n+1):(3*n-1), drop = FALSE]) < (n - 1)

  if (sum(l_neq) > 0) {

    agg_neq <- cdata %>%
      filter(l_neq) %>%
      group_by_at(1:n) %>%
      summarize(Count = sum(Count)) %>%
      arrange(desc(Count)) %>%
      as.data.frame

    m <- min(10, nrow(agg_neq))
    l_uniq <- !duplicated(cdata)
    sdata <- data[l_uniq, c(key, noms)]
    diffs <- (agg_neq %>% inner_join(sdata, by = cols))[1:m, ]

    diff_count <- sum(agg_neq$Count)
    diffs_count <- sum(diffs$Count)

  } else {
    agg_neq <- NULL
    diffs <- NULL
  }


  # Print results
  message(paste0('*** ', name,': ', paste0(cols, collapse = ', '),' ***'))
  print(agg_comp, row.names = FALSE)

  if (any(l_neq)) {
    message('\nSamples of Differences (',
            diffs_count,' of ', diff_count, ', ' ,
            round(diffs_count / diff_count, 2) * 100,'%):')
    print(diffs)
  }

  invisible(list(
    agg_comp = agg_comp,
    agg_neq = agg_neq,
    diffs = diffs,
    cols = cols,
    name = name,
    l_neq = l_neq
  ))

}


get_key <- function(data) {
  for(i in 1:ncol(data)) {
    v <- data[, i]
    if (all(!duplicated(v))) {
      n <- names(data)[i]
      if (is.null(n))
        n <- i
      return(n)
    }
  }
  warning('There is no single column key in data.')
}


convert_columns <- function(data) {
  m <- ncol(data)
  for (i in 1:m) {
    v <- data[[i]]
    w <- convert_column(v)
    data[[i]] <- w
  }
  return(data)
}


convert_column <- function(v, set_blank = NA) {

  stopifnot(length(class(v)) == 1)
  stopifnot(length(set_blank) == 1)

  # Preference, if possible to not lose data
  #   1. Date and logical
  #   2. integer
  #   3. numeric
  #   4. character

  n <- length(v)

  # if v is character, set blanks
  if (class(v) == 'character')
    v[grepl('^\\s$|^$', v)] <- set_blank

  nNA <- sum(is.na(v))

  # If v is logical or date, just return
  if (class(v) %in% c('logical', 'Date'))
    return(v)

  # Try to convert to date
  w <- rep(NA, n)
  try(w <- as.Date(v), silent = TRUE)
  if (sum(is.na(w)) == nNA)
    return(w)

  # If v is numeric or integer and all 0, 1 convert to logical
  if (class(v) %in% c('integer', 'numeric')) {
    nbit <- sum(v %in% c(0L, 1L, NA_integer_))
    if (nbit == n)
      return(as.logical(v))
  }

  # Check character for logical
  if (class(v) == 'character') {
    nbit <- sum(v %in% c('0', '1', 'TRUE', 'FALSE', 'T', 'F', NA))
    if (nbit == n) {
      v[v == '0'] <- 'FALSE'
      v[v == '1'] <- 'TRUE'
      return(as.logical(v))
    }
  }


  # if v is integer, return
  if (class(v) %in% c('integer', 'numeric'))
    return(v)


  # # if v is numeric, try to convert to integer then return
  # if (class(v) == 'numeric') {
  #   w <- as.integer(v)
  #   if (sum(is.na(w)) == nNA) {
  #     return(w)
  #   } else {
  #     return(v)
  #   }
  # }


  # # Try to convert character to integer
  # suppressWarnings(w <- as.integer(v))
  # if (sum(is.na(w)) == nNA)
  #   return(w)


  # Try to convert character to numeric
  suppressWarnings(w <- as.numeric(v))
  if (sum(is.na(w)) == nNA)
    return(w)


  # Return character
  return(v)

}


restate <- function(v, ...) {
  dots <- as.list(substitute(list(...)))[-1L]
  if (length(dots) > 0) {
    vold <- names(dots)
    vnew <- as.character(dots)
  }

  # vold <- c("0", "No", "1", "2 or more", "Yes")
  # vnew <- c('No', "No", 'Yes', 'Yes', "Yes")

  vuniq <- unique(v)
  vmiss <- vuniq[!vuniq %in% vold]

  vin <- c(vold, vmiss)
  vout <- c(vnew, vmiss)

  index <- match(v, vin)
  w <- convert_column(vout[index])
  return(w)
}


read.txt <- function(txt) {
  lns <- readLines(txt)
  paste0(lns, collapse = '\n')
}


embed.txt <- function(txt) {
  blob <- read.txt(txt)
  paste0('<pre>\n', blob, '\n</pre>')
}


