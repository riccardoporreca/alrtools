# Author: Adam L. Rich
# Date:   November 4, 2017
# Description
#
#   Putting everything back in one file...
#


safe_ifelse <- function (test, yes, no) {

  # This is to prepare for calling the base package function


  # Unfactor yes and no if they are factors
  if (is.factor(yes))  yes <- levels(yes)[yes]
  if (is.factor(no))   no  <- levels(no)[no]


  # The original function follows
  base::ifelse(test, yes, no)

}

safe_merge <- function(
  x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by,
  all = FALSE, all.x = all, all.y = all,
  sort = TRUE, suffixes = c(".x",".y"),
  incomparables = NULL, ...){

  # Checks to see if by.x and by.y are unique keys

  l.x <- nrow(x)
  l.y <- nrow(y)

  k.x <- nrow(as.data.frame(unique(x[,by.x])))
  k.y <- nrow(as.data.frame(unique(y[,by.y])))

  u.x <- (k.x == l.x)
  u.y <- (k.y == l.y)

  # Warn if both are not unique
  if(!u.x & !u.y){
    warning('Merge is not safe: by is not a unique key')
  }

  base::merge(x, y, by, by.x, by.y, all, all.x, all.y, sort, suffixes, incomparables, ...)

}

pmean <- function(..., na.rm = TRUE) {
  m <- cbind(...)
  apply(m, 1, mean, na.rm = na.rm)
}

pswitch <- function(EXPR, ...) {
  m <- cbind(...)
  n <- nrow(m)
  m[n * (EXPR - 1) + 1:n]
}

# read.csv0 <- Curry(read.csv, stringsAsFactors = FALSE)

copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
}

paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

e <- exp(1L)

quarter_name <- function(d){
  m <- rep(1:4, each = 3)[as.integer(format(d, '%m'))]
  paste(format(d, '%Y'), 'q', m, sep = '')
}

year <- function(d){
  as.integer(format(d, '%Y'))
}

month <- function(d) {
  as.integer(format(d, '%m'))
}

day <- function(d){
  as.integer(format(d, '%d'))
}

quarter <- function(d){
  rep(1:4, each = 3)[Month(d)]
}

left <- function(string, n) {
  substring(
    text = string,
    first = 1,
    last = n
  )
}

right <- function(string, n) {

  out.len <- max(length(string), length(n))
  string <- rep(string, length.out = out.len)
  n <- rep(n, length.out = out.len)

  string.nchar <- nchar(string)

  substring(
    text = string,
    first = string.nchar - n + 1,
    last = nchar(string)
  )
}

mid <- function(string, start, end) {
  substring(string, start, end)
}

trim <- function(x) {
  gsub("(^ +)|( +$)", "", x);
}

open_test <- function(test, pkg = basename(getwd()), engine = 'testthat') {

  test <- as.character(match.call(expand.dots = FALSE)$test)
  test_file <- paste0('./tests/', engine, '/test_', test, '.R')
  test_dir <- dirname(test_file)

  if (file.exists(test_file)) {
    file.edit(test_file)
    return(invisible(NULL))
  }

  if (!dir.exists(test_dir))
    dir.create(test_dir, recursive = TRUE)

  knit_env <- list(f = test, pkg = pkg)
  RCode <- knit(text = test_template, envir = knit_env)
  writeChar(RCode, test_file)

  file.edit(test_file)
  return(invisible(NULL))
}

load_env <- function(RData, env = new.env()){
  # con <- gzfile(RData)
  # on.exit(close(con))
  base::load(RData, env)
  return(env)
}

env_name <- function(env) {
  n <- base::environmentName(env)
  if (n == '') {
    return(capture.output(env))
  } else {
    return(n)
  }
}

clear_env <- function(pos = .GlobalEnv){
  rm(list = ls(pos), pos = pos)
}

source_env <-function(RScript, env = new.env()){
  with(env, {
    base::source(RScript, local = TRUE)
    env
  })
}

ExecuteIn <- function(f, env, ...) {
  if (!'environment' %in% class(env))
    env <- as.environment(env)
  if (identical(parent.env(env), emptyenv()))
    parent.env(env) <- globalenv()
  gox <- f
  environment(gox) <- env
  gox(...)
}

normalize_path <- function(p){
  p <- base::normalizePath(p, winslash = '/', mustWork = TRUE)
  if(grepl('(/|\\\\)$', p)) {
    p <- paste(p, '.', sep = '')
  }
  p
}

write.function <- function(f, file, name = NULL) {
  if (is.null(name)) {
    name <- as.character(match.call(expand.dots = FALSE)$f)
  }
  kode <- deparse(f)
  kode[1] <- paste0(name, ' <- ', kode[1])
  cat(kode, file = file, sep = '\n')
  invisible(kode)
}

write.package <- function(pkg, folder) {
  dir.create(folder, showWarnings = FALSE)
  fs <- ls(getNamespace(pkg), all = TRUE)
  for (f in fs) {
    g <- get(f, envir = getNamespace(pkg))
    p <- paste0(folder, '/', f, '.R')
    write.function(g, p, f)
  }
}

Curry <- function(FUN, ...) {

  # Curry
  #   To curry a function means to pre-evaluate some of the arguments
  #   So, if you have a function
  #
  #     sum <- function(a, b) {a + b}
  #
  #   And you always want b to be 1, you could define
  #
  #     add.one <- Curry(sum, b = 1)
  #
  #   Which is the same as
  #
  #     function(a) {a + 1}
  #

  # Curry works because list evaluates its arguments
  .orig = list(...);

  # The ... referenced here is the remainder
  #   of the args passed to the curried function
  #   when it is called
  function(...) do.call(FUN, c(.orig, list(...)))
}

HFactory  <- function(name, ignore.case = TRUE){
  is.function <- base::is.function(name)

  # Previously, name had to be a charater string representing the name of an object (how annoying!)
  # Because of the next line, it can now be an object!
  # Based on code of "rm" function
  name <- as.character(match.call(expand.dots = FALSE)$name)

  # If name is a function object, trail with "()"
  if(is.function) name <- paste(name, '()', sep = '')

  # Also, now it allows for a case-sensitive function
  if(ignore.case) ic = 'TRUE' else ic = 'FALSE'

  s <- paste('
             function(pattern = \'\'){
             names(', name, ')[grep(pattern, names(', name, '), ignore.case = ',ic,')]
}', sep = '')

  return(eval(parse(text = s)))
}

lookup <- function(data, ltable, by, drop = TRUE) {

  if (nrow(data) > 0)
    data$ID_ <- 1:nrow(data)
  else
    data$ID_ <- integer(0)


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

CNumeric <- function(v) {

  # Trim first
  v <- trim(v)

  # Check to see if negatives are wrapped in ()
  l <- grepl('^\\(.*\\)$', v)
  w <- ifelse(l, paste('-', substring(v, 2, nchar(v) - 1), sep = ''), v)

  # Check to see if we have terminating %
  m <- grepl('.*%$', w)
  x <- ifelse(m, substring(w, 1, nchar(w) - 1), w)

  # Remove any commas
  y <- gsub(pattern = '[,]', replacement = '', x)

  # Cast and return
  #   Don't forget to divide percents by 100!
  kast <- as.numeric(y)
  ifelse(m, kast/100, kast)

}

