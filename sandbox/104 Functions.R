#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   March 11, 2013
#   Description:
#  
#     Some Functions
#  
#   Copyright Notice:
#     THE FOLLOWING APPLIES TO THIS ENTIRE FILE
#       AND/OR ANY PORTION THEREOF
#     COPYRIGHT ADAM L. RICH, 2013
#     THIS HAS BEEN DISTRIBUTED AS PART OF THE 2013 CAS RPM SEMINAR
#     YOU CAN USE CODE HERE IN YOUR OWN WORK 
#       PROVIDED YOU DO NOT TAKE CREDIT FOR IT
#     YOU AGREE TO NOT POST THIS IN ANY WAY ON THE INTERNET
#     YOU CAN DISTRIBUTE TO THOSE YOU TRUST TO FOLLOW THESE RULES
#     THIS MESSAGE MUST STAY WITH THIS CODE
#     

require(RODBC)



# Some functions inspired by Excel

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




# A function to open a internet search from R
google <- function(query) {
  
  url <- paste(
    "http://www.google.com/search?q=", 
    query, 
    "&rls=com.microsoft:en-us:IE-SearchBox&ie=UTF-8&oe=UTF-8&sourceid=ie7&surl=1",
    sep = ''
  )
  
  command <- paste('start iexplore "', url, '"', sep = '')
  
  shell(command, wait = FALSE)
  
  invisible()
}



# Copy data out of R
copy.table <- function(obj, size = 4096) {
  
  clip <- paste('clipboard-', size, sep = '')
  
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)
  
}


# Paste data into R
paste.table <- function() {
  
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  
  return(df)
}



# Takes an Excel 2007 path
#   And the name of a sheet (or named range)
#   Returns a data frame
#   TODO: figure out how to get first row (not in header, but as values)
SheetToDataFrame <- function(xls.name, sheet, pConn = NULL) {
  
  keep.open <- !is.null(pConn)
  
  table <- tryCatch(
    expr = {
      if(!keep.open) pConn <- odbcConnectExcel2007(xls.name)
      a <- sqlFetch(
        pConn, 
        sheet, 
        as.is = TRUE, 
        colnames = FALSE, 
        rownames = FALSE
      )
      a
    },
    
    warning = function(war){
      stop(paste('WARNING in SheetToDataFrame:', war))
    },
    
    error = function(err){
      stop(paste('ERROR in SheetToDataFrame:', err))
    },
    
    finally = {
      if(!keep.open) odbcClose(pConn)
    }
  )
  
  return(table)
}


# List the tables available in a connection
GetTables <- function(conn) {
  t <- sqlTables(conn)
  
  # Delete hidden named ranges
  t <- t[!grepl('^_.*', t$TABLE_NAME), ]
  
  # Only get TABLE of SYSTEM TABLE
  t <- t[t$TABLE_TYPE %in% c('TABLE', 'SYSTEM TABLE'), ]
  
  # Only return the name
  t <- t$TABLE_NAME
  
  return(t)
}


# List the tables available in an Excel 2007 file
#   Will return a list of *all* named ranges, even scalars
#   And will return worksheet (appends $)
#   TODO: check for scalars and do not return
GetTablesExcel2007 <- function(xls.name) {
  conn <- odbcConnectExcel2007(xls.name)
  t <- GetTables(conn)
  odbcClose(conn)
  return(t)
}


# A smart version of as.numeric
#   I don't like the naming convention here, but it follows VBA using C*
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


# Cleans up the output of GetTablesExcel2007
#   TODO: could probably use regex
TrimTableNames <- function(x){
  n <- nchar(x) 
  l <- substring(x, 1, 1) == "'"
  
  if(sum(l & n < 2) > 0) stop('x is not well formed')
  
  x <- ifelse(l, substring(x, 2, n - 1), x)
  n <- ifelse(l, n - 2, n)
  
  l <- substring(x, n, n) == '$'
  
  x <- ifelse(l, substring(x, 1, n - 1), x)
  
  return(x)
}


# A couple functions for help with Functional programming
#   Inspired by package:roxygen via
#   http://stackoverflow.com/questions/2228544/
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



# Loads an RData file to an environment
LoadToEnvironment <- function(RData, env = new.env()){
  con <- gzfile(RData)
  on.exit(close(con))
  load(RData, env)
  return(env)
}



EnvironmentName <- function(env) {
  n <- base::environmentName(env)
  if (n == '') {
    return(capture.output(env))
  } else {
    return(n)
  }
}



HFactory <- function(name, ignore.case = TRUE){
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


# Some date functions
QuarterName <- function(d){
  m <- rep(1:4, each = 3)[as.integer(format(d, '%m'))]
  paste(format(d, '%Y'), 'q', m, sep = '')
}



Year <- function(d){
  as.integer(format(d, '%Y'))
}



Month <- function(d) {
  as.integer(format(d, '%m'))
}



Day <- function(d){
  as.integer(format(d, '%d'))
}



Quarter <- function(d){
  rep(1:4, each = 3)[Month(d)]
}



NormalizePath <- function(p){
  p <- base::normalizePath(p, winslash = '/', mustWork = TRUE)
  if(grepl('(/|\\\\)$', p)) {
    p <- paste(p, '.', sep = '')
  }
  p
}


# Similar to LoadToEnvironment
#   but for scripts, not RData files
SourceToEnvironment <-function(RScript, env = new.env()){
  with(env, {
    base::source(RScript, local = TRUE)
    env
  })
}



fix.header <- function(obj, map) {
  # Looks up names(obj) in map to get new vector names
  # If a name is not found, it is unchanged
  # map must be a list where the names are supposed to match to the names of obj
  #   the thing the names point to should be a one element character vector
  
  # map is passed in as a list
  #   1. convert it to a dataframe
  #   2. transpose it
  #   3. take the first column
  #   4. convert that to a vector
  map <- data.frame(old = names(map),
                    new = as.character(t(as.data.frame(map))[,1]),
                    stringsAsFactors = FALSE)
  
  # Map and rename
  #   names() always returns a vector of character strings
  n <- merge(data.frame(old = names(obj), stringsAsFactors = FALSE), map, all.x = TRUE, sort = FALSE)
  n$new <- ifelse(is.na(n$new), n$old, n$new)
  names(obj) <- n$new
  
  # Return obj
  obj
}



# FancyCut
#   A function that takes a vector of numbers,
#     a vector of intervals in interval notation, like [1,100] etc.
#     a vector of labels of the same length as the intervals
#
#   Like cut it returns a factor, but how it handles boundaries is more explicit
#
#   Example to use:
#
#     x <- round(runif(1000, 0, 10))
#     intervals <- c('(0,2]','(2,5)','[5,10]')
#     buckets <- c('Small','Medium','Large')
#
#     fc <- FancyCut(x, intervals, buckets)
#
#     df <- data.frame(x, fc)
#

FancyCut <- function(x, intervals, buckets = intervals, 
                     na.bucket = NA, unmatched.bucket = NA,
                     out.as.factor = TRUE) {
  
  # Make sure that intervals and buckets are the same length
  l <- length(intervals)
  if(l != length(buckets)) {
    stop('FancyCut requires a 1-1 map from intervals to buckets')
  }
  
  out <- rep(NA, length(x))
  for(index in 1:l) {
    i <- intervals[index]
    b <- buckets[index]
    n <- nchar(i[1])
    left <- substr(i, 1, 1)
    right <- substr(i, n, n)
    bounds <- strsplit(substr(i, 2, n - 1), ",")
    upper <- as.numeric(bounds[[1]][2])
    lower <- as.numeric(bounds[[1]][1])
    
    mask <- rep(FALSE, length(x))
    if(left == '[' & right == ']') {mask <- x >= lower & x <= upper}
    if(left == '[' & right == ')') {mask <- x >= lower & x <  upper}
    if(left == '(' & right == ']') {mask <- x >  lower & x <= upper}
    if(left == '(' & right == ')') {mask <- x >  lower & x <  upper}
    
    out[mask] <- b
  }
  
  out[is.na(out)]  <- unmatched.bucket
  out[is.na(x)]    <- na.bucket
  
  levels <- unique(c(buckets, na.bucket, unmatched.bucket))
  
  if(out.as.factor) {
    return(factor(
      out, 
      levels = levels,
      exclude = NULL
    ))
  } else {
    return(out)
  }
}

