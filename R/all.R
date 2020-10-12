# Author: Adam L. Rich
# Date:   November 4, 2017
#         May 14, 2020
# Description
#
#   A bunch of convenience functions
#
#   [May 14, 2020 ALR]
#   read.csv0 is back!
#   lookup removed
#   commented out any functions I don't use
#   copy.table and paste.table removed, use clipr instead
#
#   TODO
#     * Find other functions to add, like `vlookup`
#     * Finish writing tests
#     * Add R help inline to this file
#



#' Euler's number
#' @description 2.718282
"e"




#' A factor-safe version of \code{\link[base]{ifelse}}
#'
#' @description
#' The \code{\link[base]{ifelse}} function does weird
#' things when you pass it factors.
#' This version doesn't.
#'
#' @param
#' test   a vector of logicals
#' yes    values to return if test is TRUE
#' no     values to return if test if FALSE
#'
#' @return
#' For each position, \code{ifelse} returns
#' the value of \code{yes} or \code{no}
#' depending on the value of \code{test}.
#'
#' @examples
#' ifelse(
#'   c(TRUE, FALSE),
#'   c('Yes', 'Yeah!'),
#'   c('No', 'Nope'))
#'
#' # base ifelse does weird stuff with factors
#' yesf <- factor(c('Yes', 'Yeah!'))
#' nof <- factor(c('No', 'Nope'))
#' base::ifelse(c(T, F), yesf, nof)
#' ifelse(c(T, F), yesf, nof)
#'
#' @export
ifelse <- function (test, yes, no) {

  # This is to prepare for calling the base package function

  # Unfactor yes and no if they are factors
  if (is.factor(yes))  yes <- levels(yes)[yes]
  if (is.factor(no))   no  <- levels(no)[no]

  # The original function follows
  base::ifelse(test, yes, no)

}




#' Piecewise mean
#'
#' @description
#' \code{pmean} is to \code{\link[base]{mean}} what
#' \code{\link[base]{pmax}} is to \code{\link[base]{max}}.
#'
#' @param
#' ...    vectors
#' na.rm  a logical indicating whether missing values should be removed
#'
#' @return
#' Return the average of the numbers in the same position
#' across all inputs.
#'
#' @examples
#' pmean(1:10, 5, 11:20)
#' pmean(1:10, 5, c(11:19, NA))
#' pmean(1:10, 5, c(11:19, NA), na.rm = FALSE)
#'
#' @export
pmean <- function(..., na.rm = TRUE) {
  m <- cbind(...)
  apply(m, 1, mean, na.rm = na.rm)
}



#' Year of a date vector
#'
#' @param
#' d    a vector of dates
#'
#' @return
#' The year part of each value in \code{d}
#' as an integer.
#'
#' @examples
#' year(as.Date('2020-10-14'))
#' year(ISOdate(1923, 12, 2))
#'
#' @export
year <- function(d){
  as.integer(format(d, '%Y'))
}



#' Month of a date vector
#'
#' @param
#' d    a vector of dates
#'
#' @return
#' The month part of each value in \code{d}
#' as an integer.
#'
#' @examples
#' month(as.Date('2020-10-14'))
#' month(ISOdate(1923, 12, 2))
#'
#' @export
month <- function(d) {
  as.integer(format(d, '%m'))
}



#' Day of a date vector
#'
#' @param
#' d    a vector of dates
#'
#' @return
#' The day part of each value in \code{d}
#' as an integer.
#'
#' @examples
#' day(as.Date('2020-10-14'))
#' day(ISOdate(1923, 12, 2))
#'
#' @export
day <- function(d){
  as.integer(format(d, '%d'))
}



#' Quarter of a date vector
#'
#' @param
#' d    a vector of dates
#'
#' @return
#' The quarter (1, 2, 3, or 4) each date in \code{d}
#' falls in.
#'
#' @examples
#' quarter(as.Date('2020-01-14'))
#' quarter(ISOdate(1923, 12, 2))
#'
#' @export
quarter <- function(d){
  rep(1:4, each = 3)[month(d)]
}




#' Get a substring from the left
#'
#' @description
#' Inspired by the Excel function with the same name.
#'
#' @param
#' string    a vector of strings
#' n         a vector of integers (will be recycled, if needed)
#'
#' @return
#' The first \code{n} characters of each value in \code{string}.
#' If \code{n} has length 1, then that value is applied to all strings
#' because of recycling.
#' However, \code{n} can have more than one element!
#'
#'
#' @examples
#' left('lazy dog', 4)
#' left(c('lazy dog', 'blue dog'), 4)
#' left(c('lazy dog', 'red dog'), c(4, 3))
#'
#' @export
left <- function(string, n) {
  substring(
    text = string,
    first = 1,
    last = n
  )
}



#' Get a substring from the right
#'
#' @description
#' Inspired by the Excel function with the same name.
#'
#' @param
#' string    a vector of strings
#' n         a vector of integers (will be recycled, if needed)
#'
#' @return
#' The last \code{n} characters of each value in \code{string}.
#' If \code{n} has length 1, then that value is applied to all strings
#' because of recycling.
#' However, \code{n} can have more than one element!
#'
#'
#' @examples
#' right('lazy dog', 3)
#' right(c('lazy dog', 'blue dog'), 3)
#' right(c('lazy dog', 'red frog'), c(3, 4))
#'
#' @export
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



#' Get a substring from the middle
#'
#' @description
#' Inspired by the Excel function with the same name.
#'
#' @param
#' string    a vector of strings
#' start     a vector of starting points (will be recycled, if needed)
#' length    a vector of lengths (will be recycled, if needed)
#'
#' @return
#' The \code{length} characters of each value in \code{string}
#' starting with the \code{start} character.
#'
#' @examples
#' mid('lazy dog', 3, 2)
#' mid('lazy dog', 6, 3)
#' mid(c('lazy dog', 'blue dog'), 2, 2)
#' mid(c('lazy dog', 'red frog'), 6, c(3, 2))
#'
#' @export
mid <- function(string, start, length) {
  substring(string, start, start + length - 1)
}




#' Remove leading and trailing whitespace from characters
#'
#' @description
#' Inspired by the Excel function with the same name.
#'
#' @param
#' x    a vector of strings
#'
#' @return
#' \code{x} without any leading or trailing whitespace.
#'
#' @examples
#' trim('  leading spaces')
#' trim('trailing spaces   ')
#' trim('  leading and trailing spaces   ')
#' trim('\n\r\t Other kinds of whitespace \t\n\r')
#' trim("\n Doesn't affect internal whitespace  ")
#'
#' @export
trim <- function(x) {
  gsub("^(\\s+)|(\\s+)$", "", x);
}




#' Package dev: open a test file
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
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



#' Load Rdata to an environment
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
load_env <- function(RData, env = new.env()){
  # con <- gzfile(RData)
  # on.exit(close(con))
  base::load(RData, env)
  return(env)
}



#' Source R code to an environment
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
source_env <- function(RScript, env = new.env()){
  with(env, {
    base::source(RScript, local = TRUE)
    env
  })
}



#' Execute a function in an environment
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
execute_in <- function(f, env, ...) {
  if (!'environment' %in% class(env))
    env <- as.environment(env)
  if (identical(parent.env(env), emptyenv()))
    parent.env(env) <- globalenv()
  gox <- f
  environment(gox) <- env
  gox(...)
}





#' Write the code of a function to a file
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
write.function <- function(f, file, name = NULL) {
  if (is.null(name)) {
    name <- as.character(match.call(expand.dots = FALSE)$f)
  }
  kode <- deparse(f)
  kode[1] <- paste0(name, ' <- ', kode[1])
  cat(kode, file = file, sep = '\n')
  invisible(kode)
}




#' Write the code of a package to a file
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
write.package <- function(pkg, folder) {
  dir.create(folder, showWarnings = FALSE)
  fs <- ls(getNamespace(pkg), all = TRUE)
  for (f in fs) {
    g <- get(f, envir = getNamespace(pkg))
    p <- paste0(folder, '/', f, '.R')
    write.function(g, p, f)
  }
}




#' Pre-evaluate arguments of a function
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
curry <- function(FUN, ...) {

  # curry
  #   To curry a function means to pre-evaluate some of the arguments
  #   So, if you have a function
  #
  #     sum <- function(a, b) {a + b}
  #
  #   And you always want b to be 1, you could define
  #
  #     add.one <- curry(sum, b = 1)
  #
  #   Which is the same as
  #
  #     function(a) {a + 1}
  #

  # curry works because list evaluates its arguments
  .orig = list(...)

  # The ... referenced here is the remainder
  #   of the args passed to the curried function
  #   when it is called
  function(...) do.call(FUN, c(.orig, list(...)))
}




#' Header search function factory
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
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




#' Convert text to numbers
#'
#' @description
#' This function takes characters that look like numbers
#' and converts them to numbers.
#' Its name is based on the VBA function \code{CNumeric}.
#' It does some pre-processing before calling
#' \code{\link[base]{as.numeric}}.
#' \enumerate{
#'   \item converts factors
#'   \item removes all whitespace
#'   \item converts wrapping "()" to negatives
#'   \item removes commas
#'   \item converts percentages to decimals
#'   \item calls \code{\link[base]{as.numeric}}
#' }
#'
#' @param v           a vector
#'
#' @return A vector of numerics. Returns \code{NA}
#' when conversion is not possible after application
#' of the rules above.
#'
#' @examples
#' # Commas are removed
#' cnumeric(c('1,000,000', '2,000.03'))
#'
#' # But, we don't check to make sure that
#' # commas are in the right place first
#' cnumeric(c('1,0,0', '2,0000.03'))
#'
#' # Accounting-style negatives
#' cnumeric(c('(1,000.92)', '(4)'))
#'
#' # Percents are converted
#' cnumeric(c('28.3%', '-1.3%', '(15%)'))
#'
#' # If scientific notation is present, R knows what to do
#' cnumeric(c('3e7', '5e-1'))
#'
#' # TODO it doesn't deal with currencies yet
#' cnumeric(c('USD 0.10', '$14.34'))
#'
#' @export
cnumeric <- function(v) {

  # If this has levels, unlevel it
  if (!is.null(levels(v)) & is.integer(unclass(v))) {
    v <- levels(v)[unclass(v)]
  }

  # Trim first
  v <- gsub(pattern = '\\s*', replacement = '', v)

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




#' Read CSVs without factors
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
read.csv0 <- curry(utils::read.csv, stringsAsFactors = FALSE)






#' Excel-like \code{VLOOKUP} function
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
vlookup <- function(lookup_value, table_array, col_index_number, type = 0, lookup_index = 1) {
  lookup_index <- lookup_index[1]
  lookup_value <- tolower(lookup_value)
  levels <- tolower(table_array[, lookup_index])
  table_array[factor(lookup_value, levels = levels), col_index_number]
}






#' Excel-like \code{VLOOKUP} function
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
VLOOKUP <- function(...) {vlookup(...)}






#' Doc writing: get LaTeX of a matrix
#'
#' @description
#' TODO
#'
#' @param
#' TODO      Note
#'
#' @return
#' TODO
#'
#' @examples
#' TODO
#'
#' @export
matrix2latex <- function(
  M, one_line = TRUE, print = TRUE, ret = !print) {
  M <- as.matrix(M)
  if (one_line) {
    newline = ' '
  } else {
    newline = '\n'
  }
  rows <- apply(M, 1, function(r) paste0(r, collapse = ' & '))
  rows_collapsed <- paste0(rows, collapse = paste0(' \\\\', newline))
  latex <- paste0(
    c('\\begin{bmatrix}', rows_collapsed, '\\end{bmatrix}'),
    collapse = newline)
  if (print) message(latex)
  if (ret) return(latex)
  invisible()
}





#' Intersection of sets
#'
#' @description
#' Give me a bunch of vectors.
#' I'll give you a unique list of all the values
#' that appear in all of them.
#'
#'
#' @param
#' ...   as many vectors as you want
#'
#'
#' @return
#' A vector of values with no duplicates.
#' Each value in the return vector appeared in all input vectors.
#'
#' @examples
#' v1 <- 1:5
#' v2 <- 2:10
#' v3 <- c("1", "2", "3")
#' intersection(v1, v2)
#' intersection(v2, v3)
#' intersection(v1, v2, v3)
#'
#' @export
intersection <- function(...) {
  V <- list(...)
  if (length(V) == 0) return(logical())
  out <- unique(V[[1]])
  for (i in 2:length(V)) {
    out <- out[out %in% V[[i]]]
  }
  return(out)
}



