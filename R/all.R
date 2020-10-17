# Author: Adam L. Rich
# Date:   October 12, 2020
# Description:
#
#   A bunch of convenience functions
#
#   [May 14, 2020 ALR]
#   copy.table and paste.table removed, use clipr instead
#



#' Euler's number
#' @description 2.718282
"e"
# Defined in data-raw/build-templates.R




#' A factor-safe version of \code{base::ifelse}
#'
#' @description
#' The \code{\link[base]{ifelse}} function does weird
#' things when you pass it factors.
#' This version doesn't.
#'
#' @param	test   a vector of logicals
#' @param	yes    values to return if test is TRUE
#' @param	no     values to return if test if FALSE
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
#' base::ifelse(c(TRUE, FALSE), yesf, nof)
#' alrtools::ifelse(c(TRUE, FALSE), yesf, nof)
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
#' @param	...    vectors
#' @param	na.rm  a logical indicating whether missing values should be removed
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
#' @param	d    a vector of dates
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
#' @param	d    a vector of dates
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
#' @param	d    a vector of dates
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
#' @param	d    a vector of dates
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
#' @param	string    a vector of strings
#' @param	n         a vector of integers (will be recycled, if needed)
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
#' @param	string    a vector of strings
#' @param	n         a vector of integers (will be recycled, if needed)
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
#' @param	string    a vector of strings
#' @param	start     a vector of starting points (will be recycled, if needed)
#' @param	length    a vector of lengths (will be recycled, if needed)
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
#' @param	x    a vector of strings
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
#' For use in package development.
#' Open a file in tests/testthat/ for the object called "test"
#' If the file doesn't already exist, create it.
#' If it already exists, open it for editing.
#'
#' @param	test     the object you want to write a test for
#' @param	pkg      to pass for the knitr environment.  Defaults to current working directory
#' @param	engine   if you are using something other than testthat,
#'                   you can save this test to "tests/ENGINE/"
#'
#' @return
#' Returns NULL invisibly.
#' Side-effect: opens the test file in the editor.
#'
#' @examples
#' # open_test(lm)
#'
#' @export
open_test <- function(test, pkg = basename(getwd()), engine = 'testthat') {

  test <- as.character(match.call(expand.dots = FALSE)$test)
  test_file <- paste0('./tests/', engine, '/test_', test, '.R')
  test_dir <- dirname(test_file)

  if (file.exists(test_file)) {
    utils::file.edit(test_file)
    return(invisible(NULL))
  }

  if (!dir.exists(test_dir))
    dir.create(test_dir, recursive = TRUE)

  knit_env <- list(f = test, pkg = pkg)
  RCode <- knitr::knit(text = test_template, envir = knit_env)
  writeChar(RCode, test_file)

  utils::file.edit(test_file)
  return(invisible(NULL))
}



#' Load Rdata to an environment
#'
#' @description
#' Instead of loading RData files to the global environment
#' load them to a new env.
#' \code{base::load} already has a way to do this,
#' but this is a convenience function that creates
#' a new environment by default.
#'
#' @param	RData    path to the RData file you want to load
#' @param	env      the environment you want to load to.  Defaults to a new environment.
#'
#' @return
#' Returns the environment you are loading to.
#'
#' @examples
#' # data_env <- load_env('file1.RData')
#' # load_env('file2.RData', data_env)
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
#' Instead of sourcing R code in the global environment
#' where it might create new objects or overwrite existing ones,
#' create a new environment (or use an existing one)
#' and source the code there!
#'
#' @param	RScript  path to the R file you want to run
#' @param	env      the environment you want to run in.  Defaults to a new environment.
#'
#' @return
#' Returns the environment where the processing occurred.
#'
#' @examples
#' # code_env <- source_env('file1.R')
#' # source_env('file2.R', code_env)
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
#' Run a function in an environment other than Global.
#' One way to use this is to take other people's code
#' that uses global objects and create wrapper functions
#' where those objects can be passed in.
#'
#' @param	f       the function
#' @param	env     the environment you want to run in
#' @param	...     parameters to the function
#'
#' @return
#' Returns whatever \code{f} is supposed to return.
#'
#' @examples
#' # A function I get from a colleagues code
#' # Imagine it is something you cannot re-write easily
#' bad_func <- function(n) {
#'   set.seed(0)
#'   return(rnorm(n, mean = mu, sd = mu / 2))
#' }
#'
#' mu <- 5
#' bad_func(3)
#'
#' good_func <- function(n, mu) {
#'   temp_env <- new.env()
#'   temp_env$mu <- mu
#'   execute_in(bad_func, temp_env, n = n)
#' }
#'
#' good_func(3, 5)
#' good_func(3, 10)
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
#' @param	f       the function
#' @param	file    where to save it
#' @param	name    what name to give the object in the file
#'
#' @return
#' Invisibly returns the contents of the file that was written.
#' The file is a sourceable file.
#' You can use \code{name} to give the object a new name
#' in the file so if you source it, you will not have
#' conflicts.
#'
#' @examples
#' my_func <- function(a, b) {a - b * 2 + a^2}
#' temp_R <- tempfile(fileext = '.R')
#' kode <- write.function(my_func, file = temp_R)
#' print(kode)
#'
#' kode <- write.function(my_func, file = temp_R, name = "diff_name")
#' print(kode)
#'
#' print(readLines(temp_R))
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
#' Uses \code{\link{write.package}} to write
#' every function in a package to a specified directory.
#'
#' @param	pkg       the package of interest
#' @param	folder    the folder where to save the R files
#'
#' @return
#' Nothing
#'
#' @examples
#' # write.package('alrtools', 'some/folder/')
#'
#' @export
write.package <- function(pkg, folder) {
  dir.create(folder, showWarnings = FALSE)
  fs <- ls(getNamespace(pkg), all.names = TRUE)
  for (f in fs) {
    g <- get(f, envir = getNamespace(pkg))
    p <- paste0(folder, '/', f, '.R')
    write.function(g, p, f)
  }
}




#' Pre-evaluate arguments of a function
#'
#' @description
#' To curry a function means to pre-evaluate some of the arguments
#' So, if you have a function
#'   \code{sum <- function(a, b) {a + b}}
#'
#' And you always want \code{b} to be 1, you could define
#'   \code{add.one <- curry(sum, b = 1)}
#'
#' Which is the same as
#'   \code{function(a) {a + 1}}
#'
#'
#' @param	FUN        the function we are currying
#' @param	...        the arguments you want to pre-evaluate
#'
#' @return
#' the new curried function
#'
#' @examples
#' sum <- function(a, b) {a + b}
#' add.one <- curry(sum, b = 1)
#' function(a) {a + 1}
#'
#' @export
curry <- function(FUN, ...) {
  orig <- utils::tail(as.list(match.call()), -2)
  noms <- names(orig)
  ff <- formals(FUN)
  ff[noms] <- orig
  formals(FUN) <- ff
  return(FUN)
}




#' RETIRED VERSION Pre-evaluate arguments of a function
#'
#' @description
#' To curry a function means to pre-evaluate some of the arguments
#' So, if you have a function
#'   \code{sum <- function(a, b) {a + b}}
#'
#' And you always want \code{b} to be 1, you could define
#'   \code{add.one <- curry(sum, b = 1)}
#'
#' Which is the same as
#'   \code{function(a) {a + 1}}
#'
#'
#' @param	FUN        the function we are currying
#' @param	...        the arguments you want to pre-evaluate
#'
#' @return
#' the new curried function
#'
#' @examples
#' sum <- function(a, b) {a + b}
#' add.one <- curry(sum, b = 1)
#' function(a) {a + 1}
curry_v1 <- function(FUN, ...) {

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
#' Give \code{HFactory} an object or a name of an object
#' that has a \code{names} attribute
#' (Or a function that *returns* a named object).
#' \code{HFactory} will build you an easy-to-use function
#' that encapsulates searching the "header" of
#' the object.
#'
#' @param	name         the object or name of the object
#' @param	ignore.case  do you want your header search function to ignore case?
#'
#' @return
#' a function that takes one argument: \code{pattern}.
#' Give the return value a name, and you can call it like any other function.
#' It will use \code{grep} and \code{pattern} to find matching \code{names}.
#' The header-search function that is returned will catch name changes, too.
#'
#' @examples
#' my_iris <- datasets::iris
#' hi <- HFactory(my_iris)
#' hi('length')
#' hi('width')
#' my_iris$SpeciesPredicted <- 'TODO'
#' hi('species')
#'
#' @export
HFactory  <- function(name, ignore.case = TRUE){
  # TODO: there is something going on when using H functions
  # in a non-global environment
  is.function <- base::is.function(name)

  # Previously, name had to be a charater string representing the name of an object (how annoying!)
  # Because of the next line, it can now be an object!
  # Based on code of "rm" function
  name <- as.character(match.call(expand.dots = FALSE)$name)

  # If name is a function object, trail with "()"
  if(is.function) name <- paste0(name, '()')

  # Also, now it allows for a case-sensitive function
  if(ignore.case) ic = 'TRUE' else ic = 'FALSE'
  s <- paste0('function(pattern = \'\'){names(', name, ')[grep(pattern, names(', name, '), ignore.case = ',ic,')]}')
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
#' @param	v           a vector
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
#' \code{read.csv0} is to \code{read.csv} what
#' \code{paste0} is to \code{paste}.
#' More often that not, I see people using \code{read.csv}
#' with \code{stringsAsFactors = FALSE} (like you will
#' often see people calling \code{paste} with \code{sep = ''}).
#' This is a variant where \code{stringsAsFactors} is preset.
#' Relies on the \code{\link{curry}} function.
#'
#' @param	...     See parameters for \code{\link{read.csv}}
#'
#' @return
#' See help page for \code{\link{read.csv}}
#'
#' @examples
#' # See help page for \code{\link{read.csv}}
#'
#' @export
read.csv0 <- curry(utils::read.csv, stringsAsFactors = FALSE)






#' Excel-like \code{VLOOKUP} function
#'
#' @description
#' Works just like Excel's \code{VLOOKUP} function
#' with a few improvements.
#'
#' @param	lookup_value        a vector of values you want to match in the \code{table_array} column of interest
#' @param	table_array         the data you are "looking up" in
#' @param	col_index_number    the index of the column you want to return
#' @param	type                0 = exact match.  No other method is currently supported
#' @param	lookup_index        the column index of the column \code{lookup_value} is matched to
#'                              (In Excel, this is always "1")
#'
#' @return
#' Returns a vector of values with the same length as \code{lookup_value}.
#' If a value is not found, then NA is returned.
#' Unlike Excel, the column we are matching against cannot have duplicates.
#'
#' @examples
#' ref <- data.frame(
#'   state = c('UT', 'FL', 'NY', 'CA', 'SD'),
#'   category = c(1, 2, 3, 3, 1)
#' )
#'
#' vlookup('CA', ref, 1, 0)
#' vlookup('CA', ref, 2, 0)
#'
#' vlookup(c('CA', 'FL', 'KY'), ref, 2, 0)
#'
#' ref[2, 2] <- 4
#' vlookup('FL', ref, 2, 0)
#'
#'
#' @export
vlookup <- function(
    lookup_value, table_array, col_index_number, type = 0, lookup_index = 1) {
  lookup_index <- lookup_index[1]
  lookup_value <- tolower(lookup_value)
  levels <- tolower(table_array[, lookup_index])
  table_array[factor(lookup_value, levels = levels), col_index_number]
}






#' Doc writing: get LaTeX of a matrix
#'
#' @description
#' If you are working in RMarkdown and need to write a matrix in LaTeX
#' it can be a pain.  This function tries to make it easier.
#'
#' @param	M          a matrix-like object
#' @param	one_line   logical -- whether to return one-line or many
#' @param	print      logical -- print the LaTeX to the screen
#' @param	ret        logical -- return the text so it can be stored in a variable
#'
#' @return
#' If \code{ret = TRUE}, the LaTeX code to represent the matrix.
#' If \code{print = TRUE}, the code is printed to the screen using \code{message}.
#'
#' @examples
#' m <- datasets::iris[1:5, 1:4]
#' matrix2latex(m)
#' tex <- matrix2latex(m, print = FALSE)
#' print(tex)
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
#' @param	...   as many vectors as you want
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



