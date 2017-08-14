# Author: Adam L. Rich
# Date:   July 21, 2015
# Description:
#
#   Extract exercises from Hadley's book
#



require(knitr)
setwd('P:/Desktop/Advanced R/adv-r-master/')
rmd_out   <- '../Exercises-Output.Rmd'
html_out  <- '../Exercises-Output.html'
r_out     <- '../Exercises-Output.R'
pdf_out   <- '../Exercises-Output.pdf'



extract_exercises <- function(f_in) {

  con_input <- file(f_in, 'r')
  on.exit(close(con_input))

  
  lines <- readLines(con_input)
  n <- 1:length(lines)
  n_exercises <- n[grepl('^##.*Exercises.*', lines)]
  n_headers <- n[grepl('^##.*', lines)]
  n_stop <- c(n_headers[-1], length(lines) + 1)
  n_index <- n_headers %in% n_exercises
  pairs <- data.frame(
    start = n_headers[n_index], 
    stop = n_stop[n_index] - 1
  )

  out <- list()
  out$file <- f_in

  if (nrow(pairs) == 0) {
    out$has_exercises <- FALSE
    out$RmdLines <- character()
  } else {
    out$has_exercises <- TRUE
    u <- unlist(apply(pairs, 1, function(r) r[1]:r[2]))
    out$RmdLines <- lines[u]
  }  

  out

}




output_exercises <- function(data, con_output) {
  writeLines(c(
    rep('', 3),
    paste0('#### Exercises from ', data$file),
    rep('', 3),
    data$RmdLines), con_output)
}






files <- dir(recursive = T, pattern = '.*\\.Rmd*', ignore.case = T)
f_out <- '../Exercises-Output.Rmd'
if (file.exists(f_out)) file.remove(f_out)
all_lines <- lapply(files, extract_exercises)




# Which ones have exercises?
files_with_exercises <- 
  files[sapply(all_lines, function(i) i$has_exercises)]




# List expected Rmd files with exercises in order that they appear in the book
expected <- c('Data-structures.rmd', 
              'Subsetting.rmd', 
              'Functions.rmd',
              'OO-essentials.rmd',
              'Environments.rmd',
              'Exceptions-Debugging.rmd',
              'Functional-programming.rmd',
              'Functionals.rmd',
              'Function-operators.rmd',
              'Computing-on-the-language.rmd',
              'extras/local.Rmd',
              'Expressions.rmd',
              'dsl.rmd',
              'Performance.rmd',
              'Profiling.rmd',
              'memory.rmd',
              'Rcpp.rmd')


stopifnot(
  all(sort(expected) == sort(files_with_exercises)))



# Create Rmd file
if (file.exists(rmd_out)) remove(rmd_out)
conn <- file(rmd_out, 'w')
all_lines <- lapply(expected, extract_exercises)
sapply(all_lines, output_exercises, con_output = conn)
close(conn)




# Create other output
purl(rmd_out, output = r_out, documentation = 2)
knit2html(rmd_out, output = html_out)
# knit2pdf(rmd_out, output = pdf_out)






