# Author: Adam L. Rich
# Date:   April 28, 2017
# Description:
#
#   Some general stuff
#   Eventually move to BZLYUtil and alrtools
#



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
