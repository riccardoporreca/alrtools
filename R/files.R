normalize_path <- function(p){
  p <- base::normalizePath(p, winslash = '/', mustWork = TRUE)
  if(grepl('(/|\\\\)$', p)) {
    p <- paste(p, '.', sep = '')
  }
  p
}

