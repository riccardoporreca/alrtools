# Author: Adam L. Rich
# Date:   October 9, 2014
# Description:
#
#   .libPaths
#   Annotated function
#


.libPaths() <- function (new) {
  if (!missing(new)) {
    new <- Sys.glob(path.expand(new))
    paths <- unique(normalizePath(c(new, .Library.site, .Library), 
                                  "/"))
    .lib.loc <<- paths[file.info(paths)$isdir %in% TRUE]
  }
  else .lib.loc
}



require(compile)


f <- function(x) x+1
fc <- cmpfun(f)
fc(2)
disassemble(fc)



apropos('.libPaths')