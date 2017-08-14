# Author: Adam L. Rich
# Date:   December 9, 2014
# Description:
#
#   Get all functions from a namespace
#




# Setup
setwd('c:/home/Blog/actuar/')



# Parameters
stem <- getwd()
package <- 'actuar'







ns <- getNamespace(package)




# iterator
i <- 10

f <- NULL
f <- eval(parse(text = paste0('ns$', ls(ns)[i])))
name <- ls(ns)[i]
file <- file.path(stem, paste0(name, '.R'))


txt <- capture.output(f)
txt[1] <- paste(name, '<-', txt[1])
txt <- txt[!grepl(pattern = '^[<]environment.*$', x = txt)]
txt <- txt[!grepl(pattern = '^[<]bytecode.*$', x = txt)]

txt <- c(
  '# Author: Automated',
  '#         Annotations by Adam L. Rich',
  paste('# Date:  ', date()),
  '# Description:',
  '#',
  '#   Annotated Function',
  paste0('#     ', package, '::', name),
  '#',
  '',
  '',
  txt,
  ''
)


cat(txt, file = file, sep = '\n')


