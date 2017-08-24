# Author: Adam L. Rich
# Date:   September 21, 2012
# Description:
#
#   Experimenting with the clipboard
#

normal <- iris
big <- matrix(
  rep(1, times = 1e5),
  ncol = 20
)



copy.table <- function(obj) {
  
  obj.df <- as.data.frame(obj)
  size <- object.size(obj.df)
  
  
  # Add two bytes for each additional character
  #   Quotes for all non-numeric
  #   Tabs between columns
  # Or, just approximate by three characters per column
  # Each byte is represented by approx four bytes in unicode
  size.supremum <- as.integer((size * 4 + 12 * length(obj.df)) / 1024) + 1
  
  
  # Connect to the clipboard
  clipboard.desc <- paste('clipboard-', size.supremum, sep = '')
  f <- file(description = clipboard.desc, open = 'w')
  on.exit(close(f))
  
  
  # Write
  write.table(obj, f, row.names = FALSE, sep = '\t')
}

l <- data.frame(
  l1 = rep(c(TRUE, FALSE), each = 4, length.out = 8),
  l2 = rep(c(TRUE, FALSE), each = 2, length.out = 8),
  l3 = rep(c(TRUE, FALSE), each = 1, length.out = 8)
)

l$l4 <-  l$l1 &  l$l2  | l$l3
l$l5 <- (l$l1 &  l$l2) | l$l3   # This is the correct order of operations
l$l6 <-  l$l1 & (l$l2  | l$l3)

TRUE && TRUE || TRUE
TRUE && TRUE || TRUE
TRUE && TRUE || TRUE
TRUE && TRUE || TRUE
TRUE && TRUE || TRUE
TRUE && TRUE || TRUE