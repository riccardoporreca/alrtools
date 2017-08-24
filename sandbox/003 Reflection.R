# Author: Adam L. Rich
# Date:   October 9, 2014
# Description:
#
#   Examining R
#



# You can see all loaded packages using search()
# The function returns a character vector
# search is an internal function, so you cannot see in R what it is doing
#   but if you really wanted to know, you could go to the source code...
# TODO: where is this defined in the source code?
search()
str(search())

search
str(search)



# You can see all installed packages using installed.packages
# This returns a character matrix
a <- installed.packages()
a
head(a)
str(a)
class(a)
mode(a)



# Look at installed.packages code
# You actually can learn a lot about R from reading the code of functions like this
# See annotated function for more info
installed.packages







# Maybe this is easier to read by turning into a data.frame?
a <- as.data.frame(a)
a
head(a)
str(a)
class(a)
mode(a)



