#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   December 5, 2014
#   Description:
#  
#     In the Beginning
#  
#   Copyright Notice:
#     THE FOLLOWING APPLIES TO THIS ENTIRE FILE
#       AND/OR ANY PORTION THEREOF
#     COPYRIGHT ADAM L. RICH, 2014
#     THIS HAS BEEN DISTRIBUTED AS PART OF THE 2014 CAS LIMITED ATTENDANCE SEMINAR: INTRO TO R
#     YOU CAN USE CODE HERE IN YOUR OWN WORK 
#       PROVIDED YOU DO NOT TAKE CREDIT FOR IT
#     YOU AGREE TO NOT POST THIS IN ANY WAY ON THE INTERNET
#     YOU CAN DISTRIBUTE TO THOSE YOU TRUST TO FOLLOW THESE RULES
#     THIS MESSAGE MUST STAY WITH THIS CODE
#     



# Some notes about style...
#
#   At the beginning of every R script, 
#   you should put the following:
#
#     1. A flowerbox
#     2. A setwd() command
#     3. A list of the packages you are using
#
#   It is also good to write a lot of comments,
#   *especially* when you are in a hurry!



# SETUP
# Setting the working directory allows you to keep paths short
setwd('P:/Desktop/LAS Orlando/')
library(MASS) # Modern Applied Statistics in S
              # We won't actually use this here... it is just an example




# Find out what the working directory is with getwd()
getwd()




# Strings in R are C-style, 
#   meaning that they use escape characters 
#   to mark special characters like
#
#     \a – Bell (beep)
#     \b – Backspace
#     \f – Formfeed
#     \n – New line
#     \r – Carriage return
#     \t – Horizontal tab
#     \\ – Backslash
#     \' – Single quotation mark
#     \" – Double quotation mark
#     \ooo – Octal representation
#     \xdd – Hexadecimal representaion
#
# "\a" is my favorite, but doesn't work in RStudio
message("\a")
# Strings can be quoted with single or double quotes
# Question: is there a difference?




# R also has it's roots in Unix/Linux
#   Paths in Windows can be written more than one way
setwd('P:/Desktop/R Workshop')
getwd()
setwd('P:\\Desktop\\R Workshop')
getwd()

# If you are using a \\server\share style path, 
# you *must* start with '\\\\', '//' will not work!
#
#   setwd('\\\\server/share')
#




# R is really awesome at a lot of things
# In fact, you might be able to do everything without ever leaving RStudio
#
# You can do a lot directly in R
#   Like getting the contents of a directory
#   
dir('.')
dir(getwd())

# Running command line stuff
shell('explorer .', wait = FALSE)
shell('start iexplore "http://google.com"', wait = FALSE)
shell('notepad', wait = FALSE)
shell('start excel', wait = FALSE)

# Get help on R functions
?help
?lm
??formula

# Make breakfast
make.me.breakfast(when = now)   # TODO find out why this doesn't work...




# Finding your way around
#
# See everything that you have available to you
ls()
a <- 1
ls()
search()
ls(pos = 2)
ls(.GlobalEnv)
ls(pos = 2)
ls('package:base')
rm(a)
# To remove everything
# Don't run this unless you know what you are doing!
#
#   rm(list = ls(all = TRUE))
#



# R is case sensitive
# This is very important with TRUE and FALSE
# True, False, true, false, t, f, all do not work
# However, T and F usually do

# Valid names
#   underscores are allowed
#   dots are allowed (even at the beginning)
#   stick to alpha numerics, dots and underscores

# By this point, you have probably already
# seen what happens when you enter a functions
# and leave off the parantheses
ls
str
# You can actually use this to your advantage . . . 
# When writing your own functions it is great to have 
# a lot of examples to illustrate good coding




# Data types in R
#   VECTOR
#
#   Vectors are the building blocks in R
#   It is the most basic data type
#   The most common data type, data frames, 
#     are composed of vectors
#
my.vector <- c(1, 2, 3, 4, 5, 6, 7, 8)

class(my.vector)
str(my.vector)
dput(my.vector)
is.vector(my.vector)

my.scalar <- 100

class(my.scalar)
str(my.scalar)
dput(my.scalar)
is.vector(my.scalar)

length(my.vector)

# Stuff to cover on vectors
#   c()
#   length()
#   Arithmetic operations
#   indexing
#   n:m
#   seq()
#   rep()
#   Recycling rule
#   L suffix
#   min(), max(), pmin(), pmax()
#   names()
#   unique()
#   ifelse()
#   paste()
#   cut()
#   which()
#   which.min()
#   which.max()
#   match()



# Indexing vectors
#   x[n]                               Get the nth element of x
#   x[c(T, F)]                         Get every element with an odd index
#   x[-n]                              Get x omiting the nth element
#   x[1:n]                             Get elements 1 through n
#   x[-(1:n)]                          Omit elements 1 through n
#   x[c(1, 4, 2)]                      Get the 1st, 4th and 2nd elements, in that order
#   x["name"]                          Get the element named "name"
#   x[x > 3]                           Get all elements greater than 3
#   x[x > x[3]]                        Get all elements greater than the 3rd element
#   x[x > 3 & x < 5]                   Get all elements greater than 3 and less than 5
#   x[y > 5]                           Get the elements in x that aer in the same position
#                                        as elements in y if the elements in y are > 5
#                                        This can be extremely helpful . . . 
#   x[x %in% c('a', 'and', 'the')]     Get all elements in the specified setwd
# What is special about the last one?



# Indexing x does not change x!
# You have to store the result somewhere
# This is true throughout R
#   and is a key feature of its internal architecture



# A vector example
LOBs <- c('Auto', 'Homeowners', 'General Liability', 'Workers Compensation')
sample(LOBs, size = 100, replace = FALSE)
sample(LOBs, size = 100, replace = TRUE)

LOBs.sample <- sample(
  LOBs, 
  size = 1e6, 
  replace = TRUE, 
  prob = c(4, 1, 2, 1)
)

table(LOBs.sample)



# VECTOR EXERCISE
#   
#   Create a vector of the odd multiples of five (less than 101)
#   in a random order
#




# Data types in R
#   ARRAY and MATRIX
#
#   Arrays are multidimensional vectors
#   Matrices are two-dimensional arrays
#   
#   They can be any dimension
#   But, they can only have one mode
#   
#   Create arrays by setting the dimension atribute of a vector

# Creating a 10 by 10 matrix
a <- 1:100
dim(a) <- c(10, 10)
dim(a) <- c(2, 5, 10)
# REVIEW
#   How can you see the structure of this new matrix?


# Other ways to create a matrix
matrix(nrow = 2, ncol = 10)
matrix(0, nrow = 2, ncol = 10)


# Some functions that are specific to matrices
#   t()
#   diag()
#   %*%
#   solve()
#   rowSums()
#   colSums()




# Data types in R
#   LIST 
# 
#   Lists are generic bags of stuff
#   They are like vectors, but where each element can be anything
#   
#   Many functions use lists to return more than one value
#   Some R users keep their data environment clean 
#     by using lists to organize related stuff
a <- list()
a$Data <- rnorm(1000)
a$mean <- mean(a$Data)
a$sum  <- function(x) sum(x, na.rm = TRUE)
a$misc <- list()




# Data types in R
#   DATA FRAMES
#
#   A data frame is a list
#   Each member of the list is a vector
#   All the vectors have the same length
#
#   Data frames are a lot like database tables
#   You will spend most of your time working with 
#     data frames and the vectors they comprise
#
#   VERY IMPORTANT
#     Assign using "<-"
#     Attach data to function variables using "="
#

my.df <- data.frame(
  key = 1:26,
  value = letters
)

# DO NOT DO THIS
my.df <- data.frame(
  1:26,
  letters
)


# Some other functions for data frames
#   head()
#   tail()
#   length() Not the same as for vectors!
#   nrow()
#   rbind()
#   cbind()
#   merge()
#   attach()
#   detach()
#   with()
#   within()
#   summary()


# Indexing data frames
#   x[i, j]                       Element at row i, column j
#   x[i, ]                        Row i
#   x[, j]                        Column j
#   x[, c(1, 3)]                  Columns 1 and 3
#   x['name', ]                   Row named "name"
#   x[, 'name']                   Column named "name"
#   x[, c('age', 'gender')]       Data frame with two columns from original
#   x[, 'age', drop = FALSE]      Data frame with one column from original
#   x[['name']]                   Column named "name"
#   x$name                        Column named "name"



# Review Question
#   What is the difference between a list and a vector?
#   How would you create a scalar in R?



# Data types in R
#   FACTORS
#
#   A special kind of vector
#   Used for categorical variables
#   Can be annoying sometimes
#   Hint: be careful with ifelse
#   When in doubt write a test or convert to character
#
LOBs.sample <- sample(
  c('Auto', 'Homeowners', 'General Liabilty', 'Workers Compensation'), 
  size = 1e3, 
  replace = TRUE, 
  prob = c(4, 1, 2, 1)
)
class(LOBs.sample)

LOBs.factor <- as.factor(LOBs.sample)
class(LOBs.factor)
str(LOBs.sample)



# Control Stuctures
#   for
#   while
#   if...else
for (x in 1:10) {
  print(x)
}

x <- 0
while(x < 10) {
  print(x)
  x <- x + 1
}


# Important to remember that "if" only checks the first element...
# Maybe you want to use ifelse, all, any?
x <- 'a'
if (x == 'a') {
  print('x is a')
}


x <- letters
if (x == 'a') {
  print('x is a')
}




# More "advanced" stuff
#
#   Built-in Statistical distributions
#     ?Distributions
#
#   Prefixes for functions
#     d*    Density
#     p*    Cumulative distribution 
#     q*    Quantile
#     r*    Random
#     
#
#
#   *apply family of functions
#     apply()       Apply functions over array margins
#     by()          Apply a function to a dataframe split by factors
#     tapply()      Apply a function over a ragged array
#     sapply()      Apply a function over a list or vector, simple version?
#     lapply()      Apply a function over a list or vector
#     mapply()      Apply a function to multiple list or vector arguments
#     eapply()      Apply a function over an environment
#     rapply()      Apply a function recursively over a list
#
#   Data Manipulation
#     aggregate()
#     reshape()
#     merge()
#
#   Regular Expressions   
#     grep()
#     grepl()
#     sub()
#     gsub()
#     etc.
#
#   Classes
#     apropos()
#     methods()
#     unclass()
#     attr()
#     attributes()
#


# Some important notes about calling functions
#   1.  Functions have default arguments
#       It's OK to rely on defaults, 
#       just be sure you know what they are
#       Sometimes it is better to just be explicit
rnorm(10)
head(rnorm)
rnorm(10, mean = 0, sd = 1)

#   2.  In function calls with more than one argument,
#       go ahead and be explicit about what you are doing
#       For example, what is this doing?
sample(10, 5)
sample(5, 10, TRUE)

sample(x = 10, size = 5, replace = TRUE)

#   3.  There are a couple of ways to learn more
#       about the default function signature
sample
head(sample)
?sample

#   4.  Never ever use "<-" in calling a function
#       What happens when we run this?
sample(x <- 10, size <- 5, replace <- TRUE)


