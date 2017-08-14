#'  Author: Adam L. Rich
#'  Date:   July 19, 2015
#'  Description:
#'
#'    Exercises from Advanced R
#'


#'  Exercises 2.1.3
#'    1.  What are the six types of atomic vector?
#'        How does a list differ from an atomic vector
#'        
#'          logical
#'          integer
#'          numeric / double
#'          complex
#'          character
#'          raw
#'          
as.logical(0:10)
as.integer(0:10)
as.complex(1:10)
as.character(1:10)
as.raw(1:10)
as.raw(letters)

charToRaw('To be or not to be')



#'    2.  What makes is.vector() and is.numeric() fundamentally different
#'        to is.list() and is.character()?
is.vector(1:10)
is.numeric(1:10)
is.vector(letters)
is.vector(list())
is.vector(structure(1:10, class = 'span'))

is.list(1:10)



#'    4.  Why do you need to use unlist() to convert a list to an atomic vector?



#'        Why doesn't as.vector() work?
#'    5.  Why is 1 == "1" true?  Why is -1 < FALSE true?
#'        Why is "one" < 2 false?
#'    6.  Why is the default missing value, NA, a logical vector?
#'        What is special about logical vectors?






