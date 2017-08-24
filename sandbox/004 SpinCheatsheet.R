#' A Title
#' ====================


#+ setup, include = FALSE
# The #+ tag is used for chunk options
# include = FALSE means this chunk doesn't show up
require(knitr)


# The following tells knitr to NOT reformat our R code
opts_chunk$set(tidy = FALSE)


#' # A Header/New Slide
#' 
#' All regular text must be preceeded by #'
#' R code and comments can just be typed as usual (with optional chunk directives)
#' 
#' Include images like this
#' ![](./Image.jpg)
#' 
#' The following creates bullet points (with pauses for presentations)
#' >- Point 1
#' >- Point 2
#' 
#' 
#' 
#' # Another Header/New Slide
#' 
#' - The dash is for bullet points
#' 
#' Do links this way
#' - Find a document [here][a-doc]
#' 
#'   [a-doc]: ./Automated_Reporting.R
#'   


#' # `CodeFormattedText`
#' 
#' This is explanatory text for the code below
#' It will be shown as is and executed
#' The output will also be shown
nums <- 1:100
nums


#' # A Table
#' 
#' Column1      | Column2
#' -------------|------------                      
#' Cell1        | Cell3                            
#' Cell2        | Cell4                
#' 


#' # Combine Explanatory text with chunk options
#' 
#+ chunk-name
letters


#+ include = FALSE
# Pandoc markdown
# http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
