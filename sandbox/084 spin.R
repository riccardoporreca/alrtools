# Author: Yihui Xie
# Notes:  Adam L. Rich
# Date:   July 4, 2013
# Description:
#
#   Spin goat's hair into wool
#   Converts R script to Rmd file
#
# Arguments:
#
#   hair      the path to the R script to spin
#   knit      compile (to md file) after conversion?
#   report    create HTML report?
#   text      instead of passing a path, pass the R code
#   format    five possibilities, default is Rmd
#   doc       regex for matching doc (report text) lines
#


# For testing
#
#   hair    <- 'Automated Reporting.R'
#   knit    <- TRUE
#   report  <- TRUE
#   text    <- NULL
#   format  <- 'Rmd'
#   doc     <- "^#+'[ ]?"
#   i       <- 2
#   


.fmt.pat = list(
  rmd = c('```{r ', '}', '```'), 
  rnw = c('<<', '>>=', '@'),
  rhtml = c('<!--begin.rcode ', '', 'end.rcode-->'),
  rtex = c('% begin.rcode ', '', '% end.rcode'), 
  rrst = c('.. {r ', '}', '.. ..')
)


strip_white <- function(x) {x}


spin <- function (
    hair, knit = TRUE,  report = TRUE, text = NULL,
    format = c("Rmd", "Rnw", "Rhtml", "Rtex", "Rrst"), 
    doc = "^#+'[ ]?") {
  
  # Get the format desired
  # Allow partial matching
  # If NULL, use "Rmd"
  format = match.arg(format)
  
  
  # If text is missing,
  # get the R code
  # Assigned to x
  x = if (nosrc <- is.null(text)) 
    readLines(hair, warn = FALSE)
  else split_lines(text)
  
  
  # Break x into "chuncks"
  # str_detect returns 
  #   TRUE for each row that matches the regex doc
  #   FALSE otherwise
  # rle returns an object composed of two vectors
  #   lengths     how many lines is each chunk?
  #   values      Will alternate between TRUE and FALSE
  r = rle(str_detect(x, doc))
  
  
  # How many chunks are there in total?
  # Create an empty list, one slot for each chunk
  n = length(r$lengths)
  txt = vector("list", n)
  
  
  # Create a vector that gives starting and ending row numbers for each chunk
  # The ith element of idx gives the ending row for the (i-1)th chunk
  # For example, if 
  #   chunk 1 ends on line 3
  #   chunk 2 ends on line 7
  #   chunk 3 ends on line 8
  # idx would be
  #   c(0, 3, 7, 8)
  idx = c(0L, cumsum(r$lengths))
  
  
  # .fmt.pat is a list of vectors
  # Each possible format has a slot in the list
  # Each vector in the list (one for each format
  p = .fmt.pat[[tolower(format)]]
  
  
  # Just make sure that the strings are all ready for regex
  p1 = str_replace(
    str_c("^", p[1L], ".*", p[2L], "$"),  # string
    "\\{",                                # pattern
    "\\\\{"                               # replacement
  )
  
  
  # Loop through each chunk, 1:n
  for (i in seq_len(n)) {
    
    
    # Get the rows for this code chunk only
    block = x[seq(idx[i] + 1L, idx[i + 1])]
    
    
    # txt is the output text
    # Built from the groupnd up
    txt[[i]] = if (r$value[i]) {  
      # If this chunk is documentation, 
      # remove the doc regex flag
      str_replace(block, doc, "")
    
      
    } else {  # In a code chunk
      # Remove any whitespace
      block = strip_white(block)
      
      
      # Remember 0 is FALSE, everything else is TRUE
      # If we have no rows, resume next i
      if (!length(block)) 
        next
      
      
      # If we have put in any chunk options, 
      # fix those strings
      if (any(opt <- str_detect(block, "^#+(\\+|-| @knitr)"))) {
        block[opt] = str_c(
          p[1L], str_replace(block[opt], "^#+(\\+|-| @knitr)\\s*", ""), p[2L]
        )
      }
      
      
      # If this block didn't have any block options, 
      # add the r code block begin tag
      if (!str_detect(block[1L], p1)) {
        block = c(str_c(p[1L], p[2L]), block)
      }
      
      
      # Concatenate on the block ending tag
      c("", block, p[3L], "")
    }
  }
  
  
  # Flatten the list, txt
  txt = unlist(txt)
  
  
  # Add beginning and ending LaTeX if needed
  if (report && format %in% c("Rnw", "Rtex") && 
        !str_detect(txt, "^\\s*\\\\documentclass")) {
    txt = c("\\documentclass{article}", "\\begin{document}", txt, "\\end{document}")
  }
  
  
  # Get the name of the output file
  if (nosrc) {
    outsrc = sub_ext(hair, format)
    cat(txt, file = outsrc, sep = "\n")
    txt = NULL
  }
  else outsrc = NULL
  
  
  # If we are not knitting, return based on format
  if (!knit) 
    return(txt %n% outsrc)
  if (report) {
    if (format == "Rmd") 
      return(knit2html(outsrc, text = txt))
    if (!nosrc && (format %in% c("Rnw", "Rtex"))) 
      return(knit2pdf(outsrc))
  }
  
  
  # Otherwise, knit!
  knit(outsrc, text = txt)
}

