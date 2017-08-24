#' # HOW to create slides
#' 
#' 
#' to save this to md 
#' 
#'  - run spin('spin-sjb.r') from console, after ensuring in same directrory
#' 
#' 
#' to create slidy html 
#' 
#'  - go to windows powershell
#'  - switch directory to where data is 
#'  - type pandoc -s spin-sjb.md -t slidy -o spin-sjb.html  
#' (use -i for incremental slides, or >- in code)


#+ setup, include=FALSE
library(knitr)
opts_chunk$set(tidy=FALSE)


#' # The report begins here
#' 

#' Boring examples as usual
set.seed(123)
x = rnorm(5)
mean(x)

#' Now we continue writing the report. We can draw plots as well.
par(mar = c(4, 4, .1, .1))
plot(x)

var(x)
quantile(x)

#+ test-chisq5
sum(x^2) # chi-square distribution with df 5

#' # slide 2 Hello world
#' 
#'> - just a test
#'> - try another chart
#' 

#' # Slide 3: just one chart
#' 
plot(x^2)


