% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all.R
\name{curry_v1}
\alias{curry_v1}
\title{RETIRED VERSION Pre-evaluate arguments of a function}
\usage{
curry_v1(FUN, ...)
}
\arguments{
\item{FUN}{the function we are currying}

\item{...}{the arguments you want to pre-evaluate}
}
\value{
the new curried function
}
\description{
To curry a function means to pre-evaluate some of the arguments
So, if you have a function
  \code{sum <- function(a, b) {a + b}}

And you always want \code{b} to be 1, you could define
  \code{add.one <- curry(sum, b = 1)}

Which is the same as
  \code{function(a) {a + 1}}
}
\examples{
sum <- function(a, b) {a + b}
add.one <- curry(sum, b = 1)
function(a) {a + 1}
}
