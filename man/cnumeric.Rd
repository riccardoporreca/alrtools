% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all.R
\name{cnumeric}
\alias{cnumeric}
\title{Convert text to numbers}
\usage{
cnumeric(v)
}
\arguments{
\item{v}{a vector}
}
\value{
A vector of numerics. Returns \code{NA}
when conversion is not possible after application
of the rules above.
}
\description{
This function takes characters that look like numbers
and converts them to numbers.
Its name is based on the VBA function \code{CNumeric}.
It does some pre-processing before calling
\code{\link[base]{as.numeric}}.
\enumerate{
  \item converts factors
  \item removes all whitespace
  \item converts wrapping "()" to negatives
  \item removes commas
  \item converts percentages to decimals
  \item calls \code{\link[base]{as.numeric}}
}
}
\examples{
# Commas are removed
cnumeric(c('1,000,000', '2,000.03'))

# But, we don't check to make sure that
# commas are in the right place first
cnumeric(c('1,0,0', '2,0000.03'))

# Accounting-style negatives
cnumeric(c('(1,000.92)', '(4)'))

# Percents are converted
cnumeric(c('28.3\%', '-1.3\%', '(15\%)'))

# If scientific notation is present, R knows what to do
cnumeric(c('3e7', '5e-1'))

# TODO it doesn't deal with currencies yet
cnumeric(c('USD 0.10', '$14.34'))

}
