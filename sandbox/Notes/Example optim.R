# Author: Adam L. Rich
# Date:   February 1, 2013
# Description:
#
#   Example of optim function
#

x <- 1:100
m <- 0.1
b <- -10
e <- rnorm(n = 100, mean = 0, sd = 1)
y <- m * x + b + e

plot(y ~ x)

fit.ls <- lm(y ~ x)

summary(fit.ls)

str(fit.ls)
ls(fit.ls)

fit.ls$fitted.values

sq.error <- function(p) {
  
  m <- p[1]
  b <- p[2]
  
  y.fitted <- m * x + b
  return(sum((y.fitted - y) ^ 2))
  
}


fit.optim <- optim(c(.5, 5), sq.error)

fit.ls$coefficients
fit.optim$par