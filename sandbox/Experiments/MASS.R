# Author: Adam L. Rich
# Date:   June 19, 2012
# Description:
#
#   Notes while reading MASS
#

library(MASS)

x <- rnorm(1000)
y <- rnorm(1000)

truehist(c(x, y + 3), nbins = 25)

dd <- kde2d(x, y)

contour(dd)

image(dd)

x <- seq(1, 20, .5)
x

w <- 1 + x/2
y <- x + w*rnorm(x)

dum <- data.frame(x, y, w)
dum

rm(x, y, w)

fm <- lm(y ~ x, data = dum)
summary(fm)

fm1 <- lm(y ~ x, data = dum, weight = 1/w^2)
summary(fm1)

lrf <- loess(y ~ x, dum)

attach(dum)

plot(x, y)