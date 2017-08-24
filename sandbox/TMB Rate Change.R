# Author: Adam L. Rich
# Date:   July 26, 2012
# Description:
#
#   TMB Rate Change Formula
#

require(ggplot2)

fees1 <- 100e6

fees2 <- seq(fees1 / 2, fees1 * 2, length.out = 1000)

exposure.change <- fees2 / fees1 - 1

fees1.ln <- 47461 * log(fees1 / 1e6) - 119474
fees2.ln <- 47461 * log(fees2 / 1e6) - 119474

price.change.true <- fees2.ln / fees1.ln - 1

price.change.impact <- pmax(pmin(.6, price.change.true / exposure.change), .2) * exposure.change + 1

df <- data.frame(
  x = c(fees2, fees2, fees2),
  y = c(.2 * exposure.change, price.change.true, .6 * exposure.change),
  color = factor(c(
    rep('20% EC', length(fees2)),
    rep('Price Change', length(fees2)), 
    rep('60% EC', length(fees2))
  ))
)

qplot(x, y, data = df, colour = color)

# fees1[1]
# fees2[1]
# exposure.change[1]
# price.change.true[1]
# price.change.impact[1]
