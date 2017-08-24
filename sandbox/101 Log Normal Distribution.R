# Author: Adam L. Rich
# Date:   January 8, 2012
# Description:
#
#   Playing around with the lognormal distribution
#

require(actuar)

# From the stats package
#
#   dlnorm    Density             (x >= 0, y >= 0)
#   plnorm    Distribution        (x >= 0, 0 <= y <= 1)
#   qlnorm    Quantile            Inverse of plnorm
#                                 (0 <= x <= 1, y >= 0)
#   rlnorm    Random generation   


# From the actuar package
#
#   mlnorm    Raw moments
#   levlnorm  Limited expected value (lev)
#


# Check mlnorm
mlnorm.alr <- function(order, meanlog = 0, sdlog = 1) {
  exp(order * meanlog + 0.5 * order * order * sdlog * sdlog)
}
mlnorm.alr(1:5) - mlnorm(1:5)


# Check levlnorm
levlnorm.alr <- function(limit, meanlog = 0, sdlog = 1, order = 1) {
  
  exp(order * meanlog + 0.5 * order * order * sdlog * sdlog) *
    pnorm((log(limit) - meanlog - order * sdlog * sdlog) / sdlog) +
    (limit ^ order) * (1 - pnorm((log(limit) - meanlog) / sdlog))

}
levlnorm.alr(1:10) - levlnorm(1:10)


