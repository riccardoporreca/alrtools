# Author: Adam L. Rich
# Date:   November 13, 2015
# Description:
#
#   package:ChainLadder
#



require(magrittr)
require(ChainLadder)




packageDescription('ChainLadder')


ls('package:ChainLadder')



x <- ls('package:ChainLadder')[34]

a <- sapply(ls('package:ChainLadder'), function(x) {
  get(x) %>% class %>% as.vector %>% paste(collapse = ' ')
})
b <- data.frame(
  obj = names(a),
  class = as.vector(a)
)

b <- b[order(b[, 2]), ]


#' Functions in ChainLadder
#' 
#'   as.triangle
#'   ata
#'   BootChainLadder
#'   CDR
#'   chainladder
#'   ClarkCapeCod
#'   ClarkLDF
#'   CLFMdelta
#'   cum2incr
#'   getLatestCumulative
#'   glmReserve
#'   incr2cum
#'   Join2Fits
#'   JoinFitMse
#'   LRfunction
#'   MackChainLadder
#'   MultiChainLadder2
#'   MunichChainLadder
#'   PaidIncurredChain
#'   Table64
#'   Table65
#'   Table68
#'   tweedieReserve
#'   

