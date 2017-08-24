# Author: Adam L. Rich
# Date:   February 11, 2013
# Description:
#
#   To find out what is going on with sClaimCat Currency Codes
#

source('w:/sl/Actuarial/R-Software/Utilities/Prod/SLPseudoPackage.r')



sClaimCat.env <- LoadToEnvironment('W:/SL/Actuarial/ClaimCategorisation_ALR/Quarters/2012q4/2012q4 sClaimCat.Rdata')
sClaimCat <- sClaimCat.env$sClaimCat


hs <- HFactory(sClaimCat)


table(sClaimCat[, c('BBLL', 'LastCCY')], useNA = 'always')
table(sClaimCat[, c('BBLL', 'Settlement CCY')], useNA = 'always')


