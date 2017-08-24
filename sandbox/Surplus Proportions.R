# Author: Adam L. Rich
# Date:   November 5, 2013
#         May 1, 2014
# Description:
#
#   To quickly try and band up recoveries with proportional cessions
#


source('W:/SL/Actuarial/R-Software/Utilities/Dev/SLPseudoPackage.R')
setwd('W:/SL/Actuarial/ClaimCategorisation_ALR/Quarters/2014q1')


sClaimCat.env <- LoadToEnvironment('2014q1 sClaimCat.RData')
sClaimCat <- sClaimCat.env$sClaimCat
buildup.env <- LoadToEnvironment('BuildupCache.Rdata')
buildup <- buildup.env$CombinedBuildup


hb <- HFactory(buildup)


props <- buildup[, c("Policy Ref", "Proportion RI For Incurred")]
names(props) <- c('PolicyRef', 'ProportionalRate')
props <- props[!duplicated(props$PolicyRef), ]


claims <- sClaimCat[
  sClaimCat$dev == 13 & sClaimCat$YOA %in% 2007:2011 & !is.na(sClaimCat$PolicyRef), 
  c('YOA', 'PolicyRef', 'BlendUSD', 'dev', 'RevFocusArea')]


# Checks before merging
sum(is.na(claims$PolicyRef))
sum(duplicated(props$PolicyRef))
nrow(props)
nrow(claims)



# Merge
merged <- merge(props, claims)
nrow(merged)



profs <- c('A&E MM & PE',
           'Chicago Healthcare',
           'D&O',
           'D&O Large Risk Int',
           'D&O MM',
           'DIN US Lawyers',
           'Healthcare - Hospitals, LTC & MC (Exc RSG)',
           'Healthcare - RSG',
           'Healthcare - Misc Med',
           'Healthcare Large Risk Int',
           "Lloyd's TMB MM",'LTC MM',
           'Med Misc Healthcare MM',
           'MM Lawyers Combined',
           'PI-A&E exBLPT',
           'PI-Lawyers exBLPT',
           'TMB Large Risk Int',
           'TMB MM E&O',
           'TMB MM Infosec & BBR',
           'TMB UK Info Sec (2009 - 2012)',
           'TMB UK Tech (exc InfoSec)',
           'BBR Services UK',
           'BBR Services US')

merged.profs <- merged[merged$RevFocusArea %in% profs, ]




