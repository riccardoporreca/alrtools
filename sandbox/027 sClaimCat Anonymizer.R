# Author: Adam L. Rich
# Date:   February 7, 2013
# Description:
#
#   To anonymize Pre Peer data for a class
#


source('w:/sl/actuarial/R-Software/Utilities/Prod/SLPseudoPackage.R')


sClaimCat.env <- LoadToEnvironment(
  'w:/sl/actuarial/ClaimCategorisation_ALR/Quarters/2012q4/2012q4 sClaimCat.Rdata')


sClaimCat <- sClaimCat.env$sClaimCat
hs <- HFactory(sClaimCat)


COB <- c('DIN US Lawyers', 'MM Lawyers Combined', 'PI-Lawyers exBLPT')



pols.all <- unique(sClaimCat[sClaimCat$CombinedFocusArea %in% COB, 'PolicyRef'])
pols.sample <- sample(pols.all, 1000, replace = FALSE)



sClaimCat.sample <- sClaimCat[
  sClaimCat$PolicyRef %in% pols.sample, 
  c("BCClaimRef", "PeriodEnding", "ClaimSeverityInd", 
    "ClaimStatus", "YOA", "dev", 
    "IncurredUSD", "MostlikelyUSD", "PessimisticUSD", "BlendUSD")]



claims.sample <- unique(sClaimCat.sample$BCClaimRef)



claims.map <- data.frame(
  BCClaimRef = sample(claims.sample),
  ClaimId = paste('C', 1:length(claims.sample), sep = '')
)



sClaimCat.merged <- merge(
  x = sClaimCat.sample,
  y = claims.map
)



sClaimCat.anon <- sClaimCat.merged[, c(
  "ClaimId", "YOA", "dev", "PeriodEnding", 
  "ClaimSeverityInd", "ClaimStatus", 
  "IncurredUSD", "MostlikelyUSD", 
  "BlendUSD", "PessimisticUSD")]


save(sClaimCat.anon, file = 'p:/desktop/Mentoring/Claims.RData')
write.csv(sClaimCat.anon, file = 'p:/desktop/Mentoring/Claims.csv')
