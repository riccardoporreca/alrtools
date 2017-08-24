# Author: Adam L. Rich
# Date:   March 26, 2015
# Description:
#
#   Preping A&E sClaimCat data for investigation in Excel
#



# Setup
source('w:/sl/actuarial/r-software/utilities/Dev/SLPseudoPackage.R')
source('w:/sl/actuarial/claimcategorisation_alr/projects/FrequencyReporting/Dev/FrequencyChartFunctionsDev.R')
library(ChainLadder)

selectedFG <- 'EPL'

# Load in sClaimCat
load('W:\\SL\\Actuarial\\ClaimCategorisation_ALR\\Quarters\\2014q4\\2014q4 sClaimCat.Rdata')

# # Load in Premiums from ChartCreationMacro
# load('W:\\SL\\Actuarial\\ClaimCategorisation_ALR\\Quarters\\2014q4\\ulrs.RData')
# prems <- ulrP[ ulrP$COB == selectedFG,]
# 
# 
# prem.agg <- aggregate(
#   x = prems[, c('Premiums')], 
#   by = prems[, c('YOA', 'Development_Quarter')], 
#   FUN = sum,
#   na.rm = FALSE
# )
# 
# 
# triangle.p <- as.triangle(
#   Triangle = prem.agg, 
#   origin = 'YOA', 
#   dev = 'Development_Quarter', 
#   value = 'x'
# )
# 
# LatestPrems <- as.data.frame(getLatestCumulative(triangle.p, na.values = NULL))
# # add YOA
# LatestPrems$YOA <- 2003:2015
# LatestPrems <- LatestPrems[,c(2,1)]
# names(LatestPrems) <- c('YOA','Gross Prem')
# ## PREM1
# 
# copy.table(LatestPrems)


# Get Selected Focus Group data only
sc <- sClaimCat[sClaimCat$CombinedFocusArea == selectedFG, ]
sc$HasMeasures <- 
  !(abs(sc$MostLikelyUSD - sc$IncurredUSD) < 5 &
  abs(sc$BlendUSD - sc$IncurredUSD) < 5 & 
  abs(sc$PessimisticUSD - sc$IncurredUSD) < 5)



# Are there claims with more than one YOA?
a <- unique(sc[, c('BCClaimRef', 'YOA')])
b <- a$BCClaimRef[duplicated(a$BCClaimRef)]
#sc[sc$BCClaimRef %in% b, ]

sc <- sc[ !sc$BCClaimRef %in% b,]


# # Delete records with bad YOA
# sClaimCat.ae2 <- sClaimCat.ae[
#   !((sClaimCat.ae$BCClaimRef == 'V100YF06PNPMC0001' & sClaimCat.ae$YOA == 2005) | 
#       (sClaimCat.ae$BCClaimRef == 'W15KW309PNPAC0001' & sClaimCat.ae$YOA == 2008)), ]




# Get the first development point by BCClaimRef
first.dev <- aggregate(
  x = sc[, 'dev', drop = FALSE], 
  by = sc[, c('BCClaimRef'), drop = FALSE], 
  FUN = min
)

names(first.dev) <- c('BCClaimRef', 'FirstDev')




# Merge and shorten name
# Shorten column set
sc <- merge(sc, first.dev)

n <- c("BCClaimRef", "Account name", "Claimant", "YOA", "Quarter", "dev", "FirstDev", 
       "ClaimSeverityInd", "ClaimStatus", "HasMeasures", 
       "IncurredUSD", "MostLikelyUSD", "BlendUSD", "PessimisticUSD")


sc <- sc[, n]
names(sc) <- c('BCClaimRef', 'AccountName', 'Claimant', 'YOA', 'Quarter', 'Dev', 'FirstDev', 
               'ClaimSeverity', 'ClaimStatus', 'HasMeasures',
               'Incurred', 'ML', 'Blend', 'Pess')
sc$Count <- 1




# Define "last diagonal" and buckets 
sc.ld <- sc[sc$Quarter == '2014q4', ]


### this is where we define the buckets as either:
###   1) CAT1 
###   2) Incurred greater than 100k

sc.ld$Bucket.ClaimSeverity <- ifelse(sc.ld$ClaimSeverity == 1, 'A', 'B')
sc.ld$Bucket.Incurred <- ifelse(sc.ld$Incurred > 100000, 'A', 'B')


#aa <- sc[ !sc$ClaimSeverity == 1 & sc$Incurred < 100000 & sc$Pess > 1e6,]

# Merge bucket info back
sc <- merge(sc, sc.ld[, c('BCClaimRef', 'Bucket.ClaimSeverity', 'Bucket.Incurred')])





# Aggregate and copy
sc.agg <- aggregate(
  x = sc[, c('Incurred', 'ML', 'Blend', 'Pess', 'Count')], 
  by = sc[, c('YOA', 'Quarter', 'Dev', 'FirstDev', 'ClaimSeverity', 'ClaimStatus', 'Bucket.ClaimSeverity', 'Bucket.Incurred')], 
  FUN = sum,
  na.rm = FALSE
)



# Bucket B Triangle
sc.b <- sc[sc$Bucket.ClaimSeverity == 'B' &
             sc$Dev %% 4 == 0 &
             sc$Dev > 0, ]

triangle.b <- as.triangle(
  Triangle = sc.b, 
  origin = 'YOA', 
  dev = 'Dev', 
  value = 'Incurred'
)

triangle.b[1,1] <- 1


output <- MackChainLadder(triangle.b, tail = 1)

plot(output)



## Get Mack Output ## OUTPUT1
s <- summary(output)
ss <- s$ByOrigin
ss$YOA <- 2003:2014
ss <- ss[,c("YOA","Latest","Dev.To.Date","Ultimate","IBNR","Mack.S.E","CV(IBNR)")]
copy.table(ss)
copy.table(ss)


## Get All Output ## OUTPUT2

copy.table(sc.agg, buffer.size = 5000)

## Get Last Output
copy.table(sc.ld, buffer.size = 5000)




## Play ground

Mortgage
m <- MackChainLadder(Mortgage)
round(summary(m)$Totals["CV(IBNR)",], 2) ## 26% in Table 6 of paper

summary(m)$Totals["CV(IBNR)",]
s <- summary(m)
ss <- s$ByOrigin
ss
ss$YOA <- 2003:2011
ss <- ss[,c("YOA","Latest","Dev.To.Date","Ultimate","IBNR","Mack.S.E","CV(IBNR)")]
copy.table(ss)


output
Latest <- as.vector(getLatestCumulative(output$Triangle, na.values = NULL))
Ultimate <- as.vector(getLatestCumulative(output$FullTriangle, na.values = NULL))
IBNR <- Ultimate - Latest
MackSE <- as.vector(getLatestCumulative(output$Mack.S.E, na.values = NULL))

cbind(as.numeric(row.names(output$Triangle)), Latest, Ultimate, IBNR, MackSE)

tri <- output$Triangle
