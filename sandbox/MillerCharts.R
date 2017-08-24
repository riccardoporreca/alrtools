# Author: Adam L. Rich
# Date:   November 1, 2013
# Description:
#
#   Miller charts
#

setwd('P:/Desktop/Miller')
source('W:/SL/Actuarial/R-Software/Utilities/Dev/SLPseudoPackage.R')
source('W:/SL/Actuarial/R-Software/2013R Conference/RLondon2013ChartFunctions.r')
require(RODBC)
require(reshape2)



# Load data
channel <- odbcConnectExcel2007('Data.xlsx')
triangle <- sqlFetch(channel, 'Losses')


tri.melt <- melt(triangle, id.vars = c('YOA', 'PeriodEnding', 'Dev'))

usd.melt <- tri.melt[tri.melt$variable %in% 
  c("IncurredUSD", "MostLikelyUSD", "BlendUSD", "PessimisticUSD"), ]

ulr.melt <- tri.melt[tri.melt$variable %in% 
  c("IncurredULR", "MostLikelyULR", "BlendULR", "PessimisticULR"), ]

count.melt <- tri.melt[tri.melt$variable %in% 
  c("Count", "Count per Million in Premium"), ]




# Turn on the output device
pdfOn <- TRUE
if(pdfOn == TRUE) pdf(file = "MillerCharts.pdf")

plotchart(
  ulr.melt, fac = "variable", vals = "value",
  x = "Dev", yl = "Loss Ratio", leg = "YOA",
  HeadTitle = "US PE London, Miller Insurance Services"
)

plotchart(
  usd.melt, fac = "variable", vals = "value",
  x = "Dev", yl = "Dollars of Loss", leg = "YOA",
  HeadTitle = "US PE London, Miller Insurance Services"
)

plotchart(
  count.melt, fac = "variable", vals = "value",
  x = "Dev", yl = "Count/Frequency", leg = "YOA",
  HeadTitle = "US PE London, Miller Insurance Services"
)

if(pdfOn == TRUE) dev.off()
