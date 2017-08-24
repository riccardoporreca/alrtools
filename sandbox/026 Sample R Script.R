# Author: Adam L. Rich
# Date:   February 9, 2013
# Description:
#
#   Shows how to load data
#     and a few commands to summarize
#


# This command loads the data from a CSV file to a dataframe
claims <- read.csv('p:/desktop/Mentoring/Claims.csv', stringsAsFactors = FALSE)



# str() shows you what structure the dataframe has
str(claims)



# head() shows the first few records of the dataframe
# names() shows the names of the fields
head(claims)
names(claims)



# Convert PeriodEnding to be a date
claims$PeriodEnding <- as.Date(claims$PeriodEnding)



# Check the dev variable
# The sum command below should return zero
PeriodEnding.month  <- as.integer(as.character(claims$PeriodEnding, '%m'))
PeriodEnding.year   <- as.integer(as.character(claims$PeriodEnding, '%Y'))

DevCheck <- PeriodEnding.year * 4 - claims$YOA * 4 + PeriodEnding.month / 3

sum(claims$dev != DevCheck)   # Should return 0




# Convert all non-numeric, non-date fields (tags) to factors
for (f in c("ClaimId", "YOA", "ClaimSeverityInd", "ClaimStatus")) {
  claims[, f] <- as.factor(claims[, f])
}
rm(f)



# Show the number of claims by ClaimSeverityInd and dev
counts <- as.data.frame(table(claims[, c('dev', 'ClaimSeverityInd')]))
counts <- reshape(
  data = counts, 
  direction = 'wide', 
  timevar = 'ClaimSeverityInd', 
  idvar = 'dev'
)
counts$Freq.Total <- with(counts, Freq.1 + Freq.2 + Freq.3 + Freq.4)
counts



# Add Ratios
counts <- within(counts, {
  
  Ratio.4 <- Freq.4 / Freq.Total
  Ratio.3 <- Freq.3 / Freq.Total
  Ratio.2 <- Freq.2 / Freq.Total
  Ratio.1 <- Freq.1 / Freq.Total
  
})
counts



                        