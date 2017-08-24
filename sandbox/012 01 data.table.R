# Author: Adam L. Rich
# Date:   December 8, 2014
# Description:
#
#   data.table
#



# SETUP
setwd('P:/Desktop/LAS Orlando/')
library(data.table)
source('LoadFunctions.R')




# Load data
# Check names
# Rename columns
# asl17.group <- read.csv('http://www.casact.org/research/reserve_data/othliab_pos.csv')
asl17 <- read.csv('othliab_pos.csv')
actual.names <- names(asl17)
expected.names <- c("GRCODE", "GRNAME", "AccidentYear", "DevelopmentYear", "DevelopmentLag", 
                    "IncurLoss_h1", "CumPaidLoss_h1", "BulkLoss_h1", "EarnedPremDIR_h1", 
                    "EarnedPremCeded_h1", "EarnedPremNet_h1", "Single", "PostedReserve97_h1"
)

stopifnot(all(actual.names == expected.names))


names(asl17) <- c('GRCODE', 'GRNAME', 'AY', 'DY', 
                  'dev', 'UltLoss', 'PdLoss', 'IBNR', 'GPE',
                  'CPE', 'NPE', 'single', 'OS97')




# asl17 is a data.frame
# Create a data.table
# Follow the convention that data.tables are ALL CAPS
# but otherwise the same name as the data.frame
ASL17 <- data.table(asl17)




# Compare the two
str(asl17)
str(ASL17)

class(asl17)
class(ASL17)




# Create some other tables, for illustration
# In real world, these will probably be loaded,
# not created
groups <- unique(asl17[, c('GRCODE', 'GRNAME')])
GROUPS <- data.table(groups)

single <- data.frame(
  single = c(0, 1),
  singleDesc = c('Group', 'single')
)

SINGLE <- data.table(
  single = c(0, 1),
  singleDesc = c('Group', 'single')
)




# See what data.tables you have created
tables()




# Time to create keys on the data.tables
setkey(SINGLE, single)
setkey(GROUPS, GRCODE)

# setkey(ASL17, ???)  # What should be the key on ASL17?
                      # How will you know it is unique?




# One possible answer...
setkey(ASL17, GRCODE, AY, dev)
nrow(
  unique(
    asl17[, c('GRCODE', 'AY', 'dev')]
  )
)
nrow(asl17)




# Add some data to groups, GROUPS for merging demo
score <- sample(
  x = c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C', 'D', 'F'), 
  size = nrow(groups), 
  replace = TRUE, 
  prob = c(1, 2, 4, 8, 8, 8, 4, 3, 1)
)

head(GROUPS)
GROUPS[, score:=score]  # DT way
head(GROUPS)

groups <- as.data.frame(GROUPS)

rm(score)



# Demonstrate a merge
asl17.withscore <- merge(
  x = asl17, 
  y = groups[, c('GRCODE', 'score')], 
  by = c("GRCODE")
)


# ASL17.withscore <- ASL17[GROUPS]


# asl17[, c('GRCODE', 'AY', 'dev')]
# GROUPS[, .(GRCODE, score)]

ASL17.withscore <- ASL17[GROUPS[, .(GRCODE, score)]]

identical(
  data.table(asl17.withscore, key = c('GRCODE', 'AY', 'dev')), 
  ASL17.withscore
)




# What were the time differences?
system.time(merge(x = asl17, y = groups))
system.time(ASL17[GROUPS[, .(GRCODE, score)]])




# Just to make code shorter going forward...
asl17 <- asl17.withscore
ASL17 <- ASL17.withscore



# 
as.data.frame(ASL17)[1, 1]





# Subsetting rows
ASL17[1:100, ]
ASL17[score == 'A+']

ASL17[score %in% c('A+', 'A')]




# You can use data tables wherever you would use a data frame
# Just remember that the [] indexing works differently!
as.triangle(
  ASL17[score %in% c('A+', 'A') & DY <= 1997], 
  origin = 'AY', 
  value = 'PdLoss'
)




# Selecting specific columns
ASL17[, score]
ASL17[, .(score, count = length(score)), by = score]  # first score unnecessary
                                                      # contrast with SQL
ASL17[, .(count = length(score)), by = score]
ASL17[, .(count = .N), by = score]




# Add columns
ASL17[, IncLoss := UltLoss - IBNR]




# Remove columns
ASL17[, UltLoss := NULL]




# Special indexing rules
single[1, ]
SINGLE[1]

SINGLE[.(1)]
SINGLE[.(0)]

ASL17[337]
ASL17[.(337, 1990, 3)]








