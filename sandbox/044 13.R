# Author: Adam L. Rich
# Date:   February 22, 2013
# Description:
#
#   Project Euler #13
#

source('Digital.R')

con <- file('13.numbers.txt', 'r') 
nums <- readLines(con)
close(con)


total <- rep(0, nchar(nums[1]))


nums.split <- strsplit(nums, split = '')


for(n in 1:length(nums.split)) {  
  nums.split[[n]] <- as.integer(as.character(nums.split[[n]]))
}


nums.df <- as.data.frame(nums.split)
names(nums.df) <- paste('N', 1:length(nums.df), sep = '')

S <- rowSums(nums.df)


S <- S[50:1]
S.digital <- new_Digital(0)
S.digital[1:50] <- S

S.digital <- as.Digital(S.digital)