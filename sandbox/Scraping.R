# Author: Adam L. Rich
# Date:   July 25, 2012
# Description:
#
#   To scrape data from the ProjectEuler site
#
#   Each problem has a page
#   URLs for a problem look like
#
#     http://projecteuler.net/problem=1
#
# 
# library(XML)
# 
# url = "http://projecteuler.net/problem=1"
# doc = htmlTreeParse(url, useInternalNodes = T)
# 
# profiles = xpathSApply(doc, "//a[contains(@href, 'profile')]", xmlValue)
# profiles = profiles[!(1:length(profiles) %% 2)]
# 
# states   = xpathSApply(doc, "//a[contains(@href, 'bystate')]", xmlValue)
# 
# # clean data
# rm(list=ls())

# web url
site <- "http://projecteuler.net/problem=1"

a <- readLines(site)

