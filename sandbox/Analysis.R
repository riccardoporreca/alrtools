# Author: Adam L. Rich
# Date:   August 30, 2011
# Description:
#
#   To analyze the US PE Rates
#   Data is from Red Cube, cleaned and categorized 8/29/2011
#   See <path> for more detail
#

# INITIALIZE AND ATTACH PROJECT OBJECT

init <- function(dir = NA, filename.csv = NA, filename.rdata = NA, recreate = FALSE) {
  
  # For testing:
  # dir <- NA; filename.csv <- NA; filename.rdata <- NA; recreate <- FALSE;

  # Set the working directory
  orig.wd <- getwd()
  curr.wd <- ifelse(is.na(dir), '//bfl.local/dfsroot/US_Users/richad/Desktop/US PE Rates/', dir)
  filename.csv   <- ifelse(is.na(filename.csv),   'Data from Red Cube.csv', filename.csv)  
  filename.rdata <- ifelse(is.na(filename.rdata), 'init.rdata'            , filename.rdata)  

  if ((recreate == F) &
      (file.exists(filename.rdata) == T)) {
    message('Data file already exists.  Loading from disk...')
    this <- local(get(load(filename.rdata)))
    return(this)
  }

  setwd(curr.wd)
  
  # 'this' is the list that init() will return with everything needed for an analysis
  this <- list()
  
  this$wd <- curr.wd
  this$filename.csv <- filename.csv

  this$data <- list()
  this$data$cleaned <- read.csv(this$filename.csv)
  
  # make a unique list of all policies
  # and data that is dependant on only policy.reference
  this$data$policies <- with(this$data$cleaned,{
    unique(data.frame(
      policy.reference, 
      inception.year, 
      revenue.band,
      total.gross.written.premium, 
      total.net.written.premium
    ))
  })

  # assuming that policy.reference is a "primary key" on data$policies.  CHECK!
  if (sum(duplicated(this$data$policies)) > 0) {
    stop('It is assumed that each policy in data$cleaned has only one year and one total premium.  This assumption has been violated.  Please investigate.')
  }
  
  # UNPIVOT revenue by class
  this$data$class.pivot <- with(this$data$cleaned, {
    t <- as.matrix(tapply(
      X     = revenue, 
      INDEX = list(policy.reference, service.class), 
      FUN   = sum
    ))
    
    # Replace NAs with zero
    t[is.na(t)] <- 0
    
    # This is required so that t gets assigned to data$class.pivot
    t
  })

  this$data$class.pivot <- merge(
    x = this$data$policies,
    y = cbind(as.data.frame(this$data$class.pivot),
              policy.reference = rownames(this$data$class.pivot))
  )
  
  # Reset environment
  setwd(orig.wd);
  
  save(this, file = filename.rdata)
  this

} # END FUNCTION init()

# get the data, and attach it
project.us.pe.rates <- init(recreate = TRUE)
attach(project.us.pe.rates)

f <- total.gross.written.premium ~ 
     `Class 1` + 
     `Class 2` + 
     `Class 3` + 
     `Class 4` + 
     `Class 5` + 
     `Class 6` +
     inception.year +
     revenue.band

g <- total.gross.written.premium ~ 
     revenue + revenue.band + service.class



lm(g, data$cleaned)
lm(f, data$class.pivot[data$class.pivot$inception.year == 2007,])
lm(f, data$class.pivot[data$class.pivot$inception.year == 2008,])
lm(f, data$class.pivot[data$class.pivot$inception.year == 2009,])
lm(f, data$class.pivot[data$class.pivot$inception.year == 2010,])
lm(f, data$class.pivot[data$class.pivot$inception.year == 2011,])