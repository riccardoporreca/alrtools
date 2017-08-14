

# Move to package
SmartCVSLoad <- function(pathToFile) {
  
  my.df <- read.csv(pathToFile, stringAsFactors = FALSE)
  
  
  for (f in names(my.df)) {
    # Change characters to factors
  }
  
  my.df
}


# 
# GetData <- function(pathToFile) {
#   
#   my.df <- read.csv(paste(proj.dir, pathToFile, sep = ''), stringAsFactors = FALSE)
#   
#   
#   for (f in names(my.df)) {
#     # Change characters to factors
#   }
#   
#   my.df
# }
# 


GetData <- function(pathToFile) {
  SmartCSVLoad(paste(proj.dir, pathToFile, sep = ''))
}
