# Author: Adam L. Rich
# Date:   December 9, 2011
#         January 5, 2012
#         February 13, 2012
#         August 20, 2012
#         January 4, 2013
#         December 29, 2014
#
# Description:
#
#   [December 9, 2011 ALR]
#   Loads scripts to an environment that is then attached and discarded
#   Keep .GlobalEnv clean!
#
#   [January 5, 2012 ALR]
#   Figured out why Functions.r was not working.  Always good to read the code...
#   I don't like the idea of loading everything in a directory.
#   So, if anything is added to Functions.r in the future, it should also be explicitly added here
#
#   [August 20, 2012 ALR]
#   Added reference to Hamid's "deprecated" functions
#   Removed reference to "ascii"
#   Added the other packages that are required
#
#   [January 4, 2013 ALR]
#   Removed auto-install code
#
#   [December 29, 2014 ALR]
#   Added Simon's chart functions file
#




# # Make sure all dependencies are installed
# if (!(require(MASS)))         {install.packages("MASS", dependencies = TRUE)};          #base
# if (!(require(stats4)))       {install.packages("stats4", dependencies = TRUE)};        #useful stats
# if (!(require(actuar)))       {install.packages("actuar", dependencies = TRUE)};        #for simulation
# if (!(require(fitdistrplus))) {install.packages("fitdistrplus", dependencies = TRUE)};  #for curve fitting
# if (!(require(rcom)))         {install.packages("rcom", dependencies = TRUE)};          #for communication to Excel
# if (!(require(RODBC)))        {install.packages("RODBC", dependencies = TRUE)};         #for communication to SQL Server
# if (!(require(R2HTML)))       {install.packages("R2HTML", dependencies = TRUE)};        #for writing to HTML
# if (!(require(xtable)))       {install.packages("xtable", dependencies = TRUE)};        #for latex (report writing)
# if (!(require(ggplot2)))      {install.packages("ggplot2", dependencies = TRUE)};       #for graphs
# if (!(require(Hmisc)))        {install.packages("Hmisc", dependencies = TRUE)};         #for graphs
# if (!(require(copula)))       {install.packages("copula", dependencies = TRUE)};        #for RDS
# if (!(require(corpcor)))      {install.packages("corpcor", dependencies = TRUE)};       #for RDS

# [August 12, 2013 ALR]
# Do not require rcom unless it is installed

stopifnot({
  require(MASS)         
  require(stats4)       
  require(actuar)       
  require(fitdistrplus) 
  # require(rcom)         
  require(RODBC)        
  require(R2HTML)       
  require(xtable)       
  require(ggplot2)      
  require(Hmisc)        
  require(copula)
  require(corpcor)
})

if(length(intersect(row.names(installed.packages()), 'rcom')) == 1) require(rcom)




with(new.env(), {
  
  # Check to see if SLPseudoPackage is already installed
  if(!'SLPseudoPackage' %in% search()) {
    
    SLPseudoPackage <- new.env()
    with(SLPseudoPackage, {
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/enhanced.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/FancyCut.r', local = TRUE)
      
      # Components of Functions.r
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/unfactor.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/negbintab.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/LatexTable.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/DateHandler.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/EarnedPrem3.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/LayeredFit2.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/charFunctions.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/GeneralRater4.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/HTMLutilities.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/LDF Mean Term.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/AggregateDist2.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/StringFunctions.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/accidentyearsplit3.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/AvailableLibraries.r', local = TRUE)
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/CensoredFitFunction.r', local = TRUE)
      
      # [December 29, 2014 ALR]
      base::source('w:/SL/Actuarial/R-Software/Utilities/Dev/ChartFunctions.r', local = TRUE)
      
      # [June 29, 2012 HAR]
      base::source('W:/SL/Actuarial/R-Software/Utilities/Dev/deprecated.r', local = TRUE)

      # Put other files to source here
    })
    attach(SLPseudoPackage)
  }
})

