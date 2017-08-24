# Author: Adam L. Rich
# Date:   August 20, 2012
# Description:
#
#   SLPseudoPackage
#

# Packages required 
#   MASS, stats4, actuar, fitdistrplus, rcom, RODBC, R2HTML, xtable, ggplot2, Hmisc, copula, corpcor  

source('w:/sl/actuarial/richad/r/SLPseudoPackage.R')

files <- c('w:/sl/actuarial/richad/r/enhanced.r',
           'w:/sl/actuarial/richad/r/FancyCut.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/unfactor.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/negbintab.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/LatexTable.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/DateHandler.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/EarnedPrem3.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/LayeredFit2.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/charFunctions.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/GeneralRater4.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/HTMLutilities.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/LDF Mean Term.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/AggregateDist2.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/StringFunctions.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/accidentyearsplit3.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/AvailableLibraries.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/CensoredFitFunction.r',
           'w:/SL/Actuarial/R-Software/Utilities/Prod/deprecated.r')

rm(funcs.all)
for(f in files){
  newenv <- SourceToEnvironment(f)
  
  funcs <- data.frame(
    function.name = ls(newenv),
    file.path     = f
  )
  
  if(exists('funcs.all')) {
    funcs.all <- rbind(funcs.all, funcs)
  } else {
    funcs.all <- funcs
  }
}
rm(f, newenv, funcs)
