#'% Author: Simon Brickman
#'% Date:   July 4, 2013
#'% Description:
#'% 
#'%   Automated Reporting Presentation Material
#'% 



#' Automated Reporting
#' ====================



#+ setup, include = FALSE
#
# Set project directory
#   setwd('~/2013R Conference/')
#
require(knitr)
require(directlabels)
require(ggplot2)
require(reshape2)
require(data.table)
require(scales)
setwd('w:/sl/actuarial/R-Software/2013R Conference/')



#+ help-compile, include = FALSE
#
# tools::Rd2HTML is the internal function 
# used to build HTML files from Rd files
# 
tools::Rd2HTML(
  'RL2013_MAChart.Rd', 
  'RL2013_MAChart.html',
  package = 'R in Insurance: Automated Reporting',
  no_links = TRUE
)

tools::Rd2HTML(
  'RL2013_plotchart.Rd', 
  'RL2013_plotchart.html',
  package = 'R in Insurance: Automated Reporting',
  no_links = TRUE
)



#' The functions we are sharing are part of a 
#' not yet fully developed, internal use only package
#' called `BZLYCharts`.
#' If this package was available, we would simply type
#' 
#'     require(BZLYcharts)  
#'     
#' For purposes of this presentation,
#' we will load a "pseudo package".



#+ load-pseudo
#
# The following loads the R script and puts it in the search path
# It keeps our global environment clean and mimics a package.
ChartFunctions <- new.env()
with(ChartFunctions, {source("RLondon2013ChartFunctions.r", local = TRUE)})
attach(ChartFunctions); rm(ChartFunctions)



#' To get a list of the functions loaded, type `ls(2)`.
#+ pseudo-package-functions
ls(2)



#' This part of the presentation will focus on use of chart functions 
#' to help analysis of data.
#'   1. [plotchart][plotchart-Rd]
#'   2. [MAChart][MAChart-Rd]
#'   
#'   [plotchart-Rd]: ./RL2013_plotchart.html
#'   [MAChart-Rd]: ./RL2013_MAChart.html
#'     



#' Simple Example
#' ---------------
#' 
#' We start with simple example with a small data frame.
#' This data frame, `res`, is triangular claims data in a tabular form.
# Specify datasource
res <- data.frame(
  YOA = rep.int(c(2006, 2007), c(8, 4)),
  dev = c(1:8, 1:4),
  Loss_Ratio = c( 5, 10, 20, 40, 60, 70, 75, 78,
                  7, 12, 30, 50, 20, 40, 70, 80,
                 78, 75, 75, 73, 39, 60, 70, 75),
  Type = rep.int(c("Paid","Incurred"), c(12, 12))
)
res



#' ### "Tabular" Triangle
#' We can see this as a triangle using `tapply`.
#' The following method for building triangles only works
#' because YOA, dev, and Type form a key on res
with(res, tapply(Loss_Ratio, list(YOA, dev, Type), I))



#' ### "Graphical" Triangle
#' Now use this one-line statement to see this information graphically
plotchart(
  df = res,                         
  fac = "Type", 
  vals = "Loss_Ratio",
  x = "dev",
  yl = "LR(%)",
  leg = "YOA",
  HeadTitle = "Example Loss Ratios"
)



#' What is `plotchart` Doing?
#' ---------------------------
#' 
#' Open the help file for [`plotchart`][plotchart-Rd] in a new window
#' to see its parameters.
#' First parameter is the data frame.  
#' We have specified this as `res`.
#' Next four parameters are column names in `df`.
#' 

# the next 4 parameters refer to field names in the dataframe
str(res)

#' `fac` refers to the factor used to generate the series of charts. 
#' Each level of `fac` will have a corresponding chart in the output. 
#' So, here `Type` has two levels and we get two charts.
#' 
#' `vals` refers to the field containing the values.
#' 
#' `yl`, stands for y label, is the name for the y-axis.
#' 
#' `leg` refers to the legend key or the factor that defines how many
#' lines are shown on each chart.  
#' Here, there are two levels of `leg` so each chart has two lines.
#' 
#' Finally, the only additional parameter used was for the title.



#' `plotchart` was built to make it easy to make a lot of charts
#' and to be able to quickly change how the data was laid out
#' without having to copy chunks of code or formatting.
#' For example, it is very easy to change the x- and y-axes.

# Switch axes around, for example to show by YOA
plotchart(
  res,
  fac = "YOA",
  vals = "Loss_Ratio",
  x = "dev",
  yl = "LR(%)",
  leg = "Type",
  HeadTitle = "Example Loss Ratios"
)



#' More Complicated Example
#' -------------------------
#' 
#' Let's try a more involved dataset of individual claims.

#+ claims-rdata
load("Claims.RData")
head(sClaimCat.anon)

#' We see that the claims are recorded indivdually at each quarters development.
#' Let's check structure of dataframe
str(sClaimCat.anon)
#' So 4440 claims, 68297 rows of information



#' `plotchart` requires this data be "tall" and not "wide", 
#' as we saw with the `res` data above.
#' First thing to do is to make the measures a column in the data, 
#' rather than spread across columns.
#' An easy way to do this is to use the very useful function `melt` from 
#' package `reshape2` to reshape the data.

require(reshape2)
res <- melt(
  sClaimCat.anon,
  value.name = "val",
  measure.vars = c(7:10),
  variable.name = "Type"
)

#' Let's look at the data
head(res)
str(res)

# Now consolidate using powerful package data.table to summarise data
require(data.table)
RES <- data.table(res)
setkey(RES, YOA, dev, Type)
RES2 <- RES[, list(valsum = sum(val), valnum = length(val)), key(RES)]
res2 <- as.data.frame(RES2)

#' We can summarise the data in a table again and 
#' can see this as a triangle
with(res, tapply(val, list(YOA, dev, Type), sum))



#' I'll use convention of upper case for data.table objects, 
#' and lower case for data.frame only versions 
#' (plotchart reads data.frames only).



#' Now plot results
plotchart(
  res2,
  fac = "Type",
  vals = "valsum",
  x = "dev",
  yl = "Cost",
  leg = "YOA",
  HeadTitle = "Claim Development"
)



#' The labelling looks too busy 
#' as there are too many lines, 
#' so use of standard key is preferred
plotchart(
  res2,
  fac = "Type",
  vals = "valsum",
  x = "dev",
  yl = "Cost",
  leg = "YOA",
  HeadTitle = "Claim Development",
  usedirectlabel = FALSE
)



#' Now let's switch legend and factors to see how measures compare
plotchart(
  res2,
  fac="YOA",
  vals = "valsum",
  x = "dev",
  yl = "Cost",
  leg = "Type",
  HeadTitle = "Claim Development"
)



#' If we want to send these charts out we can simply specify pdf output.
#' (We plan to offer HTML/PNG indexed and individual output in the future.)
pdfOn <- TRUE
if(pdfOn == TRUE) pdf(file = "Example1.pdf")
plotchart(
  res2,
  fac = "YOA",
  vals = "valsum",
  x = "dev",
  yl = "Cost",
  leg = "Type",
  HeadTitle = "Claim Development"
)
if(pdfOn == TRUE) dev.off()



#' We can also look at claim numbers - 
#' and if we had premium or an exposure measure
#' the numbers could be turned into a frequency measure.



#' It doesn't make any sense to show numbers for all measures 
#' as these will be identical, so run without factor.
plotchart(
  res2,
  fac = "",
  vals = "valnum",
  x = "dev",
  yl = "Number",
  leg = "YOA",
  HeadTitle = "Claim Development",
  usedirectlabel = FALSE
)



# It may be interesting to see development of average claim separated
# between closed and open cases for incurred only.
setkey(RES, YOA, dev, Type, ClaimStatus)
RES3 <- RES[, list(valsum = sum(val), valnum = length(val)), key(RES)]
RES3 <- RES3[, AvgClaim:=valsum/valnum]  # add new column
res3 <- as.data.frame(RES3[Type == "IncurredUSD" & YOA < 2011 & ClaimStatus == "C"])  

plotchart(
  res3,
  fac = "",
  vals = "AvgClaim",
  x = "dev",
  yl = "Average Claim",
  leg = "YOA",
  HeadTitle = "Average Claim Amount for Closed Cases",
  usedirectlabel = FALSE
)



#' If we limit years we can effectively zoom in years of interest
res3 <- as.data.frame(
  RES3[Type == "IncurredUSD" & YOA < 2011 & YOA > 2006 & ClaimStatus == "C"]
)  

plotchart(
  res3,
  fac="",
  vals = "AvgClaim",
  x = "dev",
  yl = "Average Claim",
  leg = "YOA",
  HeadTitle = "Average Claim Amount for Closed Cases",
  usedirectlabel = FALSE
)


#' Let's compare the measures 
#' against the proposed ultimate values (called HELD).

# TODO?: add data

# See current values of BlendUSD
# RES2[RES2$Type == "BlendUSD" & RES2$PeriodEnding == max(RES2$PeriodEnding)]
     
RESH <- subset(RES2, Type == "BlendUSD") # create copy of Blend
RESH <- RESH[, valsum:=NULL]             # delete valsum
RESH$Type <- as.character(RESH$Type)     # replace factor with character type
RESH$Type <- "Held"                      # change Type content
HELD <- data.table(
  YOA = 2003:2012,
  valsum = c(25e6,30e6,45e6,20e6,50e6,50e6,50e6,35e6,35e6,30e6)
)
setkey(HELD, YOA)                        # set up key to join (add) HELD to RESH
setkey(RESH, YOA)
RESH <- HELD[RESH]                       # this does join

# Join on to previous RES2
RES4 <- rbind(RES2, RESH)

# Look at different measues
plotchart(
  as.data.frame(RES4),
  fac = "YOA",
  vals = "valsum",
  x = "dev",
  yl = "Claim Cost",
  leg = "Type",
  HeadTitle = "Comparison of Measures",
  usedirectlabel = FALSE
)

# Add column to create development ratios 
setnames(HELD, "valsum", "ult")         # change name to avoid confusion with values in other measures
setkey(RES4, YOA)                       # set up key to add ultimates to RES2
RES4 <- HELD[RES4]
RES4 <- RES4[, Development:=valsum/ult] # create development ratios

# development
plotchart(
  as.data.frame(RES4[!Type == 'Held']),
  fac = "Type",
  vals = "Development",
  yl = "Ratio to Ultimate",
  leg = "YOA",
  HeadTitle = "Loss Development",
  yScaleMin = 100,
  scale.by = 100,
  usedirectlabel = FALSE
)

plotchart(
  as.data.frame(RES4),
  fac = "YOA",
  vals = "Development",
  yl = "Ratio to Ultimate",
  leg = "Type",
  HeadTitle = "Loss Development",
  yScaleMin = 100,
  scale.by = 100,
  usedirectlabel = FALSE
)


# zzzz
# Now look at more involved example looking at duration to close and change
# over time

# load data
load("claims2.RData")
ca<-claims2

ca$Duration<-as.numeric(ca$ClaimClosedDt-ca$ClaimOpenedDt)
ca$ClosedYYMM<-format(ca$ClaimClosedDt,'%Y%m')

ca$Size<-5-findInterval(ca$IncurredUSD,c(0,.1,10000,250000,20e6))

# run by split
CA<-data.table(ca)
setkey(CA,ClosedYYMM,ClaimSeverityInd)  # add factor ClaimSeverityInd
CASUM<-CA[,list(num=length(Duration),mn=mean(Duration)),by=key(CA)]

# create full range from start date to end date in monthly steps
v<-monthsteps(d1=as.Date("2006-01-01"),as.Date("2012-12-31"))
df<-data.frame(v,ClosedYYMM=format(v,'%Y%m'))

sev<-data.frame(ClaimSeverityInd=1:4)
DF<-data.table(merge(df,sev,all=T))
setkey(DF,ClosedYYMM,ClaimSeverityInd)  

CAS2<-CASUM[DF]               # outer join DF to CASUM
CAS2[is.na(CAS2)]<-0          # replace NAs with 0
CAS2<-CAS2[,totdays:=mn*num] # add column for total open period per closed month

# run overall
setkey(CA,ClosedYYMM)
CASUM<-CA[,list(num=length(Duration),mn=mean(Duration)),by=key(CA)]

# create full range from start date to end date in monthly steps
DF<-data.table(df)
setkey(DF,ClosedYYMM) 

CAS1<-CASUM[DF]               # outer join DF to CASUM
CAS1[is.na(CAS1)]<-0          # replace NAs with 0
CAS1<-CAS1[,totdays:=mn*num] # add column for total open period per closed month

require(scales)

# pdf(file = paste(path,QtrToUse,"/charts to send out/duration to closure",
#               "/US A&E Duration to Closure.pdf",sep=""))
MAChart(df=as.data.frame(CAS1),ywt=2,ycol=3,xcol=4,
        main="Duration to close (12m moving average)",
        xlab="Closed Year+Month",ylab="Average Duration (days)",xstep=3,xRotate=T)
MAChart(df=as.data.frame(CAS2),ywt=3,ycol=4,xcol=5,
        main="Duration to close (12m MA) by SeverityIndicator",
        xlab="Closed Year+Month",ylab="Average Duration (days)",xstep=3,leg="ClaimSeverityInd",
        xRotate=T)

# dev.off()

