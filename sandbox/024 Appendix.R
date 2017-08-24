# Authors:  Il Mok Park
#           Qiao Jiang
#           Daniel Shin
#           Adam L. Rich
# Date:     March 21, 2013
# Description:
#
#   Code for mentoring project
#


# Set the working directory to the project folder
#
#   setwd('C:/Users/Qiao/Desktop/')
#   setwd('p:/Desktop/Mentoring')
#


##########################################################
#
# Data Manipulation 
# (This code added missed data to our dataset, 
#   hoping to get relatively complete data. )
#
########################################################## 


# Read the data from CSV file
#
#   Claims <- read.csv("Claims for R.csv")
#
Claims <- read.csv('Claims.csv')



# Change ClaimId to an integer
#   This code works because
#   ClaimId is a factor
#   If it were a character vector this would fail
Claims$ClaimId <- as.integer(Claims$ClaimId)



# Check to make sure that ClaimId and Dev is a 
#   unique key on Claims
nrow(unique(Claims[, c('ClaimId', 'dev')]))
nrow(Claims)



# Which are duplicates?
#   Delete all claims that have duplicates
Claims.todelete <- Claims[duplicated(Claims[, c('ClaimId', 'dev')]), ]$ClaimId
Claims.todelete <- unique(Claims.todelete)
Claims <- Claims[!Claims$ClaimId %in% Claims.todelete, ]



# Are there any claims that have more than one YOA?


# What is the greatest DEV?
dev.max <- max(Claims$dev)



# How many Claims are there?
length(unique(Claims$ClaimId))



# Make a data.frame of all possible combinations
#   of dev and ClaimId
combos <- merge(
  x = data.frame(ClaimId = unique(Claims$ClaimId)), 
  y = data.frame(dev = 1:dev.max)
)



# Get max of dev for each claim
devs.max <- tapply(Claims$dev, Claims$ClaimId, max)
devs.min <- tapply(Claims$dev, Claims$ClaimId, min)
devs.YOA <- tapply(Claims$YOA, Claims$ClaimId, min)

# Make sure our tapply vectors are in the same order
stopifnot(all(names(devs.max) == names(devs.min)))
stopifnot(all(names(devs.max) == names(devs.YOA)))

devs.bound <- data.frame(
  ClaimId = names(devs.min),
  dev.min = devs.min,
  dev.max = devs.max,
  YOA = devs.YOA
)
rm(devs.max, devs.min, devs.YOA)




# Merge combos and Claim.maxdev
combos <- merge(combos, devs.bound)




# Delete all combos where dev is greater than max.dev
combos <- combos[combos$dev <= combos$dev.max, ]




# Merge combos and Claims
Claims.all <- merge(
  x = combos, 
  y = Claims, 
  all.x = TRUE
)




# Set missing values 
#   Amounts to zero
#   Category to Open 4
Claims.all$PeriodEnding <- NULL
Claims.all$X <- NULL

Claims.all$ClaimSeverityInd[is.na(Claims.all$ClaimSeverityInd)] <- 4
Claims.all$ClaimStatus[is.na(Claims.all$ClaimStatus)] <- 'O'

Claims.all$IncurredUSD[is.na(Claims.all$IncurredUSD)] <- 0
Claims.all$MostlikelyUSD[is.na(Claims.all$MostlikelyUSD)] <- 0
Claims.all$BlendUSD[is.na(Claims.all$BlendUSD)] <- 0
Claims.all$PessimisticUSD[is.na(Claims.all$PessimisticUSD)] <- 0

##########################################################
#
# END Data Manipulation 
#
########################################################## 




##########################################################
#
# Transition Matrices Construction
# (This code constructs transition matrices. 
#   Those matrices are different from transition matrices 
#   in Stochastic process since they are just transpose of them)
#
########################################################## 


library(lattice)
library(grid)
library(gridExtra)
library(Matrix)
ptm<-proc.time()
whole.matrix<-matrix(rep(0,8*8*40),nrow=8,ncol=8*40)
n=length(dev)
TranM<-list()
for(j in 1:(max(dev)-1)){
  TM<-matrix(rep(0,64),nrow=8,ncol=8)
for(i in 1:(n-1))
{
  if(dev[i]==j&dev[i+1]==j+1){
  for(h in 1:4){
    for(k in 1:2){
      for(z in 1:4){
        for(m in 1:2){
          if(ClaimStatus[i]==k&ClaimSeverityInd[i]==h&ClaimStatus[i+1]==m&ClaimSeverityInd[i+1]==z)
            TM[(k-1)*4+z,(m-1)*4+h]=TM[(k-1)*4+z,(m-1)*4+h]+1
        }
      }
      
    }
  }   
  }  
}
  whole.matrix[,((j-1)*8+1):(8*j)]<-TM
  TranM[[j]]<-TM
}

diag<-matrix(Diagonal(8),nrow=8,ncol=8)
whole.matrix[,313:320]<-diag
TranM[[40]]<-diag

for(i in 1:((max(dev)-1)*8))
{
  if(sum(whole.matrix[,i])!=0){
  whole.matrix[,i]=whole.matrix[,i]/sum(whole.matrix[,i])
  }
  else if(sum(whole.matrix[,i])==0&&i%%8!=0)
  {
    whole.matrix[i%%8,i]=1
  }
  else if(sum(whole.matrix[,i])==0&&i%%8==0)
  {
    whole.matrix[8,i]=1
  }
}

##########################################################
#
# END Transition Matrices Construction
#
########################################################## 


##########################################################
#
# Illustrative Representation of Transition Matrices
# (Heat map like graphical representation is obtained)
#
########################################################## 


rgb.palette <- colorRampPalette(c("yellow", "red"), space = "rgb")
x<-list()
for (i in 1:(max(dev)-1)){
  a=(i-1)*8+1
  b=i*8
  x[[i]]<-levelplot(t(whole.matrix[,a:b]),main = paste0("plot",i),xlab="", ylab="", col.regions=rgb.palette(120), cuts=64, at=seq(0,1,0.125))
}
x[[40]]<-levelplot(t(whole.matrix[,313:320]),main = paste0("plot",40),xlab="", ylab="", col.regions=rgb.palette(120), cuts=64, at=seq(0,1,0.125))

for (j in 1:4){
  grid.arrange(x[[9*(j-1)+1]],x[[9*(j-1)+2]],x[[9*(j-1)+3]],x[[9*(j-1)+4]],x[[9*(j-1)+5]],
               x[[9*(j-1)+6]],x[[9*(j-1)+7]],x[[9*(j-1)+8]],x[[9*(j-1)+9]],
               nrow=3,ncol=3)  
}
grid.arrange(x[[37]],x[[38]],x[[39]],x[[40]],
            nrow=3,ncol=3)

Convergence discusses in the form of a sequence of numbers
for(i in 1:(max(dev)-1)){
  diff[i]<-sum(abs(diag-whole.matrix[,(8*(i-1)+1):(8*i)]))
}
diff[40]<-0

plot(1:40,diff)

Distributions
dist.M<-diag
  for(j in 1:(max(dev)-1)){
    dist.M<-dist.M%*%whole.matrix[,(8*(j-1)+1):(8*j)]
  }

##########################################################
#
# END Illustrative Representation of Transition Matrices
#
########################################################## 


##########################################################
#
# The Histogram of first periods of all claims
#
########################################################## 

divider<-fordivider2[,2] # it containes the number of records for each claim. It can help us separate different claims.
number.Claims<-max(ClaimId) #total number of different claims
ClaimStatus<-as.integer(ClaimStatus)
for(i in 2:number.Claims)
{
  divider[i]=divider[i]+divider[i-1]
} 
first.period<-c(Claims$dev[1])
for(i in 1:(number.Claims-1)){
  
  first.period=c(first.period,Claims$dev[divider[i]+1])
}
hist(first.period)

Severity and Loss Inconsistency
Claims<-read.csv("C:/Users/Qiao/Desktop/Claims for R.csv") #read the data
attach(Claims)
ClaimSeverityInd=Claims$ClaimSeverityInd
BlendUSD=Claims$BlendUSD
n=length(ClaimSeverityInd)
rm(v)
stat<-list()
for (m in 1:4){
  
for(i in 1:n){
  if(ClaimSeverityInd[i]==m){
    v<-c(BlendUSD[i])
    s=i
    break
  } 
}

for(j in (s+1):n){
  if(ClaimSeverityInd[j]==m){
    v<-c(v,BlendUSD[j])
}
}
stat[[m]]<-v
}

summa<-data.frame(category=c("1","2","3"),
                  logmean=c(mean(stat[[1]]),mean(stat[[2]]),mean(stat[[3]])),
                  logstd=c(sd(stat[[1]]),sd(stat[[2]]),sd(stat[[3]])))
logsumma<-data.frame(category=c("1","2","3"),
                     normmean=c(log(mean(stat[[1]])/(sd(stat[[1]])/(mean(stat[[1]]))^2+1)^0.5),
                                log(mean(stat[[2]])/(sd(stat[[2]])/(mean(stat[[2]]))^2+1)^0.5),
                                log(mean(stat[[3]])/(sd(stat[[3]])/(mean(stat[[3]]))^2+1)^0.5)),
                     normstd=c((log(sd(stat[[1]])/(mean(stat[[1]]))^2+1))^0.5,
                               (log(sd(stat[[2]])/(mean(stat[[2]]))^2+1))^0.5,
                               (log(sd(stat[[3]])/(mean(stat[[3]]))^2+1))^0.5))
simulation<-list()
for(i in 1:3){
simulation[[i]]<-rlnorm(1000, meanlog = logsumma[i,2], sdlog = logsumma[i,3])
}


##########################################################
#
# END The Histogram of first periods of all claims
#
########################################################## 


##########################################################
#
# Park’s script
#
# Date:   February 24, 2013
# Description:
#
# Shows how to construct a transition matrix
########################################################## 


# This command loads the data from a CSV file to a dataframe
claims <- read.csv('C:/Users/park/Desktop/Integrated_Project/Claims.csv', stringsAsFactors = FALSE)

# Add ClaimStage to reflect the claim status
claims$ClaimStage <- with(claims, ifelse(ClaimStatus == "C", ClaimSeverityInd + 4, 
                                         ifelse(ClaimStatus == "O", ClaimSeverityInd, NA)))

# empty list to start with
TransMatrixList <- list()

# Count the number of Transition Matrix
TransNum <- min(claims$dev):(max(claims$dev)-1)

for(i in seq(along=TransNum)){
# Select Dev = i or i+1
  SelDev <- (claims$dev == i | claims$dev == i+1)
  
# Select Matrix for Dev = i or i+1
  Matrix <- claims[SelDev,]
  Matrix <- Matrix[order(Matrix$ClaimId, Matrix$dev),]
# Construct Transition Matrix
  TransMatrix = matrix(rep(c(0), each = 64), nrow = 8, ncol = 8)
  for (j in 1:(nrow(Matrix)-1)){
    if (Matrix$ClaimId[j] == Matrix$ClaimId[j+1]) {
      From <- Matrix$ClaimStage[j]
      To <- Matrix$ClaimStage[j+1]
      TransMatrix[From, To] <- TransMatrix[From, To] + 1}
    
      j <- j + 1
  }
# Calculate the rate
  TransMatrix <- signif(TransMatrix/rowSums(TransMatrix,na.rm = FALSE, dims = 1),3)
# Convert Nan to 0
  TransMatrix[is.nan(TransMatrix)] <- 0
  TransMatrix <- t(TransMatrix)
# Add to the list
  TransMatrixList <- c(TransMatrixList, list(TransMatrix))
}
TransMatrixList

##########################################################
#
# END Park’s script
#
########################################################## 
