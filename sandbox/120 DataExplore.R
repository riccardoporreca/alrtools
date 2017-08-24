Load that packages needed
library(actuar)
library(fitdistrplus)
library(ChainLadder)
library(RODBC)
library(tweedie)
library(mondate)


# Preliminaries
#  I like to change my directory and save my workspace in that directory


2+2
4
x <- 2+2   ## ‘<-’ is R syntax for ‘=’ or assignment
x^2

# you can reassign a variable using that variable
x <- x+67
x
class(x)


#vectorized math
weight <- c(110, 180, 240)      ## three weights
height <- c(5.5, 6.1, 6.2)    ## three heights
bmi <- (weight*4.88)/height^2    ## divides element-wise
bmi
class(bmi)

#Data classes go beyond the atomic classes
w <- rnorm(100)
w
z <- w + rnorm(100)
fit <- lm(z ~ w) ## linear regression model
fit
plot(fit)
class(fit)


# You can also load files via ODBC

# Channel <- odbcConnect("SQL Server",dsn = "Client")

# Rdata <- sqlFetch(Channel, "dbo.Sample")

Rdata <- read.table(choose.files(),header=TRUE,sep=",")

# I always like to make a copy of my data so I don't have to reload when I mess up
CleanData <- Rdata
Rdata <- CleanData
#Rdata <- CleanData

# Check out the class
class(Rdata)

# Use summary to check data
summary(Rdata)
names(Rdata)

# Use brackets to examine elements
Rhead <- Rdata[1,]
Rhead
# 5th column of 49990 element
Rdata[49990,5]

table(Rdata$Make)

# Use attach to avoid typing table name
attach(Rdata)
table(Make)
detach(Rdata)

#write.table(Rdata, file = "sample2.csv", sep = ",", col.names = TRUE, row.names = FALSE,qmethod = "double")

x <- Rdata
table(x$President)
x$President <- gsub('BILL CLINTON','BARACK OBAMA',x$President)
table(x$President)

#Changes negative claims to 0
Rdata$Claims1 <- ifelse(Rdata$Claims1<0,0,Rdata$Claims1) 
Rdata$Claims2 <- ifelse(Rdata$Claims2<0,0,Rdata$Claims2) 
summary(Rdata)

#math

Ratio1 <- Rdata$Claims1/Rdata$Exposure1
summary(Ratio1)


Rdata1 <- Rdata[!(Rdata$Exposure1<0.05),]
Rdata2 <- Rdata[!(Rdata$Exposure2<0.05),]

Ratio1 <- Rdata1$Claims1/Rdata1$Exposure1
summary(Ratio1)
sum(Rdata1$Claims1)/sum(Rdata1$Exposure1)
Rdata1 <- cbind(Rdata1,Ratio1)
summary(Rdata1)

dim(Rdata)
dim(Rdata1)
dim(Rdata2)


# Coercion is critical
names(Rdata)
class(Rdata$Deductible)

DeductibleChar <- as.character(Rdata$Deductible)
class(Rdata$DeductibleChar)



class(Rdata$Rev_Class)
table(Rdata$Rev_Class)
# models will coerce to a character but mixed variable may give errors
Rdata$RevClass <- as.character(Rdata$RevClass)
ValAgeChar <- as.character(Rdata$ValAge)

Rdata <- cbind(Rdata,DeductibleChar,ValAgeChar)

#creating indicator Variables

RowCount <- nrow(Rdata)
Vector <- rep(0,RowCount)

#PurchPurchase Bands
PurchaseBand0<-Vector
PurchaseBand1<-Vector
PurchaseBand12<-Vector
PurchaseBand24<-Vector
PurchaseBand36<-Vector
PurchaseBand50<-Vector
PurchaseBand60<-Vector
PurchaseBand80<-Vector
PurchaseBand100<-Vector

#ValAge Bands
VABand3<-Vector
VABand6<-Vector
VABand9<-Vector
VABand12<-Vector
VABand18<-Vector
VABand24<-Vector
VABand36<-Vector
VABand48<-Vector
VABand60<-Vector
VABand72<-Vector

Rdata <- cbind(Rdata,
PurchaseBand0,
PurchaseBand1,
PurchaseBand12,
PurchaseBand24,
PurchaseBand36,
PurchaseBand50,
PurchaseBand60,
PurchaseBand80,
PurchaseBand100,
VABand3,
VABand6,
VABand9,
VABand12,
VABand18,
VABand24,
VABand36,
VABand48,
VABand60,
VABand72)

names(Rdata)

Rdata$PurchaseBand0<-ifelse(Rdata$PurchPrice==0,1,0)
Rdata$PurchaseBand1<-ifelse(Rdata$PurchPrice==1,1,0)
Rdata$PurchaseBand12<-ifelse(Rdata$PurchPrice==12,1,0)
Rdata$PurchaseBand24<-ifelse(Rdata$PurchPrice==24,1,0)
Rdata$PurchaseBand36<-ifelse(Rdata$PurchPrice==36,1,0)
Rdata$PurchaseBand50<-ifelse(Rdata$PurchPrice==50,1,0)
Rdata$PurchaseBand60<-ifelse(Rdata$PurchPrice==60,1,0)
Rdata$PurchaseBand80<-ifelse(Rdata$PurchPrice==80,1,0)
Rdata$PurchaseBand100<-ifelse(Rdata$PurchPrice==100,1,0)

Rdata$ClassBand1<-ifelse(Rdata$Rev_Class==1,1,0)
Rdata$ClassBand2<-ifelse(Rdata$Rev_Class==2,1,0)
Rdata$ClassBand3<-ifelse(Rdata$Rev_Class==3,1,0)
Rdata$ClassBand4<-ifelse(Rdata$Rev_Class==4,1,0)
Rdata$ClassBand5<-ifelse(Rdata$Rev_Class==5,1,0)
Rdata$ClassBand6<-ifelse(Rdata$Rev_Class==6,1,0)
Rdata$ClassBand7<-ifelse(Rdata$Rev_Class==7,1,0)
Rdata$ClassBand8<-ifelse(Rdata$Rev_Class==8,1,0)
Rdata$ClassBand9<-ifelse(Rdata$Rev_Class==9,1,0)
Rdata$ClassBand10<-ifelse(Rdata$Rev_Class==10,1,0)
Rdata$ClassBandX<-ifelse(Rdata$Rev_Class=="X",1,0)

Rdata$VABand3<-ifelse(Rdata$ValAge==3,1,0)
Rdata$VABand6<-ifelse(Rdata$ValAge==6,1,0)
Rdata$VABand9<-ifelse(Rdata$ValAge==9,1,0)
Rdata$VABand12<-ifelse(Rdata$ValAge==12,1,0)
Rdata$VABand18<-ifelse(Rdata$ValAge==18,1,0)
Rdata$VABand24<-ifelse(Rdata$ValAge==24,1,0)
Rdata$VABand36<-ifelse(Rdata$ValAge==36,1,0)
Rdata$VABand48<-ifelse(Rdata$ValAge==48,1,0)
Rdata$VABand60<-ifelse(Rdata$ValAge==60,1,0)
Rdata$VABand72<-ifelse(Rdata$ValAge==72,1,0)


summary(Rdata)
