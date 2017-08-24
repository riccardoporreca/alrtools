# This script if for educational purposes and is NOT warranted for any purpose
# Sample data included with this script should not be disseminated

Load that packages needed
library(actuar)
library(fitdistrplus)
library(ChainLadder)
library(RODBC)
library(tweedie)
library(mondate)
library(plyr)

Rdata <- read.table("SampleData.csv",header=TRUE,sep=",",stringsAsFactors =FALSE)


# I always like to make a copy of my data so I don't have to reload when I mess up
CleanData <- Rdata
Rdata <- CleanData
#Rdata <- CleanData


# Loop through the columns and find out if there are any NAs
sapply(Rdata, function(x) any(is.na(x)))

# Clean up data

table(Rdata$SampleNo)
class(Rdata$SampleNo)
SampleNo <- as.character(Rdata$SampleNo)
table(SampleNo)
SampleNo <- ifelse(nchar(SampleNo)==0,0,SampleNo)
SampleNo <- ifelse(SampleNo=="A",0,SampleNo)
table(SampleNo)
SampleNo <- as.numeric(SampleNo)
summary(SampleNo)
table(Rdata$State)
table(Rdata$PurchPrice)
table(Rdata$Football)
class(Rdata$Football)
Rdata$Football <- as.character(Rdata$Football)
Football <- ifelse(nchar(Rdata$Football)==0,"NA",Rdata$Football)
table(Football)

table(Rdata$Make)
class(Rdata$Make)
Make <- as.character(Rdata$Make)
Make <- ifelse(nchar(Make)==0,"NA",Make)
table(Make)

table(Rdata$Rev_Class)
table(Rdata$Deductible)
table(Rdata$ValAge)
class(Rdata$ValAge)
ValAgeChar <- as.character(Rdata$ValAge)
class(ValAgeChar)

# Let's put the new variables in and take the old ones out

head(Rdata)

# Let's replace Column 1,4,5 and add ValAgeChar
a<- Rdata[,2:3]
b<- Rdata[,6:15]

Rdata <- cbind(SampleNo,a,Football,Make,b,ValAgeChar)
dim(Rdata)
summary(Rdata)

#write.table(Rdata, file = "sample2.csv", sep = ",", col.names = TRUE, row.names = FALSE,qmethod = "double")

#Changes negative claims to 0
Rdata$Claims1 <- ifelse(Rdata$Claims1<0,0,Rdata$Claims1) 
Rdata$Claims2 <- ifelse(Rdata$Claims2<0,0,Rdata$Claims2) 
summary(Rdata)

Rdata1 <- Rdata[!(Rdata$Exposure1<0.05),]
Rdata2 <- Rdata[!(Rdata$Exposure2<0.05),]

#math

Ratio1 <- Rdata1$Claims1/Rdata1$Exposure1
summary(Ratio1)
sum(Rdata1$Claims1)/sum(Rdata1$Exposure1)
Rdata1 <- cbind(Rdata1,Ratio1)
summary(Rdata1)

Ratio2 <- Rdata2$Claims2/Rdata2$Exposure2
summary(Ratio2)
sum(Rdata1$Claims2)/sum(Rdata1$Exposure2)
Rdata2 <- cbind(Rdata2,Ratio2)
summary(Rdata2)

dim(Rdata)
dim(Rdata1)
dim(Rdata2)


# Easy to build model when you have the names of the fields
temp <- names(Rdata1)
write.table(temp, file = "names.csv", sep = ",", col.names = TRUE, row.names = FALSE,qmethod = "double")

# let's try a tweedie model - this is a hybrid model of a Poisson
# and gamma - great for pure premiums!  



tweed  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Make+
Credential+
Rev_Class+
President+
Deductible+
ValAgeChar
, data = Rdata1
, family=tweedie(var.power=1.0, link.power=0)
, weights = Exposure1)

summary(tweed)

tweed  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Rev_Class+
President+
Deductible+
ValAgeChar
, data = Rdata1
, family=tweedie(var.power=1.0, link.power=0)
, weights = Exposure1)

summary(tweed)

tweed2  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Rev_Class+
President+
Deductible+
ValAge
, data = Rdata1
, family=tweedie(var.power=1.0, link.power=0)
, weights = Exposure1)

# change the power

tweed3  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Rev_Class+
President+
Deductible+
ValAgeChar
, data = Rdata1
, family=tweedie(var.power=1.2, link.power=0)
, weights = Exposure1)

tweed4  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Rev_Class+
President+
Deductible+
ValAgeChar
, data = Rdata1
, family=tweedie(var.power=1.6, link.power=0)
, weights = Exposure1)

summary(tweed4)
summary(tweed)

# let's look at predictions!

pre <- exp(predict(tweed))
pre2 <- Rdata1$Exposure1
sum(pre*pre2)
sum(Rdata1$Claims1)

pre <- exp(predict(tweed4))
pre2 <- Rdata1$Exposure1
sum(pre*pre2)
sum(Rdata1$Claims1)

# the gold standard - hold out data tested on model data

Rdata1train <- Rdata1[(Rdata1$SampleNo<8),]
Rdata1test <- Rdata1[(Rdata1$SampleNo>=8),]

tweed16  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Rev_Class+
President+
Deductible+
ValAgeChar
, data = Rdata1train
, family=tweedie(var.power=1.6, link.power=0)
, weights = Exposure1)

tweed10  <-  glm(Ratio1 ~
State+
PurchPrice+
Football+
Rev_Class+
President+
Deductible+
ValAgeChar
, data = Rdata1train
, family=tweedie(var.power=1.0, link.power=0)
, weights = Exposure1)

pre <- exp(predict(tweed10,Rdata1test))
pre2 <- Rdata1test$Exposure1
sum(pre*pre2)
sum(Rdata1test$Claims1)

pre <- exp(predict(tweed16,Rdata1test))
pre2 <- Rdata1test$Exposure1
sum(pre*pre2)
sum(Rdata1test$Claims1)


#Calculating Lift

# calculate the measures 
a <- as.data.frame(exp(predict(tweed16,Rdata1test)))
b <- as.data.frame(Rdata1test$Exposure1)
c <- as.data.frame(pre*pre2)
d <- as.data.frame(Rdata1test$Claims1)

# Form a data frame, sort it by PredLC, add an index
lift <- cbind(a,b,c,d)
head(lift)
names(lift) <- c("PredLC","Exp","PredLoss","Claims")
lift<-lift[order(lift$PredLC),]
head(lift)
indexlift<-c(1:nrow(lift))
lift <- cbind(lift,indexlift)
names(lift) <- c("PredLC","Exp","PredLoss","Claims","ObsNo")
dim(lift)
#create cumulative exposures

b <- 1
CumExp <- lift$Exp[1]
b <- b+1
while (b <= nrow(lift)) {
CumExp[b] <- CumExp[b-1] + lift$Exp[b]
b <- b+1}

head(CumExp)
CumExp[nrow(lift)]
sum(lift$Exp)
names(CumExp) <- c("CumExp")
cleanlift <- lift


#specify levels
lev_lift<-10
lift <- cleanlift
lev_break <- sum(lift$Exp)/lev_lift
lev_break
bucket <- ceiling(CumExp/lev_break)
table(bucket)
bucket <- ifelse(bucket>lev_lift,lev_lift,bucket)
table(bucket)
names(bucket) <- c("bucket")
lift <- cbind(lift,CumExp,bucket)
head(lift)

# Use plyr package
library(plyr)
liftsum<- ddply(lift,.(bucket),summarise,
	N = length(bucket),
     PredLoss=sum(PredLoss),
	ActualClaim=sum(Claims),
      Exposure=sum(Exp))
liftsum
rawpred <- sum(lift$Claims)/sum(lift$Exp)
rawpred1<-rawpred*liftsum$Exposure
names(rawpred1)<-c("MeanPred")
liftsum<-cbind(liftsum,rawpred1)
liftsum


summary(tweed)
summary(tweed2)
summary(tweed3)
summary(tweed4)

plot(tweed)

table(Rdata1$Football)





#creating indicator Variables - for reference.  For an exercise, you should
# create these variables, bind them to the data, and then build the model
# over these variables.  You can then remove the nonsignifcant ones individually

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
