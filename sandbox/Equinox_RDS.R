########################Equinox RDS model#############################
#September 2012
#Ed Tredger
#This model produces a distribution of outcomes based on the input PD, LGD and limits 
#There are two levels to the business with multiple buyers relating to a single policy
#There are limits at both levels, which need to be applied
#Argument:
#For a given policy, find the buyers associated and simulate the losses 
#Gross buyer losses are based on the PoD and then the LGD (use a Beta distribution)
#Cap the losses at the Credit limit for each buyer's loss
#Accumulate the buyer losses and apply the Policy Limit and Deductible
########################################
#Add date stamp for any new comments
########################################
library(stats)
library(Rlab)
############################Inputs###################################

#Number of simulations
n <- 100000

#Need to add in a way of 'stressing' industries one in 5 years
#Set up a binomial with 0.2 prob success for each industry
#For this simulation, increase PD by a set % e.g. 50% across this industry only
#Read in trade sector as a dummy for now
#Frequency industry shock
shock_freq <- 0.2
#Severity of industry shock
shock_sev <- 2

#From Excel
#Buyer_inputs
#Policy_data
#FOREX
######Policy data#######
#Policy_list
Policy_list_temp <- Policy_data[,3]
Policy_list <- Policy_list_temp[Policy_list_temp != ""]
#Start_dates
#End_dates
#AAD
AAD_temp <- Policy_data[,7]
AAD <- AAD_temp[AAD_temp != ""]
#Policy_limits
Policy_limits_temp <- Policy_data[,8]
Policy_limits <- Policy_limits_temp[Policy_limits_temp != ""]
#EPI
EPI_temp <- Policy_data[,9]
EPI <- as.numeric(EPI_temp[EPI_temp != ""])
#Nr_limits
######Buyer data#######
#Policy_indicator
Policy_indicator_temp <- Buyer_inputs[,1]
Policy_indicator <- Policy_indicator_temp[Policy_indicator_temp != ""]
#CCY
#FOREX rates
#Indemnity
#Agg_limit
#Using the Agg Net Credit Limits (USD)
CL_temp <- Buyer_inputs[,14]
CL <- CL_temp[CL_temp != ""]
#Using the Credit limit
#CL_temp <- Buyer_inputs[,8]
#CL <- CL_temp[CL_temp != ""]
#PD
PD_temp <- Buyer_inputs[,22]
PD <- PD_temp[PD_temp != ""]
#Read in trade sector
TS_temp <- Buyer_inputs[,21]
TS <- TS_temp[PD_temp != ""]
#LGD
LGD_temp <- Buyer_inputs[,23]
LGD <- LGD_temp[LGD_temp != ""]
#Indemnity
Indemnity_temp <- Buyer_inputs[,13]
Indemnity <- Indemnity_temp[LGD_temp != ""]

#To sum up Policy_losses
Equinox_loss <- rep(0,n)
#To save individual policy data#
Policy_outputs <- matrix(0, length(Policy_list), 5)
Policy_full <- matrix(0, length(Policy_list),n)
#####################################################################

#Read in the data and assign buyers to a policy
PD <- as.numeric(PD)*(1+Stress)

for(p in 2:length(Policy_list)) {
#for(p in 16:16) {

Policy <- Policy_list[p]
#Pull out the info for the buyers belonging to that particular policy
PD_buyers <- as.numeric(PD[Policy_indicator==Policy])

LGD_buyers <- as.numeric(LGD[Policy_indicator==Policy])

n_buyers <- length(PD_buyers)

#Policy details
Policy_limit <- as.numeric(Policy_limits[p])
Policy_AAD <- as.numeric(AAD[p])

#Cover_start <- 01/03/2012
#Cover_end <- 31/08/2013
#Adjust PD for the term covered?
Premium <- as.numeric(EPI[p])

#Buyer details (for each policy)
Indemnity_buyer <- as.numeric(Indemnity[Policy_indicator==Policy])
Credit_limit <- as.numeric(CL[Policy_indicator==Policy])*Indemnity_buyer

#Set up a dummy vector to store outputs
Defaults <- rep(0,n)
#########################################################################
######################TESTING############################################
#Need to increase the Defaults for the selected 'stressed' industries
#Trade sectors
#This is based on ALL trade sectors
list_TS <- unique(TS)
n_TS <- length(list_TS)
#Need to make sure Rlab package and libraries are installed
stress_TS <- list_TS[rbern(n_TS, shock_freq)==1]

#Vector to apply stress to the policies buyers
buyer_TS <- TS[Policy_indicator==Policy]
#Make a vector of the PD multipliers
stressed_TS <- unique(sort(match(stress_TS, buyer_TS)))
PD_buyers[stressed_TS] <- PD_buyers[stressed_TS]*shock_sev
##########################################################################
##########################################################################
Defaults <-  as.matrix(replicate(n,sapply(PD_buyers, function(x) sample(0:1, 1, prob=c(1-x, x)) ) ), n_buyers, n )
buyer_sev <- as.matrix(sapply(LGD_buyers, function(y) rbeta( n,y*2/(1-y),2) )  *Credit_limit, n_buyers, n)

b <- rep(0,n)
num <- rep(0,n)

#Apply the policy limit and excess
Policy_loss <- rep(0,n)

#################################
#Testing!
if(n_buyers > 1 ){loss <- Defaults*t(buyer_sev)} else {loss <- Defaults*buyer_sev}

#num[i] <- length(loss[loss>0])

#Sum losses to give Policy level loss prior to limits
if(n_buyers > 1 ){Policy_loss <- colSums(loss)} else {Policy_loss<- loss}

#Define a function to apply the layer terms
layer <- function(x) {pmax(0,pmin(x-Policy_AAD, Policy_limit))}

#Apply this layer function to all elements in a vector
Policy_loss <- layer(Policy_loss)

##################################

Policy_outputs[p-1,1] <- mean(Policy_loss)
Policy_outputs[p-1,2] <- quantile(Policy_loss, 0.9)
Policy_outputs[p-1,3] <- quantile(Policy_loss, 0.99)
Policy_outputs[p-1,4] <- quantile(Policy_loss, 0.995)
Policy_outputs[p-1,5] <- quantile(Policy_loss, 0.996)
Policy_full[p-1,] <- Policy_loss
Equinox_loss <- Equinox_loss+Policy_loss

}

#Also work out the contribution to 1 in 200
#Take all the losses >99.5th percentile from Equinox loss
#and attribute back to the policies which contributerow

loss200 <- colSums(Policy_full[,Equinox_loss>(quantile(Equinox_loss,0.995))])

large_losses <- Policy_full[,Equinox_loss>(quantile(Equinox_loss,0.995))]

contributions <- matrix(0,length(Policy_list)-1,(n/200))
policy_contributions <- rep(0, length(Policy_list)-1)

for(l in 2:length(Policy_list)){
	contributions[l-1,] <- large_losses[l-1,]/loss200
	policy_contributions[l-1] <- mean(contributions[l-1,]) 
}

######Outputs######
#Policy_loss - full distribution
#num - number of Policy level claims

EL <- mean(Equinox_loss) 
EL10 <- quantile(Equinox_loss, 0.9)
EL100 <- quantile(Equinox_loss, 0.99)
EL200 <- quantile(Equinox_loss, 0.995)
EL250 <- quantile(Equinox_loss, 0.996)

Equinox <- matrix(0,1,5)
Equinox[,1] <- EL
Equinox[,2] <- EL10
Equinox[,3] <- EL100
Equinox[,4] <- EL200
Equinox[,5] <- EL250

ULR <- Equinox_loss/sum(EPI[2:length(Policy_list)])
x11()
plot(density(ULR), xlab="Loss ratio", ylab="Density", main="Distribution of Equinox losses", col="blue")

#To Do 
#How should we adjust for Cover period and cover basis?
#LGD distribution - beta variance parameter - or shall we keep it fixed?
#LGD data - need to vary by buyer!
#Rather than correlations, which are hard to specify
#Instead we could stress up the PD by 50%
#Testing

#Define a correlation matrix of size n with 1 along the diagonal
#rho <- 0.1
#R<- matrix( c(1,rep(c(rep(rho,n),1),n-1)),n,n)
#Apply a Cholesky decomposition
#ch<-chol(R)
#Input data
#data <- matrix(0,length(Policy_list-1),n)

#uncor <- Policy_full[2:length(Policy_list),]

#Something seems not to work at this point###############
#cor_data<-t(ch)%*%t(uncor)

#Add up across the correlated Policy losses to give correlated Equinox losses

#Equinox_cor <- rowSums(cor_data)/sum(EPI[2:length(Policy_list)])
#lines(density(Equinox_cor), col="red")






