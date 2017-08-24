#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   December 5, 2014
#   Description:
#
#     Writing Functions
#   
#   Adapted from content found at
#     http://opensourcesoftware.casact.org/chain-ladder
#
#   Copyright Notice:
#     THE FOLLOWING APPLIES TO THIS ENTIRE FILE
#       AND/OR ANY PORTION THEREOF
#     COPYRIGHT ADAM L. RICH, 2014
#     THIS HAS BEEN DISTRIBUTED AS PART OF THE 2014 CAS LIMITED ATTENDANCE SEMINAR: INTRO TO R
#     YOU CAN USE CODE HERE IN YOUR OWN WORK 
#       PROVIDED YOU DO NOT TAKE CREDIT FOR IT
#     YOU AGREE TO NOT POST THIS IN ANY WAY ON THE INTERNET
#     YOU CAN DISTRIBUTE TO THOSE YOU TRUST TO FOLLOW THESE RULES
#     THIS MESSAGE MUST STAY WITH THIS CODE
#     
#


# Setup
setwd('P:/Desktop/LAS Orlando/')
source('./LoadFunctions.R')




# Your very first function
ident <- function(x) {x}



# Could also write
ident <- function(x) {
  x
}



# Or be explicit about the return statement
# return() is implied on the last statement of the function
ident <- function(x) {
  return(x)
}



# Function to get diagonal of a square matrix
diagonal <- function(m, id = 1){
  stopifnot(nrow(m) == ncol(m))
  if(id == 1) return(m[(1:nrow(m) - 1) * (nrow(m) + 1) + 1])
  if(id == 2) return(m[(1:nrow(m) - 1) * (nrow(m) - 1) + nrow(m)])
}















