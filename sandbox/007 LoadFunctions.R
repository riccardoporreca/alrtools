#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   December 5, 2014
#   Description:
#  
#     Load Functions
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

with(new.env(), {
  
  # Check to see if "Functions" is already installed
  if(!'Functions' %in% search()) {
    
    Functions <- new.env()
    with(Functions, {
      base::source('./Functions.R', local = TRUE)
    })
    attach(Functions)
  }
})

