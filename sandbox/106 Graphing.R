#
# R Workshop
# 
#   Author: Adam L. Rich
#   Date:   March 11, 2013
#   Description:
#  
#     Graphing
#  
#   Copyright Notice:
#     THE FOLLOWING APPLIES TO THIS ENTIRE FILE
#       AND/OR ANY PORTION THEREOF
#     COPYRIGHT ADAM L. RICH, 2013
#     THIS HAS BEEN DISTRIBUTED AS PART OF THE 2013 CAS RPM SEMINAR
#     YOU CAN USE CODE HERE IN YOUR OWN WORK 
#       PROVIDED YOU DO NOT TAKE CREDIT FOR IT
#     YOU AGREE TO NOT POST THIS IN ANY WAY ON THE INTERNET
#     YOU CAN DISTRIBUTE TO THOSE YOU TRUST TO FOLLOW THESE RULES
#     THIS MESSAGE MUST STAY WITH THIS CODE
#     

setwd("P:/Desktop/R Workshop")
source('LoadFunctions.R')
require(ggplot2)


# Plots with two variables
#
#   plot(x, y)
#   plot(factor, y)
#   barplot(x)
#

# First, let's pick an interesting data set
data()


# Examine the iris data
pairs(iris)


# A basic scatterplot of x versus y
plot(iris$Petal.Width, iris$Petal.Length)


# Same thing, only using formula notation
plot(Petal.Length ~ Petal.Width, data = iris)
# Add a linear regression line
abline(lm(Petal.Length ~ Petal.Width, data = iris))



# Let's do the same thing, but a little more organized
#   and put the model through the origin
# Same thing, only using formula notation
form <- Petal.Length ~ Petal.Width - 1
model <- lm(form)

plot(form, data = iris)
abline(model)



# Add other stuff to the plot
plot(
  x = form, 
  data = iris, 
  main = "Iris petals, length against width",
  sub = 'Builtin R Data: iris',
  xlab = 'Width',
  ylab = 'Length',
  asp = 1
)

abline(model)






