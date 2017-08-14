# Author: Adam L. Rich
# Date:   June 26, 2015
# Description
#
#   ggplot2 notes
#



# What is the function?
ggplot


# Gives the methods for the generic function
methods(ggplot)



# How to see a function that is not exported
ggplot2:::ggplot.data.frame



# Get a sample ggplot to play with
df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)




# Compute sample mean and standard deviation in each group
library(plyr)
ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))




p1 <- ggplot(df, aes(x = gp, y = y))
  

p2 <- ggplot(df, aes(x = gp, y = y)) +
  geom_point()


p3 <- ggplot(df, aes(x = gp, y = y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean),
             colour = 'red', size = 3)



str(p1)



