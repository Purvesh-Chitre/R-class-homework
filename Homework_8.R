######################################################
##################### HOMEWORK 8 #####################
######################################################

# loading mtcars dataset
mtcars

# defining the scatter plot using plot()

plot(x = mtcars$mpg, y = mtcars$hp, type = "p")

# switching over to ggplot2() environment
install.packages("ggplot2")

# loading the ggplot2 library
library(ggplot2)

# using ggplot() to visualize relationship
ggplot(data = mtcars, aes(x = mpg, y = hp)) +
  geom_point(aes(size = gear)) + 
  geom_smooth()

# using color for visualization of the ggplot()
ggplot(data = mtcars, aes(x = mpg, y = hp)) +
  geom_point(aes(size = gear, color = factor(gear))) + 
  geom_smooth()

