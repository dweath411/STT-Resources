library(ggplot2)
library(tidyverse)
cars_multi <- read_csv(file = "cars_multi.csv")

#basic histogram
ggplot(data = cars_multi, aes(x = mpg)) + geom_histogram()

#a little better histogram
ggplot(data = cars_multi, aes(x = mpg)) + geom_histogram(fill = "blue")

#a histogram with density plot
ggplot(data = cars_multi, aes(x = mpg)) + geom_histogram(fill = "blue", aes(y = ..density..)) + geom_density()

#a histogram with colored by origin
ggplot(data = cars_multi, aes(x = mpg, fill = as.character(origin))) + geom_histogram()

# a histogram with faceting
ggplot(data = cars_multi, aes(y = mpg, fill = as.character(origin))) + geom_histogram() + facet_grid(origin ~.)

# a box and whisker plot
ggplot(data = cars_multi, aes(x = mpg)) + geom_boxplot() + labs(title = "Box and Whisker Plot of Gas Mileage")

# a box and whisker plot split by cylinders 
ggplot(data = cars_multi, aes(y = mpg, fill = as.character(cylinders))) + geom_boxplot()

# simple scatterplot
ggplot(data = cars_multi, aes(x = mpg, y = weight)) + geom_point()

# scatterplot with labels
ggplot(data = cars_multi, aes(x = weight, y = mpg)) + geom_point() + labs(x = "Car Weight", y = "miles per gallon", title = "Gas Mileage vs. Car Weight")

# simple scatterplot, with colors 
ggplot(data = cars_multi, aes(x = weight, y = acceleration, color = cylinders)) + geom_point()

# scatterplot, with local trendline 
ggplot(data = cars_multi, aes(x = weight, y = acceleration, color = cylinders)) + geom_point() +geom_smooth()
