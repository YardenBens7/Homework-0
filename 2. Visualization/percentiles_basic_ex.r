# Load the heights dataset from the dslabs package:
library(dslabs)
data(heights)

# Use summary() on the heights$height variable 
# to find the quartiles
summary(heights$height)

# Find the percentiles of heights$height:
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)

# Confirm that the 25th and 75th percentiles 
# match the 1st and 3rd quartiles
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)