## Import data sets and murders specifically
library(dslabs)
data("murders")
str(murders)
head(murders)

## Select column from data set
murders$population

## Show categories in murders (here it's regions)
levels(murders$region)