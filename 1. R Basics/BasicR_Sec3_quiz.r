library(dslabs)
library(dplyr)
data(heights)
options(digits = 3)    # report 3 significant digits

# How many individuals in the dataset are above average height? #
avg <- mean(heights$height)
ind <- which(heights$height > avg)
length(ind)

# How many individuals in the dataset are above average height and are female? #
length(which(heights$height > avg & heights$sex=="Female"))

# What proportion of individuals in the dataset are female? #
mean(heights$sex=="Female")

# Determine the minimum height in the heights dataset. #
which.min(heights$height)
heights$height[1032]

# Use the match() function to determine the index of the first individual with the minimum height. #
match(50, heights$height)

# Determine the maximum height. #
which.max(heights$height)
heights$height[1017]

# How many of the integers in x are NOT heights in the dataset? #
x <- 50:82
sum(!(x %in% heights$height))

# Using the heights dataset, create a new column of heights in centimeters named ht_cm. Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2. #
heights2 <- mutate(heights, ht_cm = height*2.54)

# What is the height in centimeters of the 18th individual (index 18)? #
heights2$ht_cm[18]

# What is the mean height in centimeters? #
mean(heights2$ht_cm)

# How many females are in the heights2 dataset? #
sum(heights2$sex=="Female")

# What is the mean height of the females in centimeters? #
ind2 <- which(heights2$sex=="Female")
mean(heights2$ht_cm[ind2])


library(dslabs)
data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region, data=olive)

