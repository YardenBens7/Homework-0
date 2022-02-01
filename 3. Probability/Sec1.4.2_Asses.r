library("gtools")
library("tidyverse")

# CURRENT OPTIONS
# 1 of 6 entrees
# 2 of 6 sides
# 1 of 2 drinks

# how many meal combinations right now
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))

# how many options if 3 drinks instead of 2
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(3,1))

# if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6
nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))

# minimum number of entries to pass 365 options
how_many_options <- function (k) {
  nrow(combinations(k,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
}
combos <- sapply(c(1:12), how_many_options)
data.frame(entrees = 1:12, combos = combos)

# minimum number of sides to pass 365 options
how_many_optionss <- function (k) {
  nrow(combinations(6,1))*nrow(combinations(k,2))*nrow(combinations(3,1))
}
comboss <- sapply(c(2:12), how_many_optionss)
data.frame(sides = 2:12, comboss = comboss)