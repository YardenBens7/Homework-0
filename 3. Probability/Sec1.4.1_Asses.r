library("gtools")
library("tidyverse")

# different ways for 3 medals across 8 people
medals <- permutations(8,3)
nrow(medals)

# different ways for 3 medals across 3 jamaican people
jamaica < permutations(3,3)
nrow(jamaica)

# 3 medals all to jamaican
nrow(jamaica)/nrow(medals)
# == 3/8*2/7*1/6

# Monte Carlo simulation to choosing all jamaican runners
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B=10000
all_j <- c("Jamaica", "Jamaica", "Jamaica")

results <-
  replicate(B,{
  chosen <- sample(runners,3)
  all(chosen==all_j)
  })

mean(results==TRUE)
