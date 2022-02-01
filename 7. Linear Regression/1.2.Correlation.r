install.packages("lahman")
library(Lahman)
library(dplyr)

Teams %>% filter(yearID %in% c(1961:2001)) %>% summarize(r = cor(R/G, AB/G))

Teams %>% filter(yearID %in% c(1961:2001)) %>% summarize(r = cor(W/G, E/G))

Teams %>% filter(yearID %in% c(1961:2001)) %>% summarize(r = cor(X2B/G, X3B/G))
