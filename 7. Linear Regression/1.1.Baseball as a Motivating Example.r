library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples_rate = X3B / G, doubles_per_game = X2B / G) %>%
  ggplot(aes(triples_rate, doubles_per_game)) + 
  geom_point(alpha = 0.5)
