library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>% 
  mutate(R_per_game = R/G) %>% 
  lm(avg_attendance ~ R_per_game, data = .) %>% 
  .$coef %>%
  .[2]

Teams_small %>% 
  mutate(R_per_game = HR/G) %>% 
  lm(avg_attendance ~ R_per_game, data = .) %>% 
  .$coef %>%
  .[2]

lm(avg_attendance ~ W, data = Teams_small)

lm(avg_attendance ~ yearID, data = Teams_small)

cor(Teams_small$R/Teams_small$G, Teams_small$W)
cor(Teams_small$HR/Teams_small$G, Teams_small$W)

dat <- Teams_small %>% mutate(W_Strata = round(W/10)) %>%
  filter(W_Strata >= 5 & W_Strata <= 10)

sum(dat$W_Strata == 8)

dat %>% group_by(W_Strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance/sd(R/G)))

dat %>% group_by(W_Strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance/sd(HR/G)))

new_teams_small <- Teams_small %>% mutate(Runs_per_game = R/G, Homeruns_per_game = HR/G) %>%
  lm(avg_attendance ~ Runs_per_game + Homeruns_per_game + W + yearID, data = .) %>%
  tidy()

fit <- Teams_small %>% 
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)


newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(fit,newdata)
cor(preds, newdata$avg_attendance)

