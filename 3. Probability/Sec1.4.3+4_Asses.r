library(tidyverse)

# number of groups in study: 88
nrow(esoph)

# number of total cases: 200
all_cases <- sum(esoph$ncases)

# number of total controls: 975
all_controls <- sum(esoph$ncontrols)

# probability that a subject in the highest alcohol consumption group is a cancer case
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)


esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# probability that they smoke 10g or more a day
esoph  %>% filter(tobgp != "0-9g/day") %>% summarize(sum(ncases)) / 200

# probability that they smoke 10g or more a day
esoph  %>% filter(tobgp != "0-9g/day") %>% summarize(sum(ncontrols)) / 975


esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncases)) 
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum(ncases)) / all_cases

esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncontrols)) 

esoph %>% filter(alcgp == "120+") %>% summarize(sum(ncases))
esoph %>% filter(tobgp == "30+") %>% summarize(sum(ncontrols)) / all_controls 

esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(sum(ncontrols)) / all_controls 
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>% summarize(sum(ncontrols)) / all_controls 
