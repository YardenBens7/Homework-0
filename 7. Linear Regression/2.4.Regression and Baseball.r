fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

2/PA + 4 + 2 + 0 + 4 = 2/PA +10
1/PA + 6 + 4 + 3 = 1/PA + 13

Teams %>% 
  filter(yearID == 1971) %>% 
  lm(R ~ BB + HR, data = .) %>%
  tidy()

Teams %>%
  filter (yearID == 1971) %>%
  mutate(BB = BB/G, HR = HR/G, R=R/G) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy(conf.int = TRUE)

Teams %>%
  filter (yearID %in% c(1961:2018)) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>% 
  ungroup() %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) + geom_point() + geom_smooth(method = "lm")

Teams %>%
  filter (yearID %in% c(1961:2018)) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>% 
  ungroup() %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy()
  