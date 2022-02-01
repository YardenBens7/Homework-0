library(dslabs)
data("research_funding_rates")
research_funding_rates

two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)

two_by_two[2,3] / sum(two_by_two[1,3], two_by_two[2,3]) * 100
two_by_two[2,2] / sum(two_by_two[1,2], two_by_two[2,2])*100

two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy()

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat