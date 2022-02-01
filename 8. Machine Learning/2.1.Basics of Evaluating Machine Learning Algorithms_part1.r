library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

female_class <- dat %>% filter(sex == "Female" & type == "inclass") %>% summarize(n())
class_total <- dat %>% filter(type == "inclass") %>% summarize(n())
female_class/class_total #0.6666667
female_online <- dat %>% filter(sex == "Female" & type == "online") %>% summarize(n())
online_total <- dat %>% filter(type == "online") %>% summarize(n())
female_online/online_total #0.3783784

y_hat <- ifelse(x == "online", "Male", "Female") %>%
  factor(levels = levels(y))
mean(y_hat==y)

table(y_hat,y)
sensitivity(table(y_hat,y))
specificity(table(y_hat,y))
dat %>% filter(sex=="Female") %>% summarize(n()) / nrow(dat)
