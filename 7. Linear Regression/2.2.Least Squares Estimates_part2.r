set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)
mean_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)
cc <- cor(female_heights$daughter, female_heights$mother)

slope <- cc*(sd_mother/sd_daughter) 
intercept <- mean_mother - cc*(sd_mother/sd_daughter)*mean_daughter 

predict(lm(mother~daughter, data = female_heights))[1]
female_heights$mother[1]

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99 <- Batting %>% filter(yearID %in% c(1999:2001)) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) 
  
bat_99 %>%
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  filter(mean_singles > 0.2)

bat_99 %>%
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  filter(mean_bb > 0.2)

new_bat_99 <- bat_99 %>%
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

joined_bat <- inner_join(bat_02,new_bat_99)
cor_singles <- cor(joined_bat$singles,joined_bat$mean_singles)
cor_bb <- cor(joined_bat$bb,joined_bat$mean_bb)
  
joined_bat %>% ggplot(aes(mean_singles, singles)) + geom_point()
joined_bat %>% ggplot(aes(mean_bb, bb)) + geom_point()

lm(singles ~ mean_singles, data = joined_bat)
lm(bb ~ mean_bb, data = joined_bat)