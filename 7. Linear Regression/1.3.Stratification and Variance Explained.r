set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
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
cc <- cor(female_heights$mother, female_heights$daughter)

slope <- cc*(sd_daughter/sd_mother) #0.339
intercept <- mean_daughter - cc*(sd_daughter/sd_mother)*mean_mother #42.517

cc^2*100 #10.53

# Y = a +bX + e

intercept + slope*60
