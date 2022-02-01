# Start of copy paste
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
# end of copy paste

# Question 1
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

# Question 2
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% min()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max()
temp_carbon %>% filter((year==1751 | year==2014)) %>% pull(carbon_emissions)

# Question 3
temp_carbon %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% min()
temp_carbon %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% max()
temp_carbon %>% filter((year==1880 | year==2018)) %>% select(temp_anomaly)

# Question 4
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year,temp_anomaly)) + geom_point()
p + geom_hline(aes(yintercept = 0), col = "blue")

# Question 5
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year,temp_anomaly)) + geom_line()
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

# Question 7
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot() + geom_line(aes(year,temp_anomaly,col="Green",label="temp_anomaly")) + geom_line(aes(year,ocean_anomaly,col="Red", label="ocean_anomaly")) + geom_line(aes(year,land_anomaly,col="Blue", label="land_anomaly"))

# Start of copy paste
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
# end of copy paste

# Question 8
greenhouse_gases %>%
  ggplot(aes(x=year, y=concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# Question 10
temp_carbon %>% ggplot(aes(year, carbon_emissions)) + geom_line()

# Question 11
co2_time <- historic_co2 %>% ggplot(aes(year,co2,col=source)) + geom_line()

# Question 12
co2_time + xlim(-800000,-775000)
co2_time + xlim(-375000,-330000)
co2_time + xlim(-140000,-120000)
co2_time + xlim(-3000,2018)
