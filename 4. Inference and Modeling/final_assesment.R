# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

##################################### 1 ########################################
# What is the expected total number of voters in the sample choosing "Remain"?
N*p
# What is the standard error of the total number of voters in the sample choosing "Remain"?
sqrt(N*p*(1-p))
# What is the expected value of , the proportion of "Remain" voters?
p
# What is the standard error of , the proportion of "Remain" voters?
sqrt(p*(1-p)/N)
# What is the expected value of , the spread between the proportion of "Remain" voters and "Leave" voters?
2*p - 1
# What is the standard error of , the spread between the proportion of "Remain" voters and "Leave" voters?
2*sqrt(p*(1-p)/N)

##################################### 2 ########################################
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/ 2)
# hat is the average of the observed spreads (spread)?
mean(brexit_polls$spread)
# What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)
# What is the average of x_hat, the estimates of the parameter ?
mean(brexit_polls$x_hat)
# What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)

##################################### 3 ########################################
you_gov <- brexit_polls[1,]
gov_x_hat <- 0.52
gov_sampleN <- 4772
# What is the lower bound of the 95% confidence interval?
gov_x_hat - qnorm(0.975)*sqrt(gov_x_hat*(1 - gov_x_hat) / gov_sampleN)
# What is the upper bound of the 95% confidence interval?
gov_x_hat + qnorm(0.975)*sqrt(gov_x_hat*(1 - gov_x_hat) / gov_sampleN)
# Does the 95% confidence interval predict a winner (does not cover )? Does the 95% confidence interval cover the true value of  observed during the referendum?
# The interval predicts a winner but does not cover the true value of p.

##################################### 4 ########################################
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01")

june_polls <- june_polls %>% 
  mutate(se_x_hat = sqrt(x_hat*(1 - x_hat)/ samplesize),
         se_spread = 2*se_x_hat,
         lower = spread - qnorm(0.975) * se_spread,
         upper = spread + qnorm(0.975) * se_spread,
         hit = lower < -0.038 & upper > -0.038)

# How many polls are in june_polls?
32
# What proportion of polls have a confidence interval that covers the value 0?
mean(june_polls$lower < 0 & june_polls$upper > 0)
# What proportion of polls predict "Remain" (confidence interval entirely above 0)?
mean(june_polls$lower > 0 & june_polls$upper > 0)
# What proportion of polls have a confidence interval covering the true value of d?
mean(june_polls$lower < -0.038 & june_polls$upper > -0.038)

##################################### 5 ########################################
june_polls %>% group_by(pollster) %>% summarize(pollster, n(), hit)
# The results are consistent with a large general bias that affects all pollsters.

##################################### 6 ########################################
ggplot(data = june_polls, aes(poll_type, spread)) + geom_boxplot()
# Telephone polls tend to show support "Remain" (spread > 0).
# Telephone polls tend to show higher support for "Remain" than online polls (higher spread).
# Online polls have a larger interquartile range (IQR) for the spread than telephone polls, indicating that they are more variable.
# Poll type introduces a bias that affects poll results.

##################################### 7 ########################################
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

# What is the lower bound of the 95% confidence interval for online voters?
combined_by_type %>% mutate(lower = spread - qnorm(0.975)*2*sqrt(p_hat*(1 - p_hat) / N))
# hat is the upper bound of the 95% confidence interval for online voters?
combined_by_type %>% mutate(upper = spread + qnorm(0.975)*2*sqrt(p_hat*(1 - p_hat) / N))

##################################### 8 ########################################
# Neither set of combined polls makes a prediction about the outcome of the Brexit referendum (a prediction is possible if a confidence interval does not cover 0). 
# The confidence interval for telephone polls is covers more positive values than the confidence interval for online polls.
# Neither confidence interval covers the true value of d = -0.038.

##################################### 9 ########################################
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value

# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05

##################################### 10 #######################################
# convert to data frame
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds

phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]

phone_odds <- phone_true/phone_false
phone_odds

online_odds/phone_odds

##################################### 11 #######################################
brexit_polls %>% ggplot(aes(enddate,spread,color=poll_type)) + geom_smooth(method="loess",span=0.4) + geom_point() + geom_hline(yintercept = -0.038)

##################################### 12 #######################################
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate,proportion,color=vote)) + geom_smooth(method = "loess",span=0.3)
# The percentage of undecided voters declines over time but is still around 10% throughout June.
# Over most of the date range, the confidence bands for "Leave" and "Remain" overlap.
# Over most of the date range, the confidence bands for "Leave" and "Remain" are below 50%.
# In the first half of June, "Leave" was polling higher than "Remain", although this difference was within the confidence intervals.

