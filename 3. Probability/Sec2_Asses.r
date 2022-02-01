set.seed(16)
act_scores <- rnorm(10000,20.9,5.7)

# Question 1a
mean(act_scores)

# Question 1b
sd(act_scores)

# Question 1c
sum(act_scores>=36.000000)

# Question 1d
sum(act_scores>=30.000000)/10000

# Question 1e
sum(act_scores<=10.000000)/10000

# Question 2
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

# Question 3a
# Convert to z scores
z_scores <- (act_scores-mean(act_scores))/sd(act_scores)
sum(z_scores>=2)/10000

# Question 3b
2*sd(act_scores)+mean(act_scores)

# Question 3c
qnorm(0.975,mean(act_scores),sd(act_scores))

# Question 4a
ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))

# Question 4b
qnorm(0.95, 20.9, 5.7)

# Question 4c
sample_quantiles <- seq(0.01, 0.99, 0.01)
quantile(act_scores,sample_quantiles)

# Question 4d
theoretical_quantiles <- qnorm(sample_quantiles, 20.9, 5.7)
qqplot(y=sample_quantiles, x=theoretical_quantiles)
