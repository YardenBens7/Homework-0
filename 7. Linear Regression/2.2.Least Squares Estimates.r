beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

install.packages("Lahman")
library(Lahman)

filtered_teams <- Teams %>% filter(yearID %in% c(1961:2001)) %>% mutate(HR_per_game = HR/G, BB_per_game = BB/G, Runs_per_game = R/G)
lm(Runs_per_game ~ (BB_per_game + HR_per_game), data = filtered_teams)

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
