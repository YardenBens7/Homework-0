# An old version of the SAT college entrance exam had a 
# -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. 
# The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. 
# Suppose a student chooses answers by guessing for all questions on the test.

# Question 1a - guess one question correct
1/5

# Question 1b - Expected points for guessing one question
1*1/5 + -0.25*4/5

# Question 1c - Expected score of guessing 44 questions
44 * (1*1/5 + -0.25*4/5)

# Question 1d - SE of 44 questions
sqrt(44) * (abs(1--0.25) * sqrt(1/5*4/5))

# Question 1e - use CLT to determine probability that guess got 8 points or higher
1 - pnorm(8, 44 * (1*1/5 + -0.25*4/5),  sqrt(44) * (abs(1--0.25) * sqrt(1/5*4/5)))

# Question 1f - What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
B <- 10000

S <- replicate(B, 
          {
           X <- sample(c(1, -0.25), 44, replace = TRUE, prob = c(1/5,4/5))
           sum(X)
          })
mean(S>8)

# Question 2a - What is the expected value of the score when guessing on this new test?
44 * 1*1/4 + 0*3/4

# Question 2b - What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)

score <- sapply(p, function(v) {
  e_points <- (1*v) + (0*(1-v))
  m <- 44 * e_points
  se <- sqrt(44) * abs(0-1) * sqrt(v*(1-v))
  1 - pnorm(35, m, se)
})

min(p[which(score > 0.8)])

# A casino offers a House Special bet on roulette, which is a 
# bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets
# a losing bet yields -$1 and a successful bet yields $6.
# A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.

# Question 3a - What is the expected value of the payout for one bet?
mu <- 5/38 * 6 + 33/38 * -1

# Question 3b - What is the standard error of the payout for one bet?
sigma <- abs(6--1) * sqrt(5/38 * 33/38)

# Question 3c - What is the expected value of the average payout over 500 bets?
mu

# Question 3d - What is the standard error of the average payout over 500 bets?
sigma / sqrt(500)

# Question 3e - What is the expected value of the sum of 500 bets?
500 * mu

# Question 3f - What is the standard error of the sum of 500 bets?
sqrt(500) * sigma

# Question 3g - Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, .
pnorm(0, 500 * mu, sqrt(500) * sigma)




