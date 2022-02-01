positive_disease <- 0.85
negative_healthy <- 0.9
positive_healthy <- 0.1
disease <- 0.02
healthy <- 0.98

# A = have disease, B = test is positive
positive_disease * disease / (positive_disease * disease + positive_healthy * healthy)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

positive_test <- mean(test)
disease_negative <- mean(disease[test==0])
disease_positive <- mean(disease[test==1])
mean(disease_positive) / mean(disease==1)

