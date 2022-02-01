## a=2, b=-1, c=-4
## Answers: 1.686, -1.186
(-b + sqrt(b^2 -4*a*c))/(2*a)
(-b - sqrt(b^2 -4*a*c))/(2*a)

## Answer: 5
log(1024, base = 4)

library(dslabs)
data("movielens")

#Answer: 100004 lines, 7 variables
str(movielens)

#Answer: chr
class(movielens$title)

#Answer: factor, 901 levels
class(movielens$genres)
nlevels(movielens$genres)
