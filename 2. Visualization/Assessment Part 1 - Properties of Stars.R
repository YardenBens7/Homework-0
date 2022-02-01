# Start of copy paste
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

# Question 1
> mean(stars$magnitude)
[1] 4.26
> sd(stars$magnitude)
[1] 7.35

# Question 2
stars %>% ggplot(aes(x=magnitude)) + geom_density(alpha=0.2)

# Question 3
stars %>% ggplot(aes(x=temp)) + geom_density(alpha=0.2)

# Question 4
stars %>% ggplot(aes(x=temp,y=magnitude)) + geom_point()

# Question 5
stars %>% ggplot(aes(x=temp,y=magnitude)) + scale_x_continuous(trans="log10") + scale_y_reverse() + scale_x_reverse() + geom_point()

# Question 6
# From plot

# Question 7
# From plot

# Question 8
stars %>% ggplot(aes(x=temp,y=magnitude)) + geom_point() + geom_text_repel(aes(label=star),size=4)

# Question 9             
stars %>% filter(type=="G") %>% ggplot(aes(x=temp,y=magnitude,color=type)) + geom_point()
                 