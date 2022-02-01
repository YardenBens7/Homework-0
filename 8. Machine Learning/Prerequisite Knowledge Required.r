data(heights)
heights

nrow(heights)
heights[777,]$height

which.max(heights$height)
heights$height[1017]
which.min(heights$height)

mean(heights$height)
median(heights$height)

sum(heights$sex=="Male")/nrow(heights)
sum(heights$height > 78)
nrow(heights %>% filter(sex == "Female" & height > 78))