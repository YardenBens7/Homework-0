library(dslabs)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
install.packages("tidytext")
library(tidytext)
options(digits = 3) 
data(brexit_polls)

nrow(brexit_polls %>% filter(str_detect(startdate, "-04-")))
sum(round_date(brexit_polls$enddate, unit="week") == "2016-06-12")

table(weekdays(brexit_polls$enddate))

data(movielens)
which.max(table(year(as_datetime(movielens$timestamp))))
which.max(table(hour(as_datetime(movielens$timestamp))))
dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews
reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews

gutenberg_metadata %>% filter(str_detect(title,"Pride and Prejudice")) %>% select(gutenberg_id) %>% count()

gutenberg_works(title=="Pride and Prejudice")$gutenberg_id

book <- gutenberg_works(title=="Pride and Prejudice") %>% gutenberg_download(meta_fields = "title", mirror = "http://gutenberg.readingroo.ms/")

words <- book %>%
  unnest_tokens(word, text)
nrow(words)

words <- words %>% filter(!word %in% stop_words$word)

words <- words %>% filter(!str_detect(word, "\\d"))

words %>% count(word) %>% filter(n>100)
words %>% count(word) %>% arrange(desc)

afinn <- get_sentiments("afinn")

afinn_sentiments <- inner_join(words,afinn)
nrow(afinn_sentiments)
sum(afinn_sentiments$value > 0)/6065
sum(afinn_sentiments$value == 4)
