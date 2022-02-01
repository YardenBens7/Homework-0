library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)
txt <- pdftools::pdf_text(fn)

x <- str_split(txt[9],'\n')
class(x)
length(x)

s <- x[[1]]
class(s)
length(s)
s <- str_trim(s)
s[1]

header_index <- str_which(s,"2015")[1]
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]

tail_index <- str_which(s,"Total")

sum(str_count(s,"\\d+") == 1)

out <- c(1:3,7,10,36:41)
s <- s[-out]
length(s)

s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s <- matrix(as.numeric(s), ncol=5)
s <- as.data.frame(s)
colnames(s) <- c("day", header)
mean(s$`2015`)
mean(s$`2016`)
mean(s$`2017`[1:19])
mean(s$`2017`[20:30])

tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
mean(tab$"2015")

tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()