library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
html_table(nodes[[8]])

html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

length(nodes)
html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
tab_1 <- tab_1 %>% select(-X1)
new_tab_1 <- tab_1[-c(1),] 
colnames(new_tab_1) <- c("Team", "Payroll", "Average")
new_tab_2 <- tab_2[-c(1),]
colnames(new_tab_2) <- c("Team", "Payroll", "Average")
full_join(new_tab_1,new_tab_2,by="Team")



library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
tables <- tab %>% html_table(fill=TRUE)

