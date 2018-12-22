library(RPostgreSQL)
library(DBI)
library(rvest)
library(tidyverse)
library(lubridate)

# From https://www.gofundme.com/TheTrumpWall

# Table rows are stored in order in which they were obtained. 
# I am in Mexico currently so the times vs. amount will look a little off
# as I the times I stored in the data base working locally in Mexico are
# different from the CRON on AWS in OHIO. You can resolve the time difference if you like or 
# just use the values after ROW 32. 

# ^^ Update ^^ Cleaned it up for ya!

clean_money = function(string) {
  str_remove_all(string, "\\,") %>% 
    str_remove_all("\\$") %>% 
    as.numeric
}


con <- dbConnect(PostgreSQL(), 
                 dbname   = 'website',
                 host     = 'drenr.com',
                 port     = 5432,
                 user     = "linkedin",
                 password = "linkedin")


SELECT <- '*'

FROM <- "wall_fund"

WHERE <- "1=1"

query <- paste("SELECT", SELECT, "FROM", FROM, "WHERE", WHERE)

data = tbl(con, sql(query)) %>%
  as.data.frame 

cleaned_data =
  data %>% 
  arrange(amt) %>% 
  mutate(amt = clean_money(amt),
         time = case_when(
                  time < "2018-12-20 08:50:20" ~ time + hours(6),
                  TRUE                         ~ time
                ),
         diff = amt - lag(amt),
         minute = format(time, "%M"),
         a_ten = (as.numeric(minute) %% 10) == 0,
         day = as.character(day(time))) %>%
    filter(a_ten) %>%
  filter(time != "2018-12-20 19:50:54",
         time != '2018-12-20 10:50:20') %>%
  select(-minute, -a_ten) %>% 
  filter(diff < 900000)



options(scipen=10000)
ggplot(cleaned_data) +
  aes(x = time, y = diff, colour = day) + 
  geom_point() +
  xlab("Time") +
  ylab("Contribution") +
  ggtitle("10 Minute Contribution Amount")


