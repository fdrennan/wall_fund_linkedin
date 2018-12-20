library(RPostgreSQL)
library(DBI)
library(rvest)
library(tidyverse)


# From https://www.gofundme.com/TheTrumpWall

# Table rows are stored in order in which they were obtained. 
# I am in Mexico currently so the times vs/amount will look a little off
# as I the times I stored in the data base working locally in Mexico are
# different from the CRON on AWS in OHIO. You can resolve the time difference if you like or 
# just use the values after ROW 32. 

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
  as.data.frame %>% 
  mutate(amt = clean_money(amt))

