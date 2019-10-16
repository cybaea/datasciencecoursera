# Week 2 quiz
library("tidyverse")
library("httr")
library("stringi")
# Q1
# "created_at": "2013-11-07T13:25:07Z"
# Q2
library("sqldf")
acs <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
ans <- sqldf("select pwgtp1 from acs where AGEP < 50")
# Q3
ans <- sqldf("select distinct pwgtp1 from acs")
# Q4
page <- GET("http://biostat.jhsph.edu/~jleek/contact.html") %>% content(as = "text") 
lines <- stri_split_lines(page) %>% first()
map_int(c(10,20,30,100), ~ nchar(lines[.]))
# Q5
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
# doing two colums to demonstrate
data <-
  read_fwf(
    url, 
    col_positions =
      fwf_positions(
        start = c(29, 59),
        end = c(32, 62),
        col_names = c("Nino3.SST", "Nino4.SSTA")
      ),
    col_types = "nn",
    skip = 4
    )
data %>% summarise_all(sum) %>% pull(1)
