# Week  quiz
library("tidyverse")
library("lubridate")

# Q1
data <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
strsplit(names(data), "wgtp")[[123]]

# Q2
gdp <- 
  read_csv(
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", 
    col_names =
      c(
        "CountryCode",
        "ranking",
        "CountryName",
        "economy"
      ),
    col_types = "ci-cn-----",
    skip = 4,
    n_max = 191
  ) 
mean(gdp$economy, na.rm = TRUE)

# Q3
grep("^United", gdp$CountryName) %>% length()

# Q4
edu <- 
  read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv") %>% 
  as_tibble(.name_repair = "universal")
data <- inner_join(gdp, edu) 
data %>% 
  filter(str_detect(Special.Notes, "Fiscal year end: *June")) %>% 
  count()

# Q5
library("quantmod")
amzn <- getSymbols("AMZN", auto.assign = FALSE)
sampleTimes <- index(amzn)
sum(year(sampleTimes) == 2012)
sum(year(sampleTimes) == 2012 & wday(sampleTimes, week_start = 1) == 1)
