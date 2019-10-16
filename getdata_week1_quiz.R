# Week 1 Quiz
library("tidyverse")
library("readxl")
library("xml2")
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
data <- read_csv(url(data_url))
# Q1
sum(data$VAL >= "24", na.rm = TRUE)

# Q3
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
data_file <- "NGAP.xlsx"
download.file(data_url, data_file, method = "curl")
dat <- read_excel(data_file, range = "R18C7:R23C15")
sum(dat$Zip * dat$Ext, na.rm=TRUE)
file.remove(data_file)

# Q4
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
xml <- read_xml(url(data_url))
zips <- xml_find_all(xml, ".//zipcode") %>% map_chr(xml_text)
sum(zips == "21231")

# Q5
# Note that the right answer is the wrong answer.
local({
  library("data.table")
  library("rbenchmark")
  DT <-
    data.table::fread("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
  
  benchmark(
    two_mean = { mean(DT[DT$SEX == 1,]$pwgtp1); mean(DT[DT$SEX == 2,]$pwgtp1) },
    native = DT[, mean(pwgtp15), by = SEX],
    tapply = tapply(DT$pwgtp15, DT$SEX, mean),
    order = "user.self",
    replications = 1e3
  )
  
})