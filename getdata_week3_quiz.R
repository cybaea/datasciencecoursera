# Week 3 quiz
library("tidyverse")
library("jpeg")
library("httr")
# Q1
data <- read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
data %>% 
  transmute(
    agricultureLogical = ACR >= 3 & AGS >= 6
  ) %>% 
  pull() %>% 
  which() %>% 
  head(3)

# Q2
image_data <- 
  GET("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg") %>% 
  content(as = "raw")
image <- readJPEG(image_data, native = TRUE)
quantile(image, probs = c(.3,.8)) # this is a weird thing to do

# Q3
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
edu <- 
  read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv") %>% 
  as_tibble(.name_repair = "universal")
data <- inner_join(gdp, edu) %>% arrange(desc(ranking))
NROW(data)
data[13, "CountryName"]

# Q4
data %>% 
  group_by(Income.Group) %>% 
  summarise(avg_rank = mean(ranking)) %>% 
  filter(str_detect(Income.Group, "^High")) %>% 
  arrange(desc(Income.Group)) %>% 
  pull()

# Q5

data %>% 
  mutate(ranking.q = cut_number(ranking, 5)) %>% 
  group_by(ranking.q, Income.Group) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = ranking.q, values_from = n) %>% 
  select(1:2) %>% 
  filter(Income.Group == "Lower middle income")
