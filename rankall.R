# Write a function called rankall that takes two arguments: an outcome name
# (outcome) and a hospital ranking (num). The function reads the
# outcome-of-care-measures.csv file and returns a 2-column data frame containing
# the hospital in each state that has the ranking specified in num. For example
# the function call rankall("heart attack", "best") would return a data frame
# containing the names of the hospitals that are the best in their respective
# states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named
# hospital, which contains the hospital name, and the second column is named
# state, which contains the 2-character abbreviation for the state name.
# Hospitals that do not have data on a particular outcome should be excluded
# from the set of hospitals when deciding the rankings

rankall <- function(outcome, num = "best") {
  require("tidyverse")
  ## Read outcome data
  outcome_data <- 
    suppressMessages(read_csv(
      "outcome-of-care-measures.csv"
    ))
  outcome_data <-
    outcome_data %>% 
    select(
      `Hospital Name`,
      State,
      starts_with("Hospital 30-Day Death (Mortality) Rates from ")
    ) %>% 
    mutate_at(vars(-`Hospital Name`, -State),
              ~ suppressWarnings(as.numeric(.))) %>% 
    rename_all(~ str_remove(., fixed("Hospital 30-Day Death (Mortality) Rates from "))) %>% 
    rename(hospital = `Hospital Name`) %>% 
    rename_all(str_to_lower)
  ## Check that state and outcome are valid
  valid_states <- unique(outcome_data$state)
  valid_outcomes <-
    c(
      "heart attack", "heart failure", "pneumonia"
    )
  # if(!state %in% valid_states) {
  #   stop("invalid state")
  # }
  if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  get_num <- 
    if(num == "best") {
      first
    } else if(num == "worst") {
      last
    } else {
      function(x) nth(x, num)
    }
  data <-
    outcome_data %>% 
    filter(!is.na(.data[[outcome]])) %>% 
    arrange(state, !!sym(outcome), hospital) %>% 
    group_by(state) %>% 
    summarise(hospital = get_num(hospital)) %>% 
    select(hospital, everything())
  data <- as.data.frame(data)
  row.names(data) <- data$state
  data 
}