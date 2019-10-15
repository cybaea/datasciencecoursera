# Write a function called rankhospital that takes three arguments: the
# 2-character abbreviated name of a state (state), an outcome (outcome), and the
# ranking of a hospital in that state for that outcome (num). The function reads
# the outcome-of-care-measures.csv file and returns a character vector with the
# name of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
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
    rename_all(str_to_lower)
  ## Check that state and outcome are valid
  valid_states <- unique(outcome_data$state)
  valid_outcomes <-
    c(
      "heart attack", "heart failure", "pneumonia"
    )
  if(!state %in% valid_states) {
    stop("invalid state")
  }
  if(!outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  sorted_names <-
    outcome_data %>% 
    filter(state == !!state, !is.na(.data[[outcome]])) %>% 
    arrange(!!sym(outcome), `hospital name`) %>% 
    pull(`hospital name`)
  if(num == "best") {
    return(first(sorted_names))
  }
  if(num == "worst") {
    return(last(sorted_names))
  }
  if(num > length(sorted_names)) {
    return(NA_character_)
  }
  sorted_names[num]
}