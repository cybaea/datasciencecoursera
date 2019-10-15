# Write a function called best that take two arguments: the 2-character
# abbreviated name of a state and an outcome name. The function reads the
# outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the best (i.e. lowest) 30-day mortality for the
# specified outcome in that state. The hospital name is the name provided in the
# Hospital.Name variable. The outcomes can be one of “heart attack”, “heart
# failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the
# rankings.

best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome_data %>% 
    filter(state == !!state, !is.na(.data[[outcome]])) %>% 
    arrange(!!sym(outcome), `hospital name`) %>% 
    pull(`hospital name`) %>% 
    first()
}
