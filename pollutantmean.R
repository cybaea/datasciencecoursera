## Three different solutions
pollutantmean_base <- function(directory, pollutant, id = 1:332) {
  files <- dir(directory, pattern = "*.csv")
  paths <- file.path(directory, files)
  data <- do.call(rbind, lapply(paths, read.csv))
  mean(data[data$ID %in% id,][[pollutant]], na.rm = TRUE)
}
pollutantmean_loop <- function(directory, pollutant, id = 1:332) {
  files <- dir(directory, pattern = "*.csv")
  paths <- file.path(directory, files)
  ## Not really recommneded, but this is how you do it:
  data <- read.csv(paths[[1]])
  for(file in paths[-1]) {
    new_data <- read.csv(file)
    data <- rbind(data, new_data)
  }
  mean(data[data$ID %in% id,][[pollutant]], na.rm = TRUE)
}
pollutantmean_tidy <- function(directory, pollutant, id = 1:332) {
  require("tidyverse") # Like library(), but for functions
  files <- dir(directory, pattern = "*.csv")
  paths <- file.path(directory, files)
  data <- map_dfr(paths, read_csv)
  data %>% 
    filter(ID %in% id) %>% 
    pull(!!pollutant) %>% # help("quasiquotation")
    mean(na.rm = TRUE)
}
pollutantmean <- pollutantmean_tidy
