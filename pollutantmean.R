pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- dir(directory, pattern = "*.csv")
  paths <- file.path(directory, files)
  data <- do.call(rbind, lapply(paths, read.csv))
  mean(data[data$ID %in% id,][[pollutant]], na.rm = TRUE)
}
