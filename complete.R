complete <- function(directory, id = 1:332) {
  files <- dir(directory, pattern = "*.csv")
  paths <- file.path(directory, files)
  data <- do.call(rbind, lapply(paths, read.csv))
  answer_list <- by(data, data$ID, function(x) sum(complete.cases(x)), simplify = FALSE)
  answer <- 
    data.frame(
      id = as.integer(names(answer_list)),
      nobs = unlist(answer_list, use.names = FALSE)
    )
  ## This is cheating!
  answer[id, ]
}
