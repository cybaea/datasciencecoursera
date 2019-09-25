corr <- function(directory, threshold = 0) {
  files <- dir(directory, pattern = "*.csv")
  paths <- file.path(directory, files)
  data <- do.call(rbind, lapply(paths, read.csv))
  n_complete_list <- by(data, data$ID, function(x) sum(complete.cases(x)), simplify = FALSE)
  n_complete <- 
    data.frame(
      id = as.integer(names(n_complete_list)),
      nobs = unlist(n_complete_list, use.names = FALSE)
    )
  ids <- n_complete[n_complete$nobs > threshold, "id"]
  if (length(ids) < 1) {
    return(numeric(0))
  }
  cor_list <- by(data, data$ID, function(x) cor(x$sulfate, x$nitrate, use = "na.or.complete"))
  unname(cor_list[ids])
}
