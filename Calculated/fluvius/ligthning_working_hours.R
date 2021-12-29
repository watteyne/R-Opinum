main <- function() {
  values <- 0
  for (name in names(inputVariables)) {
    values <- values + ifelse(inputVariables[[name]]$TimeSeries$Values > 0, 1, 0)
  }
  list(TimeSeries = data.frame(Dates = inputVariables$v0000$TimeSeries$Dates,
                               Values = ifelse(values > (length(inputVariables) / 2), 1, 0)))
}

main()