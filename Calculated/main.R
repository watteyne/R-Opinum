values <- c()
dates <- c()

{for (var in inputVariables) {
  values[[length(values) + 1]] <- var$TimeSeries$Values
  dates[[length(dates) + 1]] <- var$TimeSeries$Dates
}}

list(TimeSeries=data.frame(Dates=dates[[1]], Values=max(unlist(values))))
