library(dplyr)

result <- inputVariables$consumption$TimeSeries
result$Dates <- (result$Dates %/% (24 * 3600)) * 24 * 3600
result$Values <- ifelse((result$Values > 0) & (inputVariables$workingHours$TimeSeries$Values ==  0), 1, 0)

result <- result %>%
	group_by(Dates) %>%
	summarise(Values = sum(Values) * inputForms$power * inputForms$devices)

result <- result[-1, ]

list(TimeSeries=result[result$Values > 0, ])