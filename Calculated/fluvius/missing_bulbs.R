library(dplyr)

result <- inputVariables$consumption$TimeSeries
result$Dates <- (result$Dates %/% (24 * 3600)) * 24 * 3600

result <- result %>%
    group_by(Dates) %>%
    summarise(Values = inputForms$devices - (max(Values) / inputForms$power))

result <- result[-1, ]

list(TimeSeries=result[result$Values > 0, ])

