load("projects/GEG/R50/sampleData.rdata")

library(dplyr)

index_variables <- inputVariables[grep("INDEX", names(inputVariables))]

index_dates <- Reduce(union, index_variables %>%
  lapply(
    function(x) {
      x$TimeSeries$Dates
    }))

index_values <- Reduce('+', index_variables %>%
  lapply(
    function(x) {
      if (nrow(x$TimeSeries) > 0) {
        approx(x$TimeSeries$Dates, x$TimeSeries$Values, index_dates,
               method = 'constant', rule = 2, f = 0)$y
      } else {
        replicate(length(index_dates), 0)
      }
    }
  ))

index <- data.frame(Dates = index_dates, Values = index_values)

index_dates <- seq(index_dates[1] %/% 86400, index_dates[length(index_dates)] %/% 86400) * 86400

index <- approx(index$Dates, index$Values, index_dates)
names(index) <- c("Dates", "Values")
index$next_r151_index <- lead(index$Values)

cdc <- inputVariables$CDC$TimeSeries

cdc$day <- (cdc$Dates %/% 86400) * 86400

cdc <- cdc %>%
    group_by(day) %>%
    mutate(cdc_index = cumsum(Values), sum_values = sum(Values))

cdc <- merge(cdc, index, by.x="day", by.y="Dates")

cdc$Values <- cdc$Values.y + cdc$cdc_index / cdc$sum_values * (cdc$next_r151_index - cdc$Values.y)




# list(TimeSeries=index)