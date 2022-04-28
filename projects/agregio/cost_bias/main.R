load("c:/dev/R/projects/agregio/cost_bias/sampleData.rdata")

library(lubridate)
library(dplyr)
library(zoo)

dates <- Reduce(intersect, inputVariables[names(inputVariables) != "self"] %>%
  lapply(
    function(x){
      as.POSIXct(x$TimeSeries$Dates, origin='1970-01-01', tz='UTC')
    }))



{ for (var_name in names(inputVariables)) {
  df <- inputVariables[[var_name]]$TimeSeries
  df$Dates <- as.POSIXct(df$Dates, origin='1970-01-01', tz='UTC')
  df <- df[df$Dates %in% dates, ]
  inputVariables[[var_name]]$TimeSeries <- df
}
}

result <- inputVariables$SPOT$TimeSeries

result$Values <- result$Values *
  (inputVariables$production$TimeSeries$Values - inputVariables$longForecast$TimeSeries$Values)

result$Values <- result$Values - (inputVariables$Intraday$TimeSeries$Values *
  (inputVariables$shortForecast$TimeSeries$Values - inputVariables$longForecast$TimeSeries$Values))

short_bias <- inputVariables$shortForecast$TimeSeries$Values - inputVariables$production$TimeSeries$Values

result$Values <- result$Values + ifelse(short_bias < 0,
                                        inputVariables$PREP$TimeSeries$Values * short_bias,
                                        inputVariables$PREN$TimeSeries$Values * short_bias)

result <- group_by(result, Dates=floor_date(result$Dates, "month")) %>%
  summarise(Values=sum(Values))

production <- group_by(inputVariables$production$TimeSeries,
                       Dates=floor_date(inputVariables$production$TimeSeries$Dates, "month")) %>%
  summarise(Values=sum(Values))

n_months <- min(nrow(result), inputConstants$monthCoverage)

if (n_months >= ceiling(2 * inputConstants$monthCoverage / 3)) {
  result <- data.frame(Dates=result$Dates[1:(nrow(result) + 1 - n_months)],
                       Values=rollapply(result$Values, n_months, FUN=sum))

  production <- data.frame(Dates=production$Dates[1:(nrow(production) + 1 - n_months)],
                           Values=rollapply(production$Values, n_months, FUN=sum))

  result$Values <- result$Values / production$Values

  if (nrow(inputVariables$self$TimeSeries[inputVariables$self$TimeSeries$Dates == result$Dates[1], ]) > 0) {
    result <- result[-1, ]
  }
} else {
  result$Values <- NA
}

list(TimeSeries=result)