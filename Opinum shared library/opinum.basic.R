load('c:/dev/R/Opinum shared library/sampleData.rdata')

library(lubridate)

get_granularity_names <- function(granularity) {
  # Focus on other things than days later on
  if (granularity == "day") {
    seq_granularity <- "DSTday"
  } else {
    seq_granularity <- granularity
  }
  list(rounding = granularity, sequence = seq_granularity)
}

opinum_consumption_from_index <- function(input_variable,
                                          target_variable,
                                          keep_original_points = FALSE,
                                          granularity = "day",
                                          na_at_the_end = FALSE) {
  df <- input_variable$TimeSeries
  df$Dates <- as.POSIXct(df$Dates, origin='1970-01-01', tz='UTC')
  df$Dates <- force_tz(df$Dates, target_variable$SourceTimeZoneId)
  granularities <- get_granularity_names(granularity)
  dates <- seq(ceiling_date(df$Dates[1], unit=granularities$rounding),
               floor_date(df$Dates[length(df$Dates)], unit=granularities$rounding),
               by=granularities$sequence)
  if (keep_original_points) {
    dates <- sort(union(df$Dates, dates))
    dates <- as.POSIXct(dates, origin='1970-01-01', tz='UTC')
    dates <- force_tz(dates, target_variable$SourceTimeZoneId)
  }
  indices <- approx(df$Dates, df$Values, dates, method="linear")$y
  if (na_at_the_end) {
    values <- c(diff(indices), NA)
  } else {
    values <- c(NA, diff(indices))
  }
  list(TimeSeries=data.frame(Dates=force_tz(dates, "UTC"), Values=values))
}

opinum_consumption_from_manual_consumption <- function(input_variable,
                                                       target_variable,
                                                       keep_original_points = TRUE,
                                                       granularity = "day",
                                                       na_at_the_end = FALSE) {
  input_variable$TimeSeries['Values'] <- cumsum(input_variable$TimeSeries['Values'])
  opinum_consumption_from_index(input_variable, target_variable, keep_original_points, granularity, na_at_the_end)
}

opinum_consumption_from_manual_consumption(inputVariables$index,targetVariable)
