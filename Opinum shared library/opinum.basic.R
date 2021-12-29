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

opinum_consumption_from_index <- function(input_variable, keep_original_points = TRUE, granularity = "day") {
  df <- input_variable$TimeSeries
  df$Dates <- as.POSIXct(df$Dates, origin='1970-01-01')
  granularities <- get_granularity_names(granularity)
  dates <- seq(ceiling_date(df$Dates[1], unit=granularities$rounding),
               floor_date(df$Dates[length(df$Dates)], unit=granularities$rounding),
               by=granularities$sequence)
  if (keep_original_points) {
    dates <- sort(union(df$Dates, dates))
  }
  indices <- approx(df$Dates, df$Values, dates, method="linear")$y
  list(TimeSeries=data.frame(Dates=dates[-1], Values=diff(indices)))
}

opinum_consumption_from_manual_consumption <- function(input_variable, keep_original_points = TRUE, granularity = "day") {
  input_variable$TimeSeries['Values'] <- cumsum(input_variable$TimeSeries['Values'])
  opinum_consumption_from_index(input_variable, keep_original_points, granularity)
}
