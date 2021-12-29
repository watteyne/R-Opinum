source("Opinum shared library/opinum.basic.R")


get_test_data <- function() {
  df <- c(as.POSIXct("2021-03-01 10:00:00"), 10,
          as.POSIXct("2021-03-02 10:00:00"), 11, # One day after
          as.POSIXct("2021-03-03 04:00:00"), 12, # The next day at 4
          as.POSIXct("2021-03-03 10:00:00"), 13, # The same day at 10
          as.POSIXct("2021-03-04 12:00:00"), 14, # The next day at 12
          as.POSIXct("2021-03-27 10:00:00"), 200, # 23 days after at 10
          as.POSIXct("2021-03-28 10:00:00"), 14, # The day after (but we lost one hour in Belgium)
          as.POSIXct("2021-10-30 10:00:00"), 2000, # 216 days after
          as.POSIXct("2021-10-31 10:00:00"), 15, # The day after (but we gained one hour in Belgium)
          as.POSIXct("2021-11-30 10:00:00"), 350 # 30 days after
  )
  conso <- as.data.frame(t(matrix(df, ncol = length(df) / 2)))
  colnames(conso) <- c('Dates', 'Values')
  index <- conso
  index['Values'] <- cumsum(index['Values'])
  # I hope that an explicit calculation explains the expected results
  result <- c(as.POSIXct("2021-03-02 00:00:00"), (11 / ((24 - 10) + 10)) * (24 - 10),
              as.POSIXct("2021-03-02 10:00:00"), (11 / ((24 - 10) + 10)) * 10,
              as.POSIXct("2021-03-03 00:00:00"), (12 / ((24 - 10) + 4)) * (24 - 10),
              as.POSIXct("2021-03-03 10:00:00"), 13,
              as.POSIXct("2021-03-04 00:00:00"), (14 / ((24 - 10) + (24 - 12))) * (24 - 10),
              as.POSIXct("2021-03-27 00:00:00"), (200 / ((24 - 12) + (22 * 24) + 10)) * 24,
              as.POSIXct("2021-03-28 00:00:00"), (14 / ((24 - 10) + (10 - 1))) * ((24 - 1) - (10 - 1)),
              as.POSIXct("2021-10-30 00:00:00"), (2000 / ((24 - 10) + (215 * 24) + 10)) * 24,
              as.POSIXct("2021-10-31 00:00:00"), (15 / ((24 - 10) + (10 + 1))) * ((24 + 1) - (10 + 1)),
              as.POSIXct("2021-11-30 00:00:00"), (350 / (((24 + 1) - (10 + 1)) + (29 * 24) + 10)) * 24
  )
  expected_result <- as.data.frame(t(matrix(result, ncol = length(result) / 2)))
  colnames(expected_result) <- c('Dates', 'Values')
  list(index = index, conso = conso, expected_result = expected_result)
}

test_opinum_daily_consumption_from_index <- function() {
  print('Test index errors:')
  test_data <- get_test_data()
  result <- opinum_consumption_from_index(list(TimeSeries = test_data$index))
  result <- result$TimeSeries[result$TimeSeries$Dates %in% test_data$expected_result$Dates,]
  colnames(result) <- c('Dates', 'Calculated')
  result['Expected'] <- test_data$expected_result$Values
  print(result[(abs(result['Expected'] - result['Calculated'])) > 0.001, ])
}

test_opinum_daily_consumption_from_consumption <- function() {
  print('Test consumption errors:')
  test_data <- get_test_data()
  result <- opinum_consumption_from_manual_consumption(list(TimeSeries = test_data$conso))
  result <- result$TimeSeries[result$TimeSeries$Dates %in% test_data$expected_result$Dates,]
  colnames(result) <- c('Dates', 'Calculated')
  result['Expected'] <- test_data$expected_result$Values
  print(result[(abs(result['Expected'] - result['Calculated'])) > 0.001, ])
}

test_opinum_daily_consumption_from_index()
test_opinum_daily_consumption_from_consumption()


