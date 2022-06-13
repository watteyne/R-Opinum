load('c:/dev/R/Calculated/georgetown/sampleDataGUS.rdata')

library('lubridate')

LIMIT <- 1

last_number <- inputForms$number %% 10

if (last_number == 1 | last_number == 5 | last_number == 9) {
  days_of_week <- c(2, 5)
} else if (last_number == 0 | last_number == 3 | last_number == 7) {
  days_of_week <- c(4, 7)
} else {
  days_of_week <- c(3, 6)
}

df <- inputVariables$adhoc$TimeSeries
df$Values <- df$Values + inputVariables$programmed$TimeSeries$Values

df$Dates <- as.POSIXct(df$Dates, origin='1970-01-01', tz='UTC')
df$WeekDay <- (wday(df$Dates) - 1) %% 7

df <- df[! df$WeekDay %in% days_of_week, ]
df$Values <- ifelse(df$Values > LIMIT, 1, 0)

list(TimeSeries=df)