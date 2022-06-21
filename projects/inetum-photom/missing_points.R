load("c:/dev/R/projects/inetum-photom/missing_points.rdata")

dates <- inputVariables$irradiance$TimeSeries$Dates

received_points <- length(dates)

time_delta <- dates[received_points] - dates[1]

expected_points <- time_delta / 600 + 1

missing_points <- expected_points - received_points

list(TimeSeries=data.frame(Dates=dates[received_points], Values=ifelse(missing_points > 0, missing_points, 0)))

