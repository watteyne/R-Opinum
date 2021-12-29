load("C:/dev/R/projects/Wattmatters/index_demarrage.rdata")

index <- inputVariables$index$TimeSeries

dates <- seq(index$Dates[1], index$Dates[length(index$Dates)], by=15 * 60)

list(TimeSeries=data.frame(Dates=dates[-1], Values=diff(approx(index$Dates, index$Values, dates, method="constant")$y)))

