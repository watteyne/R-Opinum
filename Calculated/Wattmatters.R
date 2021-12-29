index <- inputVariables$Index$TimeSeries

index <- index[(index$Dates < 1624850100) | (index$Dates > 1625027400), ]

dates <- seq(index$Dates[1], index$Dates[nrow(index)], by=900)

values <- approx(index$Dates, index$Values, dates)$y

values <- diff(values) * inputForms$TI + inputForms$Facteur

list(TimeSeries=data.frame(Dates=dates[-1], Values=values))



