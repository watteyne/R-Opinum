load("C:/dev/R/projects/Wattmatters/theoretical_production.rdata")

efficiency <- ifelse(is.null(inputForms$efficiency), 0, inputForms$efficiency)
area <- ifelse(is.null(inputForms$area), 0, inputForms$area)

result <- inputVariables$irradiation$TimeSeries

dates <- seq(result$Dates[1], result$Dates[length(result$Dates)], by=15 * 60)

full_irradiation <- approx(result$Dates, result$Values, dates)

list(TimeSeries=data.frame(Dates=dates, Values=approx(result$Dates, result$Values, dates)$y * efficiency * area))
