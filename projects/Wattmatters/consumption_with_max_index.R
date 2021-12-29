load("C:/dev/R/projects/Wattmatters/index_with_max_index.rdata")

T1 <- ifelse(is.null(inputForms$T1), 1, inputForms$T1)
correction <- ifelse(is.null(inputForms$correction), 1, inputForms$correction)
maxindex <- ifelse(is.null(inputForms$maxindex), 0, inputForms$maxindex)

index <- inputVariables$Index$TimeSeries

dates <- seq(index$Dates[1], index$Dates[length(index$Dates)], by=15 * 60)

full_index <- approx(index$Dates, index$Values, dates)

values <- full_index$y + cumsum(ifelse(c(0, diff(full_index$y))<0, maxindex, 0))

list(TimeSeries=data.frame(Dates=dates[-1], Values=diff(values) * T1 * correction))
