load("C:/dev/R/projects/Wattmatters/gas_consumption.rdata")

library(lubridate)

cons <- inputVariables$cons$TimeSeries
cons$Dates <- as.POSIXct(cons$Dates, origin='1970-01-01', tz=inputVariables$cons$Variable$SourceTimeZoneId)

pcs_name <- ifelse(is.null(inputForms$pcs), "Dilbeek", inputForms$pcs)
pcs <- inputVariables[[pcs_name]]$TimeSeries
pcs$Dates <- as.POSIXct(pcs$Dates, origin='1970-01-01', tz=inputVariables[[pcs_name]]$Variable$SourceTimeZoneId)
pcs$Dates <- pcs$Dates %m-% period('1 month')

pcs <- data.frame(Dates=cons$Dates,
                  Values=approx(pcs$Dates, pcs$Values, cons$Dates, method='constant', rule=2, f=0)$y)

cons$Values <- cons$Values * pcs$Values

list(TimeSeries=cons)



