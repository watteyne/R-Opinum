load('c:/dev/R/Calculated/sampleData.rdata')

library(lubridate)
library(dplyr)

{
  capacities <- data.frame(Dates=numeric(0), Values=numeric(0))

  for (name in names(inputVariables)) {
    if (startsWith(name, 'v')) {
      capacity <- inputVariables[[name]]$TimeSeries
      if (nrow(capacity) > 0) {
        meaningful <- inputVariables[[gsub('v', 'm', name)]]$TimeSeries
        if (nrow(meaningful) > 0) {
          capacity$Dates <- floor_date(as.POSIXct(capacity$Dates, origin='1970-01-01', tz='UTC'), unit='month')
          meaningful$Dates <- as.POSIXct(meaningful$Dates, origin='1970-01-01', tz='UTC')
          capacity <- capacity[capacity$Dates >= meaningful$Dates[1], ]
          capacity <- aggregate(Values ~ Dates, capacity, mean)
          capacity[(! capacity$Dates %in% meaningful$Dates), "Values"] <- 0
          capacity[(capacity$Dates %in% meaningful$Dates), "Values"] <-
            capacity[(capacity$Dates %in% meaningful$Dates), "Values"] * meaningful[meaningful$Dates %in% capacity$Dates, "Values"]
          capacities <- rbind(capacities, capacity)
        }
      }
    }
  }

  list(TimeSeries=aggregate(Values ~ Dates, capacities, sum))
}



