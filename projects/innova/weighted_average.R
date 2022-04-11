load("C:/dev/R/projects/innova/sampleData.rdata")

library(lubridate)
library(dplyr)

price <- inputVariables$PriceE$TimeSeries
cons <- inputVariables$ConsumptionE$TimeSeries

dates <- intersect(price$Dates, cons$Dates)

result <- cons[cons$Dates %in% dates, ]

result$Dates <- as.POSIXct(result$Dates, origin='1970-01-01', tz='UTC')

result$Cost <- result$Values * price[price$Dates %in% dates, 'Values']

result$Dates <- floor_date(result$Dates, unit='month')

result <- group_by(result, Dates) %>%
  summarise(Values = sum(Values),
            Cost = sum(Cost))
result <- data.frame(Dates=result$Dates, Values=result$Cost / result$Values)

calculation1 <- list(TimeSeries=result)

# Just for shared code, ignore

inputVariables$Weighted$TimeSeries <- data.frame(Dates=as.integer(result$Dates), Values=result$Values)

# Code 2

weighted <- inputVariables$Weighted$TimeSeries
cons <- inputVariables$ConsumptionE$TimeSeries

list(TimeSeries=data.frame(Dates=cons$Dates,
                           Values=approx(weighted$Dates, weighted$Values, cons$Dates, method='constant', f=0, rule=2)$y))


