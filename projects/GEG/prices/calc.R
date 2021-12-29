load("projects/GEG/prices/sampleData.rdata")

library(lubridate)

price <- inputVariables$price$TimeSeries
price$Dates <- as.POSIXct(price$Dates, origin="1970-01-01", tz="UTC")
dates <- seq(price$Dates[1], price$Dates[length(price$Dates)] %m+% years(5), by="month")

list(TimeSeries=data.frame(Dates=dates,
                           Values=approx(price$Dates, price$Values, dates,
                                         method="constant", f=0, rule=2)$y))