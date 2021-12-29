library("lubridate")

IDiff <- inputVariables$IDiff$TimeSeries
Prevision <- inputVariables$Prevision$TimeSeries
Output <- Prevision

Output$Values <-  0
Output[(IDiff$Values > 2) & (IDiff$Values >  (Prevision$Values * 1.2)), "Values"] <- 1

Output$Values <- c(0, diff(Output$Values))
Output <- Output[Output$Values != 0, ]
Output[Output$Values < 0, "Values"] <- 0

Prevision <- Prevision[Prevision$Dates %in% Output$Dates, ]
Output <- Output[Output$Dates %in% Prevision$Dates, ]

dates <- seq(Output$Dates[1], tail(Output$Dates, n=1), step=3600000)

approx(Output$Dates, Output$Values, dates)$y

#Output$weekday <- wday(as.POSIXct(Output$Dates, origin='1970-01-01', tz="Europe/Paris"))

#aggregate(x=Output$Values, by=list(Output$weekday), FUN=sum)

#list(TimeSeries=Prevision)