load("c:/dev/R/projects/agregio/cost_bias/sampleData.rdata")

library(lubridate);
library(dplyr)

{ for (var_name in names(inputVariables)) {
  df <- inputVariables[[var_name]]$TimeSeries
  df$Dates <- as.POSIXct(df$Dates, origin='1970-01-01', tz='UTC')
  df$Month <- floor_date(df$Dates, "month")
  inputVariables[[var_name]]$TimeSeries <- df
}
}

dates <- names(inputVariables) %>%
  inputVariables[[var_name]]$TimeSeries$Dates %>%
  reduce(intersect)

