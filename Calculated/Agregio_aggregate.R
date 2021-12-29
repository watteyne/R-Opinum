library(lubridate)
library(dplyr)

main <- function() {
  with_data_names <- c()
  start_date <- numeric(0)
  end_date <- numeric(0)
  for (name in names(inputVariables)) {
    if (startsWith(name, 'p')) {
      var_dates <- inputVariables[[name]]$TimeSeries$Dates
      inputVariables[[name]]$TimeSeries$Dates <- as.POSIXct(inputVariables[[name]]$TimeSeries$Dates,
                                                            origin='1970-01-01',
                                                            tz='UTC')
      if (length(var_dates) > 0) {
        with_data_names <- c(with_data_names, name)
        start_date <- min(start_date, var_dates[1])
        end_date <- max(end_date, var_dates[length(var_dates)])
        inputVariables[[name]]$TimeSeries$Month <- floor_date(inputVariables[[name]]$TimeSeries$Dates,
                                                              unit='month')
      }
    }
  }

  meaningful_data_names <- gsub('p', 'm', with_data_names)
  for (name in meaningful_data_names) {
    inputVariables[[name]]$TimeSeries$Dates <- as.POSIXct(inputVariables[[name]]$TimeSeries$Dates,
                                                          origin='1970-01-01',
                                                          tz='UTC')
  }

  result <- data.frame(Dates=numeric(0), Values=numeric(0))

  if (length(start_date) == 0) {
    return(list(TimeSeries=result))
  }

  for (month in seq(floor_date(as.POSIXct(start_date, origin='1970-01-01', tz='UTC'), unit='month'),
                    floor_date(as.POSIXct(end_date, origin='1970-01-01', tz='UTC'), unit='month'),
                    by='month')) {
    month_prod <- data.frame(Dates=numeric(0), Values=numeric(0))
    # print(as.POSIXct(month, origin='1970-01-01'))
    for (i in 1:length(with_data_names)) {
      meaningful <- inputVariables[[meaningful_data_names[i]]]$TimeSeries
      # print(meaningful_data_names[i])
      if (nrow(meaningful[(meaningful$Dates == month) & (meaningful$Values == 1), ]) > 0) {
        prod <- inputVariables[[with_data_names[i]]]$TimeSeries
        prod <- prod[prod$Month == month, ]
        # print(prod$Dates[1])
        if ((length(prod$Dates) > 0) & (day(prod$Dates[1]) <= 10)) {
            if (nrow(month_prod) > 0) {
              month_prod <- month_prod[month_prod$Dates %in% prod$Dates, ]
              month_prod$Values <- month_prod$Values +  prod[prod$Dates %in% month_prod$Dates, 'Values']
            } else {
              month_prod <- prod
            }
        }
      }
    }
    result <- rbind(result, month_prod)
  }
  return(list(TimeSeries=result))
}

main()