library(lubridate)
library(dplyr)

main <- function() {
  if (inputForms$aggregate == "AGG_A") {
    var_names <- c('eldCoverage0', 'measuredCoverage0')
  } else {
    var_names <- c('rp13Coverage0', 'rp12Coverage0', 'measuredCoverage0')
  }

  start_active <- floor_date(with_tz(as.POSIXct(inputForms$ActiveStart), tz='UTC'), unit='month')
  stop_active <- floor_date(with_tz(as.POSIXct(inputForms$ActiveStop), tz='UTC'), unit='month')

  result <- data.frame(Dates=seq(start_active, stop_active, by='month'),
                       Values=0)

  for (name in var_names) {
    coverage <- inputVariables[[name]]$TimeSeries
    coverage <- coverage[coverage$Values > 90, ]
    covered_dates <- floor_date(as.POSIXct(coverage$Dates, origin='1970-01-01', tz='UTC'), unit='month')
    result[result$Dates %in% covered_dates, 'Values'] <- 1
  }

  return(list(TimeSeries=result))
}

main()