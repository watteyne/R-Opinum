load("projects/GEG/valorisation/sampleData.rdata")

library(lubridate)
library(dplyr)
library(stringr)

append_prices <- function(suffix) {
  Reduce(rbind, na.omit(names(inputVariables) %>%
    str_extract(paste0('OFFRE\\d', suffix))) %>%
    lapply(
      function(name) {
        df <- inputVariables[[name]]$TimeSeries
        df$Dates <- as.POSIXct(df$Dates, origin = "1970-01-01", tz=inputVariables[[name]]$Variable$SourceTimeZoneId)
        if (nrow(df) > 0) {
          df$offer <- as.integer(str_extract(name, '\\d'))
          colnames(df)[which(colnames(df) == 'Values')] <- suffix
        }
        df
      }
    ) )
}

fixed_prices <- append_prices('ABO')
variable_prices <- append_prices('CONSO')

index <- inputVariables$INDEX$TimeSeries
index$Dates <- as.POSIXct(index$Dates, origin = "1970-01-01", tz="UTC")

dates <- seq(ceiling_date(index$Dates[1], unit='month'),
             floor_date(index$Dates[length(index$Dates)], unit='month'),
             by = "month")
dates <- force_tz(dates, tz='Europe/Paris')

conso <- approx(index$Dates, index$Values, dates)
conso <- data.frame(Dates=conso$x[-1], Values=diff(conso$y))

offer <- inputVariables$OFFRE$TimeSeries
offer <- approx(offer$Dates, offer$Values, dates, method = "constant", f=0, rule=2)
names(offer) <- c('Dates', 'offer')

conso <- merge(conso, offer, by='Dates')

conso <- merge(conso, fixed_prices, by=c('Dates', 'offer'))
conso <- merge(conso, variable_prices, by=c('Dates', 'offer'))

conso$Values <- conso$Values * conso$CONSO + conso$ABO

list(TimeSeries=conso)
