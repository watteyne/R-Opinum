library(dplyr)

dates <- Reduce(intersect, inputVariables %>%
  lapply(
    function(x){
      x$TimeSeries$Dates
    }))

values <- Reduce('+', inputVariables %>%
  lapply(
    function(x){
      x$TimeSeries %>%
        filter(Dates %in% dates) %>%
        select(Values)
    }))

list(TimeSeries=data.frame(Dates=dates, Values=values))
