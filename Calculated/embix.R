library(dplyr)

dates <- Reduce(intersect, inputVariables %>%
  lapply(
    function(x){
      x$TimeSeries$Dates
    }))

values <- inputVariables %>%
  lapply(
    function(x){
      x$TimeSeries %>%
        filter(Dates %in% dates) %>%
        select(Values)
    })
