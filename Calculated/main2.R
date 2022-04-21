load('c:/dev/R/Calculated/sampleData.rdata')

library(dplyr)
library(purrr)

all_names <- names(inputVariables)

volume_df <- all_names[all_names != "self"] %>%
  lapply(
    function(x){
      inputVariables[[x]]$TimeSeries %>%
        mutate(Values = Values * ifelse(endsWith(x, "BVolume"), 1, -1)) %>%
        rename(!!x := Values)
    }) %>%
  reduce(full_join, by = "Dates") %>%
  replace(is.na(.),0) %>%
  mutate(Values = rowSums(select(., -Dates))) %>%
  select(Dates, Values)

self <- inputVariables$self$TimeSeries
result <- volume_df[! volume_df$Dates %in% self$Dates, ]

overlap <- volume_df[volume_df$Dates %in% self$Dates, ]
self <- self[self$Dates %in% overlap$Dates, ]

overlap <- overlap[abs(overlap$Values - self$Values) > 0.0001, ]

list(TimeSeries=rbind(result, overlap))
