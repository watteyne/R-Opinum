load('projects/agregio/sampleData.rdata')

library(dplyr)

main <- function() {

  result <- data.frame(Dates = numeric(0), Values = numeric(0))

  for (name in names(inputVariables)) {
    if (name != 'Metronome') {
      capacity <- inputVariables[[name]]$TimeSeries
      result <- merge(result, capacity, by = 'Dates', all = TRUE)
      result$Values <- coalesce(result$Values.x, 0) + coalesce(result$Values.y, 0)
      result <- result[c('Dates', 'Values')]
    }
  }

  return(list(TimeSeries = result))
}

main()