library(dplyr)
library(microbenchmark)
library(compare)

backup <- inputVariables

inputVariables$value$TimeSeries[1, "Values"] <- 24
inputVariables$value$TimeSeries[1, "Dates"] <- 1583190010
inputVariables$resolution$TimeSeries[1, "Values"] <- 10
inputVariables$resolution$TimeSeries[1, "Dates"] <- 1583190010
# inputVariables$bestValue$TimeSeries[1, "Values"] <- 25
# inputVariables$bestValue$TimeSeries[1, "Dates"] <- 1583190010

inputVariables$value$TimeSeries[12, "Values"] <- 24
inputVariables$resolution$TimeSeries[12, "Values"] <- 10

# inputVariables$bestValue$TimeSeries <- inputVariables$bestValue$TimeSeries[-2:-4, ]
inputVariables$bestResolution$TimeSeries <- inputVariables$bestResolution$TimeSeries[-2:-4, ]

# inputVariables$bestValue$TimeSeries <- data.frame(Dates=numeric(0), Values=numeric(0))
# inputVariables$bestResolution$TimeSeries <- data.frame(Dates=numeric(0), Values=numeric(0))

main <- function(inputVariables){
    values <- inputVariables$value$TimeSeries
    result <- data.frame(Dates= numeric(0), Values= numeric(0))
    # We look at all incoming values
    for (row in 1:nrow(values)) {
        date <- values[row, "Dates"]
        resolutions <- inputVariables$resolution$TimeSeries
        linked_resolution <- resolutions[resolutions$Dates == date, "Values"]
        if (length(linked_resolution) == 0) {
            # If no corresponding resolution yet, forget it. It will come later on
            next
        }
        best_resolutions <- inputVariables$bestResolution$TimeSeries
        linked_best_resolution <- best_resolutions[best_resolutions$Dates == date, "Values"]
        if (length(linked_best_resolution) == 0) {
            # If no best resolution yet, we have the best value
            result[nrow(result) + 1,] <- list(date, linked_resolution)
        } else if (linked_resolution < linked_best_resolution) {
            result[nrow(result) + 1,] <- list(date, linked_resolution)
        }

    }

    return(list(TimeSeries = result))
}

main2 <- function(inputVariables){

    values <- inputVariables$value$TimeSeries
    resolutions <- inputVariables$resolution$TimeSeries

    best_values <- inputVariables$bestValue$TimeSeries
    best_resolutions <- inputVariables$bestResolution$TimeSeries

    # We keep only entries with a pair value/resolution
    values <- values[values$Dates %in% resolutions$Dates, ]
    resolutions <- resolutions[resolutions$Dates %in% values$Dates, ]

    # On one hand, we have resolutions with unprocessed date. They are new
    new_resolutions <- resolutions[! resolutions$Dates %in% best_resolutions$Dates, ]
    # But because we process them after the vales, we ensure that we have a value
    new_resolutions <- new_resolutions[new_resolutions$Dates %in% best_values$Dates, ]

    # On the other hand, we look for existing dates where the new resolution is better
    # We first align our entries to the best known
    values <- values[values$Dates %in% best_resolutions$Dates, ]
    resolutions <- resolutions[resolutions$Dates %in% best_resolutions$Dates, ]
    best_resolutions <- best_resolutions[best_resolutions$Dates %in% resolutions$Dates, ]
    better_resolutions <- resolutions[resolutions$Values < best_resolutions$Values, ]

    list(TimeSeries = rbind(new_resolutions, better_resolutions))
}

output <- main(inputVariables)$TimeSeries
output2 <- main2(inputVariables)$TimeSeries

compare(output, output2)

microbenchmark(
main(inputVariables),
main2(inputVariables),
times=100
)

inputVariables <- backup