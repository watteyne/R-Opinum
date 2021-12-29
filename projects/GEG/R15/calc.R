load("projects/GEG/R15/sampleData.rdata")

result <- inputVariables$REEL$TimeSeries
priority_2 <- inputVariables$ESTIME$TimeSeries
priority_3 <- inputVariables$REGULARISE$TimeSeries

result <- rbind(result, priority_2[! priority_2$Dates %in% result$Dates, ])
result <- rbind(result, priority_3[! priority_3$Dates %in% result$Dates, ])

result