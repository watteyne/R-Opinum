load("C:/dev/R/projects/Wattmatters/gas_consumption.rdata")

cons <- inputVariables$cons$TimeSeries

pcs <- inputVariables[[ifelse(is.null(inputForms$pcs), "Dilbeek", inputForms$pcs)]]$TimeSeries
last_pcs <- pcs$Values[length(pcs$Values)]

cons$Values <- cons$Values * last_pcs

list(TimeSeries=cons)

