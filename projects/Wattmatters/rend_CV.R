load("C:/dev/R/projects/Wattmatters/rend_CV.rdata")

prod_elec <- inputVariables$ProdElec$TimeSeries
prod_heat <- inputVariables$ProdHeat$TimeSeries
cons_gas <- inputVariables$ConsGas$TimeSeries

dates <- intersect(intersect(prod_elec$Dates, prod_heat$Dates), cons_gas$Dates)

coefficient <- ifelse(is.null(inputForms$Coefficient), 1, inputForms$Coefficient)
loss <- ifelse(is.null(inputForms$Pertes), 0, inputForms$Pertes)

list(TimeSeries=data.frame(Dates=dates,
                           Values=(prod_elec[prod_elec$Dates %in% dates, "Values"] / 0.55 +
                                  (prod_heat[prod_heat$Dates %in% dates, "Values"] - loss) / 0.9 -
                                  cons_gas[cons_gas$Dates %in% dates, "Values"]) / 1000 * coefficient))