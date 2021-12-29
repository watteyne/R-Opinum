SetVirtualMeterSpecific <- function(opisenseToken, nRun)
{
  outputResult <- data.frame()
  kRun <- 0
  #==== Scripts ====
  specificScript <- '{
  inputVariables$Consumption$TimeSeries$Values <- inputVariables$Consumption$TimeSeries$Values / inputConstants$Area
inputVariables$Consumption
}'
  normalisedScript <- '{ 
  index <- order(inputVariables$DDR$TimeSeries$Dates)
  ddR <- inputVariables$DDR$TimeSeries[index,]
  t <- ddR$Dates
  
  index <- order(inputVariables$Consumption$TimeSeries$Dates)
  consumption <- inputVariables$Consumption$TimeSeries[index,]
  index <- consumption$Dates %in% t
  consumption <- consumption[index,]
  
  index <- order(inputVariables$DDN$TimeSeries$Dates)
  ddN <- inputVariables$DDN$TimeSeries[index,]
  index <- ddN$Dates %in% t
  ddN <- ddN[index,]
  
  t <- unique(ddR$Dates[ddR$Dates %in% ddN$Dates[ddN$Dates %in% consumption$Dates]])
  ddR <- ddR[ddR$Dates %in% t,]
  ddN <- ddN[ddN$Dates %in% t,]
  consumption <- consumption[consumption$Dates %in% t,]
  
  index <- (ddR$Values < 1) | (ddN$Values < 1)
  ddR$Values[index] <- 1
  ddN$Values[index] <- 1
  
  ddFactor <- ddN$Values/ddR$Values
  index <- is.na(ddFactor)
  ddFactor[index] <- 1
  
  list(TimeSeries = data.frame(Dates = t,
  Values = ddFactor * consumption$Values))
  
}'
  powerScript <- "{
  index <- order(inputVariables$Consumption$TimeSeries$Dates)
  consumption <- inputVariables$Consumption$TimeSeries[index,]
  n <- dim(consumption)[1]
  x <- consumption$Values[seq(2,n,1)]
  t <- consumption$Dates[seq(2,n,1)]
  dt <- diff(consumption$Dates) / 60 / 60
  list(TimeSeries = data.frame(Dates = t,
  Values = x/dt))
}"
  a3RelativeSundayConsumptionScript <- "{
  '#===== Toolbox functions ====='
  FilterNaOutput <- function(result){
  index <- !is.na(result$Values)
  data.frame(Dates = result$Dates[index], Values = result$Values[index])}
  FilterDuplicateOutput <- function(result, filterOutput, doubleComparisonThreshold){
  indexNew <- is.element(result$Dates, filterOutput$Dates)
  indexOld <- is.element(filterOutput$Dates, result$Dates)
  filterOutput <- filterOutput[indexOld,]
  newOutput <- result[indexNew,]
  indexFilter <- abs(filterOutput$Values - newOutput$Values) < doubleComparisonThreshold
  datesFilter <- is.element(result$Dates, result$Dates[indexNew][indexFilter])
  data.frame(Dates = result$Dates[!datesFilter], Values = result$Values[!datesFilter])}
  '#===== Main Function ====='
  tryCatch(
  {
  '#===== Optional parameters ====='
  doubleComparisonThreshold <- 0.0001
  if(!is.null(inputVariables$FilterOutput)){
  index <- order(inputVariables$FilterOutput$TimeSeries$Dates)
  filterOutput <- inputVariables$FilterOutput$TimeSeries[index,]
  inputVariables <- inputVariables[!(names(inputVariables) %in% c('FilterOutput'))]
  }else{filterOutput <- NULL}
  '#===== Formula ====='
  x <- inputVariables$Gas$TimeSeries
  index <- order(x$Dates)
  x <- x[index,]
  t <- x$Dates
  v <- x$Values
  maxIndex <- length(t)
  abnormal <- rep(0, length.out = maxIndex)
  SunIndex <- which(format(as.POSIXct(t, origin = '1970-01-01'), format = '%w') == '0')
  SunIndex <- SunIndex[SunIndex > 6]
  for (counter in SunIndex){
  weekMean <- mean(v[(counter-6):(counter-1)])
  if(weekMean != 0){
  abnormal[counter] <- v[counter]/weekMean*100
  }
  else{
  abnormal[counter] <- 0
  }
  }
  result <- data.frame(Dates = t[SunIndex],
  Values = abnormal[SunIndex])
  '#===== Output ====='
  result <- FilterNaOutput(result)
  if(!is.null(filterOutput)){result <- FilterDuplicateOutput(result, filterOutput, doubleComparisonThreshold)}
  if(dim(result)[1] != 0){list(TimeSeries = result)}else{list(Errors = c('Calculation returned no output'))}
  },
  error = function(x){list(Errors = c(paste(x$message,paste(x$call, collapse = ' '), sep = ':')))},
  warning = function(x){list(Errors = c(paste(x$message,paste(x$call, collapse = ' '), sep = ':')))}
  )
}"
  a13MaxCapacityCheckScript <- "{
  t <- inputVariables$Production$TimeSeries$Dates
  v <- inputVariables$Production$TimeSeries$Values
  maxIndex <- length(t)
  maxProduction <- inputConstants$Boundary*inputForms$WPiek/(1000*12)
  list(TimeSeries = data.frame(Dates = t[2:maxIndex],
  Values = ifelse(v[1:maxIndex-1] > maxProduction,
  0.5,
  0) + 
  ifelse(v[2:maxIndex] > maxProduction,
  0.5,
  0)))
}"
  #==== Update Virtual Meters ====
  specificWaterUnitId <- 70
  specificEnergyUnitId <- 68
  temperatureUnitId <- 28
  powerUnitId <- 4
  percentUnitId <- 31
  eventUnitId <- 64
  
  rmiSiteId <- 17392;
  rmiDdnSourceId <- 27582;
  rmiDdnVariableId <- 290440;
  
  mapRmi <- function(eolyRmi)
  {
    if(length(eolyRmi)==0){
      return(list(sourceId = 27582, variableId = 284285))
    }
    switch(eolyRmi,
           "Retie" = list(sourceId = 27583, variableId = 284286),
           "Deurne" = list(sourceId = 27585, variableId = 284288),
           "Melle" = list(sourceId = 27581, variableId = 284284),
           "Florennes" = list(sourceId = 27586, variableId = 284289),
           "Ukkel" = list(sourceId = 27582, variableId = 284285),
           "Middelkerke" = list(sourceId = 27584, variableId = 284287),
           "Kleine-Brogel" = list(sourceId = 27588, variableId = 284291),
           "Bierset" = list(sourceId = 27590, variableId = 284293),
           "Beauvechain" = list(sourceId = 27587, variableId = 284290),
           "Saint-Hubert" = list(sourceId = 27589, variableId = 284292),
           list(sourceId = 27582, variableId = 284285))
  }
  
  filter = list(displayLevel = 'Site',
                sourceTypeId = 27,
                customFilter = "'CSR_TYP_NAME'='V_TOTAAL'")
  opisenseSources <- getOpisenseObject(opisenseToken, 'sources', filter)
  filter$customFilter <- "'CSR_TYP_NAME'='O_PV'"
  pvSources <- getOpisenseObject(opisenseToken, 'sources', filter)
  
  if(!is.null(dim(pvSources)[1])){
    for(iSource in 1:dim(pvSources)[1]){
      opisenseSource <- pvSources[iSource,]
      filter <- list(displayLevel = 'Normal',
                     sourceId = opisenseSource$id)
      opisenseVariables <- getOpisenseObject(opisenseToken, 'variables', filter)
      opisenseVariable <- opisenseVariables[(opisenseVariables$name == 'Consumption') | (opisenseVariables$name == 'General Consumption') | (opisenseVariables$name == 'General consumption'),]
      a13MaxCapacityCheckVariable <- opisenseVariables[(opisenseVariables$name == 'A13 Max Capacity Check'),]
      a13Id <- if(dim(a13MaxCapacityCheckVariable)[1] != 0){a13MaxCapacityCheckVariable$id}else{0}
      a13InputVariables <- list(list())
      a13InputConstants <- list(list())
      a13InputForms <- list(list())
      targetA13MaxCapacityCheckVariable <- list(id = a13Id,
                                                name = 'A13 Max Capacity Check',
                                                granularity = 5,
                                                granularityTimeBase = 'Minute',
                                                unitId = eventUnitId,
                                                quantityType = 'Maximum',
                                                aggregate = 'MAX',
                                                sourceId = opisenseSource$id)
      a13InputVariables[[1]][[1]] <- list(alias = 'Production',
                                          siteId = opisenseSource$siteId,
                                          sourceId = opisenseSource$id,
                                          variableId = opisenseVariable$id,
                                          granularity = 'Raw',
                                          periodType = 3,
                                          period = 2,
                                          unitId = opisenseVariable$unitId,
                                          canBeUsedAsATrigger = TRUE)
      a13InputConstants[[1]][[1]] <- list(alias = 'Boundary',
                                          value = 1.05)
      a13InputForms[[1]][[1]] <- list(alias = "WPiek",
                                      entityType = 1,
                                      siteId = opisenseSource$siteId,
                                      formId = "59fb0eb3e1abe734b83a4c97",
                                      groupId = 4,
                                      propId = "18")
      a13CalculatedVariable <- buildVariable(targetA13MaxCapacityCheckVariable, a13InputVariables, a13InputConstants, a13InputForms, c(a13MaxCapacityCheckScript))
      if(a13Id == 0){
        a13Id <- pushVariable(opisenseToken, a13CalculatedVariable, a13CalculatedVariable$sourceId, a13CalculatedVariable$id)
        print(paste0('Created a13 max capacity check variable ', a13Id, ' on source ', a13CalculatedVariable$sourceId))
        outputResult <- rbind(outputResult, data.frame(Type = 'A13',
                                                       SourceId = a13CalculatedVariable$sourceId,
                                                       VariableId = a13Id))
      }
    }
  }
  
  if(is.null(dim(opisenseSources)[1])){
    return(outputResult)
  }
  
  for(iSource in 1:dim(opisenseSources)[1]){
    opisenseSource <- opisenseSources[iSource,]
    filter <- list(displayLevel = 'Normal',
                   sourceId = opisenseSource$id)
    opisenseVariables <- getOpisenseObject(opisenseToken, 'variables', filter)
    opisenseVariable <- opisenseVariables[(opisenseVariables$name == 'Consumption') | (opisenseVariables$name == 'General Consumption') | (opisenseVariables$name == 'General consumption'),]
    specificVariable <- opisenseVariables[(opisenseVariables$name == 'Specific Consumption') | (opisenseVariables$name == 'Specific Normalised Consumption'),]
    normalisedVariable <- opisenseVariables[(opisenseVariables$name == 'Normalised Consumption'),]
    powerVariable <- opisenseVariables[(opisenseVariables$name == 'Power'),]
    a3RelativeSundayConsumptionVariable <- opisenseVariables[(opisenseVariables$name == 'A3 Relative Sunday Consumption'),]
    
    specificId <- if(dim(specificVariable)[1] != 0){specificVariable$id}else{0}
    normalisedId <- if(dim(normalisedVariable)[1] != 0){normalisedVariable$id}else{0}
    powerId <- if(dim(powerVariable)[1] != 0){powerVariable$id}else{0}
    a3Id <- if(dim(a3RelativeSundayConsumptionVariable)[1] != 0){a3RelativeSundayConsumptionVariable$id}else{0}
    
    filter <- list(displayLevel = 'VerboseSite',
                   siteIds = c(opisenseSource$siteId))
    opisenseSite <- getOpisenseObject(opisenseToken, 'sites', filter)
    siteArea <- as.numeric(opisenseSite$clientData$`Location Master Data`$`Technical Information`$`OPPERVLAKTE NETTO`)
    eolyRmi <- as.character(opisenseSite$clientData$`Location Master Data`$`Technical Information`$KMI_ZONE)
    siteRmi <- mapRmi(eolyRmi)
    specificUnitId <- if(opisenseSource$energyTypeId == 3){specificWaterUnitId}else{specificEnergyUnitId}
    targetSpecificVariable <- list(id = specificId,
                                   name = 'Specific Consumption',
                                   unitId = specificUnitId, 
                                   granularity = 5, 
                                   granularityTimeBase = 'Minute',
                                   quantityType = 'Integrated',
                                   aggregate = 'SUM',
                                   sourceId = opisenseSource$id)
    targetPowerVariable <- list(id = specificId,
                                   name = 'Power',
                                   unitId = powerUnitId, 
                                   granularity = 5, 
                                   granularityTimeBase = 'Minute',
                                   quantityType = 'Averaged',
                                   aggregate = 'AVG',
                                   sourceId = opisenseSource$id)
    inputVariables <- list(list())
    inputConstants <- list(list())
    a3InputVariables <- list(list())
    if(opisenseSource$energyTypeId == 2){
      targetA3RelativeSundayConsumptionVariable <- list(id = a3Id,
                                                        name = 'A3 Relative Sunday Consumption',
                                                        granularity = 1,
                                                        granularityTimeBase = 'Day',
                                                        unitId = percentUnitId,
                                                        quantityType = 'Averaged',
                                                        aggregate = 'AVG',
                                                        sourceId = opisenseSource$id)
      a3InputVariables[[1]][[1]] <- list(alias = 'Gas',
                                         siteId = opisenseSource$siteId,
                                         sourceId = opisenseSource$id,
                                         variableId = opisenseVariable$id,
                                         granularity = 'Day',
                                         periodType = 1,
                                         period = 2,
                                         periodTimeBase = 'Week',
                                         aggregation = 'SUM',
                                         unitId = opisenseVariable$unitId,
                                         canBeUsedAsATrigger = TRUE)
      a3CalculatedVariable <- buildVariable(targetA3RelativeSundayConsumptionVariable, list(list()), list(list()), list(list()), c(''))
      if(a3Id == 0){
        a3Id <- pushVariable(opisenseToken, a3CalculatedVariable, a3CalculatedVariable$sourceId, a3CalculatedVariable$id)
        targetA3RelativeSundayConsumptionVariable$id <- a3Id
        a3InputVariables[[1]][[2]] <- list(alias = 'FilterOutput',
                                           siteId = opisenseSource$siteId,
                                           sourceId = opisenseSource$id,
                                           variableId = a3Id,
                                           granularity = 'Day',
                                           periodType = 1,
                                           period = 2,
                                           periodTimeBase = 'Week',
                                           aggregation = 'SUM',
                                           unitId = percentUnitId,
                                           canBeUsedAsATrigger = FALSE)
        a3CalculatedVariable <- buildVariable(targetA3RelativeSundayConsumptionVariable, a3InputVariables, list(list()), list(list()), c(a3RelativeSundayConsumptionScript))
        a3Id <- pushVariable(opisenseToken, a3CalculatedVariable, a3CalculatedVariable$sourceId, a3CalculatedVariable$id)
        print(paste0('Created a3 relative sunday consumption variable ', a3Id, ' on source ', a3CalculatedVariable$sourceId))
        outputResult <- rbind(outputResult, data.frame(Type = 'A3',
                                                       SourceId = a3CalculatedVariable$sourceId,
                                                       VariableId = a3Id))
      }
      
      targetSpecificVariable$name <- 'Specific Normalised Consumption'
      targetSpecificVariable$granularity <- 1
      targetSpecificVariable$granularityTimeBase <- 'Day'
      
      targetNormalisedVariable <- list(id = normalisedId,
                                       name = 'Normalised Consumption',
                                       unitId = opisenseVariable$unitId, 
                                       granularity = 1, 
                                       granularityTimeBase = 'Day',
                                       quantityType = 'Integrated',
                                       aggregate = 'SUM',
                                       sourceId = opisenseSource$id)
      inputVariables[[1]][[1]] <- list(alias = 'Consumption',
                                       siteId = opisenseSource$siteId,
                                       sourceId = opisenseSource$id,
                                       variableId = opisenseVariable$id,
                                       granularity = 1,
                                       periodType = 1,
                                       period = 3,
                                       periodTimeBase = 'Day',
                                       aggregation = 'SUM',
                                       unitId = opisenseVariable$unitId,
                                       canBeUsedAsATrigger = FALSE)
      inputVariables[[1]][[2]] <- list(alias = 'DDR',
                                       siteId = rmiSiteId,
                                       sourceId = siteRmi$sourceId,
                                       variableId = siteRmi$variableId,
                                       granularity = 1,
                                       periodType = 1,
                                       period = 1,
                                       periodTimeBase = 'Day',
                                       aggregation = 'SUM',
                                       unitId = temperatureUnitId,
                                       canBeUsedAsATrigger = TRUE)
      inputVariables[[1]][[3]] <- list(alias = 'DDN',
                                       siteId = rmiSiteId,
                                       sourceId = rmiDdnSourceId,
                                       variableId = rmiDdnVariableId,
                                       granularity = 1,
                                       periodType = 1,
                                       period = 3,
                                       periodTimeBase = 'Day',
                                       aggregation = 'SUM',
                                       unitId = temperatureUnitId,
                                       canBeUsedAsATrigger = FALSE)
      calculatedVariable <- buildVariable(targetNormalisedVariable, inputVariables, list(list()), list(list()), c(normalisedScript))
      if(normalisedId == 0){
        normalisedId <- pushVariable(opisenseToken, calculatedVariable, calculatedVariable$sourceId, calculatedVariable$id)
        print(paste0('Created normalised variable ', normalisedId, ' on source ', calculatedVariable$sourceId))
        outputResult <- rbind(outputResult, data.frame(Type = 'Normalised',
                                                       SourceId = calculatedVariable$sourceId,
                                                       VariableId = normalisedId))
      }
      
      inputVariables <- list(list())
      inputVariables[[1]][[1]] <- list(alias = 'Consumption',
                                       siteId = opisenseSource$siteId,
                                       sourceId = opisenseSource$id,
                                       variableId = normalisedId,
                                       granularity = 0,
                                       periodType = 3,
                                       period = 1,
                                       unitId = opisenseVariable$unitId,
                                       canBeUsedAsATrigger = TRUE)
    }
    else{
      
      inputVariables[[1]][[1]] <- list(alias = 'Consumption',
                                       siteId = opisenseSource$siteId,
                                       sourceId = opisenseSource$id,
                                       variableId = opisenseVariable$id,
                                       granularity = 0,
                                       periodType = 3,
                                       period = 1,
                                       unitId = opisenseVariable$unitId,
                                       canBeUsedAsATrigger = TRUE)
    }
    if(length(siteArea) != 0){
      inputConstants[[1]][[1]] <- list(value = siteArea,
                                       alias = 'Area')
      calculatedVariable <- buildVariable(targetSpecificVariable, inputVariables, inputConstants, list(list()), c(specificScript))
      if(specificId == 0){
        specificId <- pushVariable(opisenseToken, calculatedVariable, calculatedVariable$sourceId, calculatedVariable$id)
        print(paste0('Created specific variable ', specificId, ' on source ', calculatedVariable$sourceId))
        outputResult <- rbind(outputResult, data.frame(Type = 'Specific',
                                                       SourceId = calculatedVariable$sourceId,
                                                       VariableId = specificId))
      }
    }
    if(opisenseSource$energyTypeId == 1){
      calculatedVariable <- buildVariable(targetPowerVariable, inputVariables, list(list()), list(list()), c(powerScript))
      if(powerId == 0){
        powerId <- pushVariable(opisenseToken, calculatedVariable, calculatedVariable$sourceId, calculatedVariable$id)
        print(paste0('Created power variable ', powerId, ' on source ', calculatedVariable$sourceId))
        outputResult <- rbind(outputResult, data.frame(Type = 'Power',
                                                       SourceId = calculatedVariable$sourceId,
                                                       VariableId = powerId))
      }
    }
    kRun <- kRun + 1
    if(kRun == nRun){
      return(outputResult)
    }
  }
  outputResult
}