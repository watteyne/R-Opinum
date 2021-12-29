BatchAggregations <- function(opisenseToken, pathConfig, aggregationFile, nRun)
{
  outputResult <- data.frame()
  kRun <- 0
  # =========== PARAMETERS =====
  siteId <- getOpisenseObject(opisenseToken, 'sites', list(displayLevel = 'Site', customFilter = "Name = 'COLRUYT GROUP'"))$id
  sourceTypeId <- 50
  gatewayId <- 585
  timeZoneId <- "Romance Standard Time"
  variableTypeId <- 131
  granularity <- 5
  granularityTimeBase <- "Minute"
  siteTypeId <- 15
  #==== Get account config file ====
  virtualMeters <- read.xls(paste0(pathConfig,aggregationFile),sheet = 'Config')
  filterDefinitions <- read.xls(paste0(pathConfig,aggregationFile),sheet = 'Filters')
  nVM <- dim(virtualMeters)[1]
  nFilter <- dim(virtualMeters)[2]
  jFilter0 <- 7
  filterNames <- names(virtualMeters[,jFilter0:nFilter])
  for(kFilter in 1:(nFilter-jFilter0+1)){
    filterDefinition <- filterDefinitions[as.character(filterDefinitions$Name) == filterNames[kFilter],]
    filterNames[kFilter] <- switch (as.character(filterDefinition$Level),
                                    'Site' = paste0('Site{',as.character(filterDefinition$Form),"='FilterValue'}"),
                                    'Source' = paste0(as.character(filterDefinition$Form),"='FilterValue'"))
  }
  #==== Aggregation Formula ====
  #Add aggregation script here and selection key in Lists tab of EolyAggregations.xlsx file for other type of processing (+ update switch accordingly further down)
  scriptFormulaSum <- '{timeSeries <- sapply(inputVariables, function(x){data.frame(Dates = x$TimeSeries$Dates, Values = x$TimeSeries$Values)})
  timeSeries <- data.frame(Dates = unlist(timeSeries[1,], use.names = FALSE), Values = unlist(timeSeries[2,], use.names = FALSE))
output <- aggregate(timeSeries$Values,by=list(Dates=timeSeries$Dates),FUN=sum)
output <- output[2:(dim(output)[1]-1),]
list(TimeSeries = data.frame(Dates = output$Dates, Values = output$x))}'
  scriptFormulaAvg <- '{timeSeries <- sapply(inputVariables, function(x){data.frame(Dates = x$TimeSeries$Dates, Values = x$TimeSeries$Values)})
  timeSeries <- data.frame(Dates = unlist(timeSeries[1,], use.names = FALSE), Values = unlist(timeSeries[2,], use.names = FALSE))
output <- aggregate(timeSeries$Values,by=list(Dates=timeSeries$Dates),FUN=mean)
output <- output[2:(dim(output)[1]-1),]
list(TimeSeries = data.frame(Dates = output$Dates, Values = output$x))}'
  formulaSum <- c(scriptFormulaSum)
  formulaAvg <- c(scriptFormulaAvg)
  #==== Set virtual meters ====
  for(iVM in 1:nVM){
    sourceName <- as.character(virtualMeters$Name[iVM])
    print(paste0("Processing ", sourceName))
    filter <- list(displayLevel = 'Normal', name = sourceName)
    sourceId <- getOpisenseObject(opisenseToken, 'sources', filter)$id
    filter <- list(displayLevel = 'Normal', sourceId = sourceId)
    energyTypeId <- as.integer(virtualMeters$EnergyTypeId[iVM])
    targetVariableName <- as.character(virtualMeters$Output[iVM])
    aggregationFormula <- as.character(virtualMeters$Script[iVM])
    scriptFormula <- switch (aggregationFormula,
                             'SUM' = formulaSum,
                             'AVG' = formulaAvg
    )
    
    if(length(sourceId) != 0){
      sourceVariables <- getOpisenseObject(opisenseToken, 'variables', filter)
      variableId <- sourceVariables[sourceVariables$name == targetVariableName,]$id
    }
    else{
      variableId <- 0
    }
    
    variableNames <- strsplit(as.character(virtualMeters$Variable[iVM]),';')[[1]]
    customFilter <- paste0('EnergyTypeId=',energyTypeId, ' AND ')
    for(jFilter in jFilter0:nFilter){
      filterValue <- strsplit(as.character(virtualMeters[iVM, jFilter]),';')[[1]]
      customFilter <- paste0(customFilter, if(jFilter == jFilter0){'('}else{' AND ('})
      for(kValue in 1:length(filterValue)){
        customFilter <- paste0(customFilter, if(kValue != 1){' OR '}else{''}, gsub('FilterValue',filterValue[kValue],filterNames[jFilter-jFilter0+1]))
      }
      customFilter <- paste0(customFilter, ')')
    }
    
    filter <- list(displayLevel = 'Site',
                   customFilter = customFilter)
    opisenseSources <- getOpisenseObject(opisenseToken, 'sources', filter)
    inputVariables <- list(list())
    if(is.null(dim(opisenseSources)[1])){
      print(paste0("No matching sources"))
      next
    }
    print(paste0("Adding ", dim(opisenseSources)[1], " sources"))
    iInputVariable <- 0
    for(iSource in 1:dim(opisenseSources)[1]){
      opisenseSource <- opisenseSources[iSource,]
      filter <- list(displayLevel = 'Normal',
                     sourceId = opisenseSource$id)
      opisenseVariables <- getOpisenseObject(opisenseToken, 'variables', filter)
      opisenseVariable <- opisenseVariables[opisenseVariables$name %in% variableNames,]
      if(dim(opisenseVariable)[1] == 0){
        next
      }
      iInputVariable <- iInputVariable + 1
      canBeUsedAsATrigger <- if(iInputVariable == 1){TRUE}else{FALSE}
      inputVariables[[1]][[iInputVariable]] <- list(alias = paste0('s',opisenseSource$id,'v',opisenseVariable$id),
                                                    siteId = opisenseSource$siteId,
                                                    sourceId = opisenseSource$id,
                                                    variableId = opisenseVariable$id,
                                                    granularity = 3,
                                                    periodType = 1,
                                                    period = 3,
                                                    periodTimeBase = 4,
                                                    aggregation = 'SUM',
                                                    unitId = opisenseVariable$unitId,
                                                    canBeUsedAsATrigger = canBeUsedAsATrigger)
    }
    if(length(inputVariables[[1]]) == 0){
      print(paste0("No matching variables"))
      next
    }
    
    if(length(sourceId) == 0){
      source <- list(siteId = siteId,
                     sourceTypeId = sourceTypeId,
                     energyTypeId = energyTypeId,
                     gatewayId = gatewayId,
                     timeZoneId = timeZoneId,
                     displayVariableTypeId = variableTypeId,
                     name = sourceName,
                     description = customFilter)
      sourceId <- pushOpisenseObject(opisenseToken, 'sources', source, c())
      print(paste0("Created Source ", sourceId))
    }
    
    targetVariable <- list(id = if(length(variableId) == 0){0}else{variableId},
                           name = targetVariableName,
                           variableTypeId = variableTypeId, 
                           unitId = inputVariables[[1]][[1]]$unitId, 
                           granularity = 1, 
                           granularityTimeBase = 'Day',
                           quantityType = 'Integrated',
                           aggregate = 'SUM',
                           sourceId = sourceId)
    calculatedVariable <- buildVariable(targetVariable,inputVariables,list(list()),list(list()),scriptFormula)
    variableId <- pushVariable(opisenseToken, calculatedVariable, calculatedVariable$sourceId, calculatedVariable$id)
    if(targetVariable$id == 0){print(paste0("Created Variable ", variableId))}else{print(paste0("Updated Variable ", variableId))}
    outputResult <- rbind(outputResult, data.frame(SourceId = sourceId,
                                                   VariableId = variableId))
    kRun <- kRun + 1
    if(kRun == nRun){
      return(outputResult)
    }
  }
  outputResult
}
