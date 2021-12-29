BatchBuildAlerts <- function(opisenseToken, pathConfig, alertFile, nRun)
{
  outputResult <- data.frame()
  kRun <- 0
  #==== Read XLS template ====
  alertsList <- read.xls(paste0(pathConfig,alertFile),sheet = 'Settings')
  scheduleLevelsList <- read.xls(paste0(pathConfig,alertFile),sheet = 'ScheduleLevelLists')
  notificationList <- read.xls(paste0(pathConfig,alertFile),sheet = 'NotificationLists')
  #==== Functions ====
  getEnergyName <- function(energyId){
    switch(as.character(alertsList$EnergyType[iAlert]),
           '0'='Solar',
           '1'='Electricity',
           '2'='Gas',
           '3'='Water',
           '4'='Fuel',
           '5'='Heat',
           '18'='Cold')
  }
  #==== Get Opisense data ====
  filter <- list(displayLevel = 'Site')
  opisenseAlerts <- getOpisenseObject(opisenseToken, 'alerts', filter)
  opisenseSites <- getOpisenseObject(opisenseToken, 'sites', filter)
  opisenseSources0 <- getOpisenseObject(opisenseToken, 'sources', filter)
  index <- opisenseSources0$sourceTypeId == 77 & opisenseSources0$siteName != 'Unassigned'
  opisenseSources0 <- opisenseSources0[index,]
  filter$displayLevel <- 'Normal'
  opisenseVariables <- getOpisenseObject(opisenseToken, 'variables', filter)
  opisenseSchedules <- getOpisenseObject(opisenseToken, 'schedule',c())
  #==== Create alerts based on definition file ====
  for (iAlert in 1:dim(alertsList)[1]){
    alertType <- as.character(alertsList$alertType[iAlert])
    
    alertThresholdType <- NA
    granularity <- NA
    granularityTimeBase <- NA
    value <- NA
    aggregationType <- NA
    scheduleInterval <- NA
    scheduleIntervalTimeBase <- NA
    scheduleNextExecutionDate <- NA
    scheduleConfig <- NA
    
    switch(alertType,
           "Raw"={
             alertThresholdType <- as.character(alertsList$alertThresholdType[iAlert])
             value <- as.numeric(alertsList$value[iAlert])
           },
           "Rolling"={
             alertThresholdType <- as.character(alertsList$alertThresholdType[iAlert])
             granularity <- as.numeric(alertsList$granularity[iAlert])
             granularityTimeBase <- as.character(alertsList$granularityTimeBase[iAlert])
             value <- as.numeric(alertsList$value[iAlert])
             aggregationType <- as.character(alertsList$aggregationType[iAlert])
           },
           "Scheduled"={
             alertThresholdType <- as.character(alertsList$alertThresholdType[iAlert])
             granularity <- as.numeric(alertsList$granularity[iAlert])
             granularityTimeBase <- as.character(alertsList$granularityTimeBase[iAlert])
             value <- as.numeric(alertsList$value[iAlert])
             aggregationType <- as.character(alertsList$aggregationType[iAlert])
             scheduleInterval <- as.numeric(alertsList$scheduleInterval[iAlert])
             scheduleIntervalTimeBase <- as.character(alertsList$scheduleIntervalTimeBase[iAlert])
             scheduleNextExecutionDate <- as.character(alertsList$scheduleNextExecutionDate[iAlert])
           })
    
    indexSources <- opisenseSources0$energyTypeId==as.numeric(alertsList$EnergyType[iAlert])
    opisenseSources <- opisenseSources0[indexSources,]
    filterSources <- strsplit(as.character(alertsList$Source[iAlert]),';')
    conventionInfo <- strsplit(as.character(alertsList$Naming[iAlert]),';')
    for (iSource in 1:length(filterSources)){
      for (k in 1:dim(opisenseSources)[1]){
        opisenseSource <- opisenseSources[k,]
        if (sum(opisenseSource$tags[[1]]==filterSources[[1]][iSource])==1){
          siteName <- opisenseSites[opisenseSites$id == opisenseSource$siteId,]$name
          variableId <- opisenseVariables[(opisenseVariables$sourceId == opisenseSource$id) & (opisenseVariables$name == as.character(alertsList$Variable[iAlert])),]$id
          
          sL <- scheduleLevelsList[scheduleLevelsList$ID == as.character(alertsList$ScheduleLevelList[iAlert]),]
          if (as.character(alertsList$ScheduleName[iAlert]) != ""){
            scheduleId <- opisenseSchedules$id[opisenseSchedules$name == as.character(alertsList$ScheduleName[iAlert])]
            scheduleLevels <- getOpisenseScheduleLevels(opisenseToken, scheduleId)
            scheduleLevelIds <- scheduleLevels[scheduleLevels$name %in% sL$Level,]$id
            if (length(scheduleLevelIds)!=0){
              scheduleConfig <- list(scheduleId = scheduleId,
                                     scheduleLevelIds = scheduleLevelIds)  
            }
          }
          energyName <- getEnergyName(as.character(alertsList$EnergyType[iAlert]))
          
          friendlyName <- switch(conventionInfo[[1]][1],
                                 'Convention_1'={paste0(as.character(alertsList$Type[iAlert]),' ',siteName,' ',energyName,' ',conventionInfo[[1]][2])},
                                 'Convention_2'={paste0(as.character(alertsList$Type[iAlert]),' ',siteName,' ',conventionInfo[[1]][2])},
                                 'Convention_3'={paste0(as.character(alertsList$Type[iAlert]),' ',siteName,' ',opisenseSource$name,' ',conventionInfo[[1]][2])})
          
          notifications <- list()
          nL <- notificationList[notificationList$ID == as.character(alertsList$NotificationList[iAlert]),]
          
          for (iList in 1:dim(nL)[1]){
            notifications[[iList]] <- list(
              type= as.character(nL$type[iList]),
              target= as.character(nL$target[iList]),
              value= as.character(nL$value[iList])
            )
          }
          
          alertId <- opisenseAlerts[opisenseAlerts$friendlyName == friendlyName,]$id
          
          alert <- list(
            id= {if(length(alertId) != 0){alertId[1]}else{NA}},
            sourceId= opisenseSource$id,
            siteId= opisenseSource$siteId,
            variableId= variableId,
            friendlyName= friendlyName,
            alertThresholdType= alertThresholdType,
            granularity= granularity,
            granularityTimeBase= granularityTimeBase,
            value= value,
            state= as.character(alertsList$State[iAlert]),
            type= alertType,
            aggregationType= aggregationType,
            scheduleInterval= scheduleInterval,
            scheduleIntervalTimeBase= scheduleIntervalTimeBase,
            scheduleNextExecutionDate= scheduleNextExecutionDate,
            notifications= notifications,
            scheduleConfig= scheduleConfig
          )
          
          alertIdNew <- pushOpisenseObject(opisenseToken,'alerts',alert,alertId)
          if(length(alertId)==0 && alertIdNew!=0){
            print(paste0('Created: ', friendlyName,' with id: ', alertIdNew))
          }
          else if(length(alertId)==0 && alertIdNew==0){
            print(paste0('Failed to create: ', friendlyName))
          }
          else if(alertId == alertIdNew){
            print(paste0('Updated: ', friendlyName,' with id: ', alertIdNew))
          }
          else if(length(alertId)!=0 && alertIdNew==0){
            print(paste0('Failed to update: ', friendlyName,' with id: ', alertId))
          }
          outputResult <- rbind(outputResult, data.frame(Alert = friendlyName,
                                                         AlertId = alertIdNew))
          kRun <- kRun + 1
          if(kRun == nRun){
            return(outputResult)
          }
        }
      }
    }
  }
  outputResult
}





