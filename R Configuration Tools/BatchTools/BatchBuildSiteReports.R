BatchBuildSiteReports <- function(opisenseToken, pathConfig, reportFile, nRun)
{
  outputResult <- data.frame()
  kRun <- 0
  opisenseReports <- getOpisenseObject(opisenseToken, 'reports', list())
  #==== Read XLS template ====
  dashboardList <- read.xls(paste0(pathConfig, reportFile),sheet = 'ReportList')
  userList <- read.xls(paste0(pathConfig, reportFile),sheet = 'UserList')
  tileList <- read.xls(paste0(pathConfig, reportFile),sheet = 'TabList')
  sourceList <- read.xls(paste0(pathConfig, reportFile),sheet = 'SourceList')
  #==== Create dashboards ====
  groupSiteId <- getOpisenseObject(opisenseToken, 'sites', list(displayLevel = 'Site', customFilter = "Name = 'COLRUYT GROUP'"))$id
  filter <- list(displayLevel = 'VerboseSite')
  opisenseSites <- getOpisenseObject(opisenseToken, 'sites', filter)
  opisenseSites <- opisenseSites[!is.na(opisenseSites$clientData$`Location Master Data`$`General Information`$LOC_TYP_NAME),]
  filter <- list(displayLevel = 'Site')
  opisenseAccountSources <- getOpisenseObject(opisenseToken, 'sources', filter)
  filter <- list(displayLevel = 'Normal')
  opisenseAccountVariables <- getOpisenseObject(opisenseToken, 'variables', filter)
  kDashboard <- 0
  failedDashboard <- c()
  kFailed <- 1
  for(iDashboard in 1:dim(dashboardList)[1]){
    cloudItem <- getOpisenseObject(opisenseToken, 'storage', list(fileName = as.character(dashboardList$TemplateFile[iDashboard])))
    templateCloudItemId <- max(cloudItem$cloudItems$id)
    users <- list()
    dashboardUsers <- userList[as.character(userList$ID) == as.character(dashboardList$Users[iDashboard]),]
    
    dashboardTiles <- tileList[as.character(tileList$ID) == as.character(dashboardList$Tiles[iDashboard]),]
    
    dashboardSiteGroups <- strsplit(as.character(dashboardList[iDashboard,]$Sites), ';')[[1]]
    for(dashboardSiteGroup in dashboardSiteGroups){
      dashboardSites <- opisenseSites[opisenseSites$clientData$`Location Master Data`$`General Information`$LOC_TYP_NAME == dashboardSiteGroup,]
      for(iSite in 1:dim(dashboardSites)[1]){
        opisenseToken <- getoAuthToken(user, password)
        dashboardSite <- dashboardSites[iSite,]
        
        users <- dashboardUsers[as.integer(dashboardUsers$LOC_ID) == dashboardSite$clientData$`Location Master Data`$`Technical Identifiers`$LOC_ID,]
        notifications <- list()
        for(kUser in 1:dim(users)[1]){
          notifications[[kUser]] <- list(type= as.character(users$type[kUser]),
                                         target= as.character(users$target[kUser]),
                                         format= as.character(users$format[kUser]),
                                         sendAs= as.character(users$sendAs[kUser]),
                                         senderAddress = as.character(users$senderAddress[kUser]),
                                         value= as.character(users$email[kUser]))
        }
        
        dataSources <- list()
        kSource <- 0
        for(iTile in 1:dim(dashboardTiles)[1]){
          dashboardTile <- dashboardTiles[iTile,]
          
          dashboardSources <- sourceList[as.character(sourceList$ID) == as.character(dashboardTile$dataSources),]
          
          for(iSource in 1:dim(dashboardSources)[1]){
            dashboardSource <- dashboardSources[iSource,]
            dataName <- as.character(dashboardSource$name)
            energyTypeId <- as.integer(dashboardSource$energyTypeId)
            siteId <- if(as.integer(dashboardSource$siteId) == 0){dashboardSite$id}else{groupSiteId}
            sourceCsr <- strsplit(as.character(dashboardSource$source),';')
            filterVariables <- strsplit(as.character(dashboardSource$variable),';')[[1]]
            unitId <- as.integer(dashboardSource$unitId)
            for(iCsr in 1:length(sourceCsr[[1]])){
              csr <- sourceCsr[[1]][iCsr]
              if((csr == 'ELE') || (csr == 'WAT') || (csr == 'GAS')){
                opisenseSources <- opisenseAccountSources[(opisenseAccountSources$energyTypeId == energyTypeId) & 
                                                            grepl(dashboardSiteGroup, opisenseAccountSources$name) &
                                                            (opisenseAccountSources$siteId == siteId),]
              }
              else{
                opisenseSources <- opisenseAccountSources[(opisenseAccountSources$energyTypeId == energyTypeId) & 
                                                            (opisenseAccountSources$clientData$`Meter Master Data`$`Other Information`$CSR_TYP_NAME == csr) &
                                                            (opisenseAccountSources$siteId == siteId),]
              }
              if(is.null(opisenseSources) || (dim(opisenseSources)[1] == 0)) next
              for(jSource in 1:dim(opisenseSources)[1]){
                opisenseSource <- opisenseSources[jSource,]
                for(iVariable in 1:length(filterVariables)){
                  opisenseVariable <- opisenseAccountVariables[(opisenseAccountVariables$sourceId == opisenseSource$id) &
                                                                 (tolower(opisenseAccountVariables$name) == tolower(filterVariables[iVariable])),]
                  if(!is.null(opisenseVariable) && (dim(opisenseVariable)[1] != 0)){
                    kSource <- kSource + 1
                    dataSources[[kSource]] <- list(siteId=siteId,
                                                   sheetName = as.character(dashboardTile$title),
                                                   sourceId = opisenseSource$id,
                                                   variableId = opisenseVariable$id,
                                                   unitId = unitId,
                                                   granularity= as.character(dashboardTile$granularity),
                                                   aggregation= as.character(dashboardTile$aggregation),
                                                   periodType= as.integer(dashboardTile$periodType),
                                                   period= as.integer(dashboardTile$period),
                                                   periodTimeBase= as.character(dashboardTile$periodTimeBase),
                                                   type= 'GivenVariable')
                  }
                }
              }
            }
          }
        }
        if(length(dataSources) == 0) next
        dashboardId <- opisenseReports[opisenseReports$name == paste0('Shop Report - ', dashboardSite$name),]$id
        if(length(dashboardId)==0)dashboardId <- 0
        
        dashboard <- list(id= if(dashboardId==0){NA}else{dashboardId},
                          name = paste0('Shop Report - ', dashboardSite$name),
                          templateCloudItemId = templateCloudItemId,
                          frequencyId = as.character(dashboardList$FrequencyId[iDashboard]),
                          nextExecutionDate = as.character(dashboardList$NextExecutionDate[iDashboard]),
                          notifications = notifications,
                          datasources = dataSources,
                          metadataDatasource = list(
                            includeEvents = FALSE,
                            eventsSheetName = 'Events',
                            includeAlerts = FALSE,
                            alertsSheetName = 'Alerts',
                            includeInvoices = FALSE,
                            invoicesSheetName = 'Invoices',
                            includeSites = TRUE,
                            sitesSheetName = 'Sites',
                            includeSources = TRUE,
                            sourcesSheetName = 'Sources',
                            includeSchedules = FALSE,
                            schedulesSheetName = 'Schedules'
                          )
        )
        
        dashboardId <- pushOpisenseObject(opisenseToken,'reports',dashboard,dashboardId)
        kDashboard <- kDashboard + 1
        print(paste0(kDashboard,' :Created Report ', dashboardId, ' on ', dashboardSite$name))
        if(dashboardId == 0){
          kFailed <- kFailed + 1
          failedDashboard[kFailed] <- dashboardSite$id
        }
        outputResult <- rbind(outputResult, data.frame(Dashboard = dashboard$name,
                                                       DashboardId = dashboardId))
        kRun <- kRun + 1
        if(kRun == nRun){
          return(outputResult)
        }
      }
    }
  }
  outputResult
}