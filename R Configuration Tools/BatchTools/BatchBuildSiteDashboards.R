BatchBuildSiteDashboards <- function(opisenseToken, pathConfig, dashboardFile, nRun)
{
  #TEMP
  #opisenseTokenPower <- getoAuthToken('EolyMasterPROD', 'Eoly_!9b3feabbd5')
  opisenseTokenPower <- opisenseToken
  #TEMP
  outputResult <- data.frame()
  kRun <- 0
  accountUsers0 <- getOpisenseObject(opisenseToken, 'account/users',list(pageNumber=0, itemsPerPage=10000))$users$id
  #==== Read XLS template ====
  dashboardList <- read.xls(paste0(pathConfig, dashboardFile),sheet = 'DashboardList')
  userList <- read.xls(paste0(pathConfig, dashboardFile),sheet = 'UserList')
  tileList <- read.xls(paste0(pathConfig, dashboardFile),sheet = 'TileList')
  sourceList <- read.xls(paste0(pathConfig, dashboardFile),sheet = 'SourceList')
  colorList <- read.xls(paste0(pathConfig, dashboardFile),sheet = 'ColorList')
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
    
    users <- list()
    dashboardUsers <- userList[as.character(userList$ID) == as.character(dashboardList$Users[iDashboard]),]
    accountUsers <- accountUsers0
    kUser <- 0
    for(iUser in 1:dim(dashboardUsers)[1]){
      kUser <- kUser + 1
      users[[kUser]] <- list(userId = as.character(dashboardUsers$UserId[iUser]),
                             permission = as.integer(dashboardUsers$Permission[iUser]))
      accountUsers <- accountUsers[accountUsers != as.character(dashboardUsers$UserId[iUser])]
    }
    for(accountUser in accountUsers){
      kUser <- kUser + 1
      users[[kUser]] <- list(userId = accountUser,
                             permission = 0)
    }
    
    
    dashboardTiles <- tileList[as.character(tileList$ID) == as.character(dashboardList$Tiles[iDashboard]),]
    
    dashboardSiteGroups <- strsplit(as.character(dashboardList[iDashboard,]$Sites), ';')[[1]]
    for(dashboardSiteGroup in dashboardSiteGroups){
      dashboardSites <- opisenseSites[opisenseSites$clientData$`Location Master Data`$`General Information`$LOC_TYP_NAME == dashboardSiteGroup,]
      for(iSite in 1:dim(dashboardSites)[1]){
        opisenseToken <- getoAuthToken(user, password)
        dashboardSite <- dashboardSites[iSite,]
        
        tiles <- list()
        kTile <- 0
        for(iTile in 1:dim(dashboardTiles)[1]){
          dashboardTile <- dashboardTiles[iTile,]
          
          timeFrameData <- strsplit(as.character(dashboardTile$timeFrame),';')[[1]]
          timeFrame <- list(type = as.integer(timeFrameData[1]),
                            value = as.integer(timeFrameData[2]),
                            period = if(as.character(timeFrameData[3]) == 'NA'){NA}else{as.character(timeFrameData[3])},
                            includeDataUpToNow = if(as.character(timeFrameData[4]) == 'true'){TRUE}else{FALSE})
          
          dataSources <- list()
          dashboardSources <- sourceList[as.character(sourceList$ID) == as.character(dashboardTile$dataSources),]
          kSource <- 0
          for(iSource in 1:dim(dashboardSources)[1]){
            dashboardSource <- dashboardSources[iSource,]
            dataName <- as.character(dashboardSource$name)
            energyTypeId <- as.integer(dashboardSource$energyTypeId)
            siteId <- if(as.integer(dashboardSource$siteId) == 0){dashboardSite$id}else{groupSiteId}
            sourceCsr <- strsplit(as.character(dashboardSource$source),';')
            filterVariables <- strsplit(as.character(dashboardSource$variable),';')[[1]]
            variableShift <- as.integer(strsplit(as.character(dashboardSource$shift),';')[[1]])
            unitId <- as.integer(dashboardSource$unitId)
            displayId <- as.integer(dashboardSource$displayId)
            axisId <- as.integer(dashboardSource$axisId)
            variableColor <- as.character(dashboardSource$color)
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
              opisenseSources <- opisenseSources[!is.na(opisenseSources$name),]
              if(is.null(opisenseSources) || (dim(opisenseSources)[1] == 0)) next
              for(jSource in 1:dim(opisenseSources)[1]){
                opisenseSource <- opisenseSources[jSource,]
                for(iVariable in 1:length(filterVariables)){
                  opisenseVariable <- opisenseAccountVariables[(opisenseAccountVariables$sourceId == opisenseSource$id) &
                                                                 (tolower(opisenseAccountVariables$name) == tolower(filterVariables[iVariable])),]
                  if(!is.null(opisenseVariable) && (dim(opisenseVariable)[1] != 0)){
                    kSource <- kSource + 1
                    legend <- list()
                    name <- if(dataName == 'SourceName'){opisenseSource$name}else{dataName}
                    legend[[1]] <- list(type='Custom',
                                        customValue=name,
                                        checked=TRUE)
                    dataSources[[kSource]] <- list(siteId=siteId,
                                                   shift = list(value = variableShift[1],
                                                                timePeriod = variableShift[2]),
                                                   displayId = displayId,
                                                   axisId = axisId,
                                                   color = if(variableColor == 'fromList'){as.character(colorList$color[kSource])}else{variableColor},
                                                   name = name,
                                                   legend = legend,
                                                   sourceId = opisenseSource$id,
                                                   variableId = opisenseVariable$id,
                                                   unitId = unitId)
                  }
                }
              }
            }
          }
          if(length(dataSources) == 0) next
          kTile <- kTile + 1
          tiles[[kTile]] <- list(title = as.character(dashboardTile$title),
                                 size = as.integer(dashboardTile$size),
                                 visualSourceType = as.integer(dashboardTile$visualSourceType),
                                 visualizationType = as.integer(dashboardTile$visualizationType),
                                 timeFrame = timeFrame,
                                 grouping = as.integer(dashboardTile$grouping),
                                 granularity = as.integer(dashboardTile$granularity),
                                 gradientColorNumber = as.integer(dashboardTile$gradientColorNumber),
                                 gradientBeginColor = as.character(dashboardTile$gradientBeginColor),
                                 gradientMediumColor = as.character(dashboardTile$gradientMediumColor),
                                 gradientEndColor = as.character(dashboardTile$gradientEndColor),
                                 dataSources = dataSources)
        }
        dashboard0 <- getOpisenseObject(opisenseToken,'dashboards',list(siteId = siteId))
        dashboardId <- dashboard0[dashboard0$name == as.character(dashboardList$Name[iDashboard]),]$id
        if(length(dashboardId)==0)dashboardId <- 0
        dashboard <- list(id = if(dashboardId==0){NA}else{dashboardId},
                          siteId = siteId,
                          name = as.character(dashboardList$Name[iDashboard]),
                          users = users,
                          tiles = tiles)
        dashboardId <- pushOpisenseObject(opisenseTokenPower,'dashboards',dashboard,dashboardId)
        kDashboard <- kDashboard + 1
        print(paste0(kDashboard,' :Created Dashboard ', dashboardId, ' on ', dashboardSite$name))
        if(dashboardId == 0){
          kFailed <- kFailed + 1
          failedDashboard[kFailed] <- dashboardSite$id
        }
        outputResult <- rbind(outputResult, data.frame(Dashboard = dashboard$name,
                                                       SiteId = dashboard$siteId,
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