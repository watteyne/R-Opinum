graphics.off(); rm(list=setdiff(ls(), c('password','user')));
library(jsonlite); library(httr); library(gdata); library(tcltk);
wdPath <- paste0(getSrcDirectory(function(x) {x}),'/')
#==== Load API and Batch functions ====
path0 <- c(paste0(wdPath,'API/'),
           paste0(wdPath,'BatchTools/'))
for (k in 1:length(path0)){
  files <- list.files(path = path0[k], pattern = '^.*[Rr]$', include.dirs = FALSE, full.names = TRUE)
  for (f in files) source(f)
}
#==== Get user input ====
user <- "pwa@opinum.com"
password <- "$tupiD001"
if (!exists('password') || !exists('user')){
  credentials <- getLoginDetails()
  user <- credentials$user
  password <- credentials$password
}
opisenseToken <- getoAuthToken(user, password)
if(opisenseToken == "Bearer "){
  print('Cannot log in Opisense - please check your credentials')
  rm(list=ls())
}else{
  nRun <- 0 #Set 0 for full run or set number of desired first item to manage
  t0 <- Sys.time()
  #result1 <- SetVirtualMeterSpecific(opisenseToken, nRun)
  t1 <- Sys.time()
  #result2 <- BatchAggregations(opisenseToken, wdPath, 'EolyAggregations.xlsx', nRun)
  t2 <- Sys.time()
  #result3 <- BatchBuildAlerts(opisenseToken, wdPath, 'EolyAlerts.xlsx', nRun)
  t3 <- Sys.time()
  result4 <- BatchBuildSiteDashboards(opisenseToken, wdPath, 'EolyDashboardsTEST.xlsx', nRun)
  #Use EolyDashboardsTEST.xlsx when running in test environment - difference: admin users in template
  t4 <- Sys.time()
  #result5 <- BatchBuildSiteReports(opisenseToken, wdPath, 'EolyReports.xlsx', nRun)
  t5 <- Sys.time()
}