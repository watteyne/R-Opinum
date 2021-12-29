getOpisenseScheduleLevels <- function(oAuthToken, scheduleId){
  result <- GET(paste0('https://api.opinum.com:443/schedule/',scheduleId,'/levels'),
                add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.1"))
  if (length(content(result)) == 0){
    return(NULL)
  }
  result <- fromJSON(content(result, as = "text"))
  return(result)
}
