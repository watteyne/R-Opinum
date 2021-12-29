getOpisenseSchedules <- function(oAuthToken, siteId){
  result <- GET(paste0("https://api.opinum.com:443/site/", siteId,'/schedules'),
                add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0"))
  if (length(content(result)) == 0){
    return(NULL)
  }
  result <- fromJSON(content(result, as = "text"))
  return(result)
}
