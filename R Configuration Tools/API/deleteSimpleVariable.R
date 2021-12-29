deleteSimpleVariable <- function(oAuthToken, sourceId, variableId){
    
    
    query = paste0("https://api.opinum.com:443/", 'sources/', sourceId, '/variables/', variableId)
    
    
  result <- DELETE(query,
                 add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0"))
  return(result$status_code)
}