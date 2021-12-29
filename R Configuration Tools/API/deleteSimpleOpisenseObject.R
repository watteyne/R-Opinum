deleteSimpleOpisenseObject <- function(oAuthToken, apiCall, opisenseId){
  result <- DELETE(paste0("https://api.opinum.com:443/", apiCall, '/', opisenseId),
                   add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0"))
  return(result$status_code)
}