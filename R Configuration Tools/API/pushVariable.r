pushVariable <- function(oAuthToken,variable,sourceId,variableId){
  jsonBody <- toJSON(variable,
                 auto_unbox = TRUE,
                 byrow = TRUE,
                 colNames = TRUE,
                 asIs = TRUE)
  if (substr(jsonBody,1,1)=='['){
    jsonBody <- substr(jsonBody,2,nchar(jsonBody)-1)
  }
  if (length(variableId) != 0 && variableId != 0 && !is.na(variableId)){
    result <- PUT(paste0("https://api.opinum.com:443/sources/",sourceId,"/variables/",variableId),
                  add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0", "Content-Type" = "application/json"),
                  body = jsonBody)
    if (result$status_code == 204){
      result = variableId
    }
    else{
      result = 0
    }
  }
  else{
    result <- POST(paste0("https://api.opinum.com:443/variables/source/",sourceId),
                  add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0", "Content-Type" = "application/json"),
                  body = jsonBody)
    result = ifelse(is.atomic(content(result)),content(result),content(result)$id)
  }

  return(result)
}
