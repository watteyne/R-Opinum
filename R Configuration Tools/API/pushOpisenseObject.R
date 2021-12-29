pushOpisenseObject <- function(oAuthToken,opisenseObjectType,opisenseObject,opisenseId){
  jsonBody <- toJSON(opisenseObject,
                 auto_unbox = TRUE,
                 byrow = TRUE,
                 colNames = TRUE,
                 asIs = TRUE)
  if (substr(jsonBody,1,1)=='['){
    jsonBody <- substr(jsonBody,2,nchar(jsonBody)-1)
  }
  
  if (length(opisenseId) != 0 && opisenseId != 0 && !is.na(opisenseId)){
    print('PUT')
    result <- PUT(paste0("https://api-qa.opinum.com:443/",opisenseObjectType,"/",opisenseId),
                  add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.1", "Content-Type" = "application/json"),
                  body = jsonBody)
    if (round(result$status_code/100,0) == 2){
      result = opisenseId
    }
    else{
      result = 0
    }
  }
  else{
    print('POST')
    result <- POST(paste0("https://api-qa.opinum.com:443/",opisenseObjectType,"/"),
                  add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.1", "Content-Type" = "application/json"),
                  body = jsonBody)
    result <- tryCatch({ifelse(is.atomic(content(result)),content(result),content(result)$id)},error=function(x){0})
  }
  
  return(result)
}
