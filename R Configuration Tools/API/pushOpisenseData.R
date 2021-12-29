pushOpisenseData <- function(oAuthToken,opisenseObject){
  jsonBody <- toJSON(opisenseObject,
                 auto_unbox = TRUE,
                 byrow = TRUE,
                 colNames = TRUE,
                 asIs = TRUE)
  if (substr(jsonBody,1,1)=='['){
    jsonBody <- substr(jsonBody,2,nchar(jsonBody)-1)
  }
  result <- POST("https://push.opinum.com/api/standard/",
                 add_headers("Authorization" = oAuthToken, "Content-Type" = "application/json"),
                 body = jsonBody)
  
  return(result$Status)
}
