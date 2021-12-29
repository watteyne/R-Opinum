getOpisenseObject <- function(oAuthToken, apiCall, filter){
  filter <- paste(paste(rep(names(filter), sapply(filter, length)), unlist(filter), sep = "="), collapse = "&")
  filter <- URLencode(filter, reserved = FALSE, repeated = FALSE)
  result <- GET(paste(paste0("https://api-qa.opinum.com:443/", apiCall), filter, sep = "?"),
                 add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.1"))
  if (length(content(result)) == 0){
    return(NULL)
  }
  result <- fromJSON(content(result, as = "text"))
  return(result)
}
