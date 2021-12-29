getoAuthToken <- function(username,password) {
  oAuthFilter <- list(
    client_id = api_client_id,
    client_secret = api_client_secret,
    grant_type = "password",
    scope = api_scope,
    username = username,
    password = password
  )
  oAuthResult <- POST("https://identity-qa.opinum.com/connect/token",
                     add_headers("Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                     body = paste(paste(names(oAuthFilter),gsub("&","%26",oAuthFilter),sep = "="), collapse = "&"));
  return(paste("Bearer", content(oAuthResult)$access_token))
}
