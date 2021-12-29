getGroups <- function(config,oAuthToken){
  groups <- GET("https://api.opinum.com:443/groups",
                 add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0"))
  if (length(content(groups)) == 0){
    return(NULL)
  }
  groups <- fromJSON(content(groups, as = "text"))
  return(groups)
}
