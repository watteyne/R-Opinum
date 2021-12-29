pushSiteToGroup <- function(oAuthToken,groupId,siteId){
  POST(paste0("https://api.opinum.com:443/groups/",groupId,"/sites?sitesId=",siteId),
       add_headers("Accept" = "application/json", "Authorization" = oAuthToken, "X-Opisense-Api-Version" = "1.0", "Content-Type" = "application/json"))
}
