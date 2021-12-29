batchGetOpisenseObject <- function(opisenseToken, apiCall, filter, splitId){
  result <- data.frame()
  ids <- unlist(filter[[splitId]], use.names = FALSE)
  splitIds <- split(ids, ceiling(seq_along(ids)/100))
  for(k in 1:length(splitIds)){
    filter[[splitId]] <- unlist(splitIds[k], use.names = FALSE)
    result <- rbind(result, getOpisenseObject(opisenseToken, apiCall, filter))
    a<-1
  }
  result
}