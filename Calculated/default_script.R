{
'#===== Toolbox functions ====='
  GetInputAxis <- function(inputVariables){
    t <- unique(unlist(lapply(inputVariables, function(x){x$TimeSeries$Dates}), use.names = FALSE))
    t <- t[order(t)]
    dt <- diff(t)
    t <- t[seq(2,length(t),1)]
    data.frame(t = t, dt = dt)}
  GetRegularAxis <- function(targetVariable, inputVariables){
    tMin <- max(unlist(lapply(inputVariables, function(x){min(x$TimeSeries$Dates)}), use.names = FALSE))
    tMax <- min(unlist(lapply(inputVariables, function(x){max(x$TimeSeries$Dates)}), use.names = FALSE))
    switch(targetVariable$GranularityTimeBase,
           'Second' = {
             granularity = 'sec'},
           'Minute' = {
             granularity = 'min'
             dt <- targetVariable$Granularity*60
             tMin <- dt*ceiling(tMin/dt)},
           'Hour' = {
             granularity = 'hour'
             dt <- targetVariable$Granularity*60*60
             tMin <- dt*ceiling(tMin/dt)},
           'Day' = {
             granularity = 'day'
             dt <- targetVariable$Granularity*60*60*24
             tMin <- dt*ceiling(tMin/dt)},
           'Week' = {
             granularity = 'week'
             dt <- targetVariable$Granularity*60*60*24
             tMin <- dt*ceiling(tMin/dt)
             tWeekDay <- as.numeric(strftime(as.POSIXct(tMin, tz = 'UTC', origin = '1970-01-01'), format = '%w'))
             dt <- ((7-tWeekDay+1) %% 7) * 60*60*24
             tMin <- tMin + dt
           },
           'Month' = {
             granularity = 'month'
             dt <- targetVariable$Granularity*60*60*24
             tMin <- dt*ceiling(tMin/dt)
             tTemp <- as.POSIXct(tMin, tz = 'UTC', origin = '1970-01-01')
             tDay <- as.numeric(strftime(tTemp, format = '%d'))
             if (tDay != 1){
               tYear <- as.numeric(strftime(tTemp, format = '%Y'))
               tMonth <- as.numeric(strftime(tTemp, format = '%m'))+1
               tMin <- as.numeric(as.POSIXct(paste(tYear, tMonth,'01', sep = '-'), tz = 'UTC', origin = '1970-01-01'))
             }},
           'Year' = {
             granularity = 'year'
             dt <- targetVariable$Granularity*60*60*24
             tMin <- dt*ceiling(tMin/dt)
             tTemp <- as.POSIXct(tMin, tz = 'UTC', origin = '1970-01-01')
             tDay <- as.numeric(strftime(tTemp, format = '%d'))
             tMonth <- as.numeric(strftime(tTemp, format = '%m'))
             if (tDay != 1 || tMonth != 1){
               tYear <- as.numeric(strftime(tTemp, format = '%Y'))
               tMin <- as.numeric(as.POSIXct(paste(tYear+1,'01','01',sep = '-'), tz = 'UTC', origin = '1970-01-01'))
             }}
    )
    tMinDate <- as.POSIXct(tMin, tz = 'UTC', origin = '1970-01-01')
    tMaxDate <- as.POSIXct(tMax, tz = 'UTC', origin = '1970-01-01')
    t <- as.numeric(seq(tMinDate, tMaxDate, by = paste(targetVariable$Granularity, granularity)))
    dt <- diff(as.numeric(c(seq(tMinDate, length = 2, by = paste(paste0('-',targetVariable$Granularity), granularity))[2], t)))
    data.frame(t = t, dt = dt)}
  SetDataOnCommonAxis <- function(t, inputVariables, useVariableQuantityType){
    as.data.frame(sapply(inputVariables, function(x){
      index <- order(x$TimeSeries$Dates)
      x$TimeSeries <- x$TimeSeries[index,]
      quantityType <- if(!useVariableQuantityType){'Integrated'}else{x$Variable$QuantityType}
      switch(quantityType,
             'Instantaneous' = {
               tx <- x$TimeSeries$Dates
               vx <- x$TimeSeries$Values
               approx(tx, vx, t$t, method = 'linear')$y
             },
             'Integrated' = {
               tx <- x$TimeSeries$Dates[seq(2,length(x$TimeSeries$Dates),1)]
               dvx <- x$TimeSeries$Values[seq(2,length(x$TimeSeries$Values),1)] / diff(x$TimeSeries$Dates)
               approx(tx, dvx, t$t, method = 'constant', f=1)$y * t$dt
             },
             'Cumulative' = {
               tx <- x$TimeSeries$Dates
               vx <- x$TimeSeries$Values
               approx(tx, vx, t$t, method = 'linear')$y
             },
             'Minimum' = {
               tx <- x$TimeSeries$Dates
               vx <- x$TimeSeries$Values
               approx(tx, vx, t$t, method = 'linear')$y
             },
             'Maximum' = {
               tx <- x$TimeSeries$Dates
               vx <- x$TimeSeries$Values
               approx(tx, vx, t$t, method = 'linear')$y
             },
             'Averaged' = {
               tx <- x$TimeSeries$Dates
               vx <- x$TimeSeries$Values
               approx(tx, vx, t$t, method = 'linear')$y
             }
      )}))}
  FilterNaOutput <- function(result){
    index <- !is.na(result$Values)
    data.frame(Dates = result$Dates[index], Values = result$Values[index])}
  FilterDuplicateOutput <- function(result, filterOutput, doubleComparisonThreshold){
    indexNew <- is.element(result$Dates, filterOutput$Dates)
    indexOld <- is.element(filterOutput$Dates, result$Dates)
    filterOutput <- filterOutput[indexOld,]
    newOutput <- result[indexNew,]
    indexFilter <- abs(filterOutput$Values - newOutput$Values) < doubleComparisonThreshold
    datesFilter <- is.element(result$Dates, result$Dates[indexNew][indexFilter])
    data.frame(Dates = result$Dates[!datesFilter], Values = result$Values[!datesFilter])}
'#===== Main Function ====='
  tryCatch(
    {
 '#===== Optional parameters ====='
      useRegularAxis <- if(!is.null(inputConstants$UseRegularAxis)){inputConstants$UseRegularAxis == 1}else{FALSE}
      useVariableQuantityType <- if(!is.null(inputConstants$UseVariableQuantityType)){inputConstants$UseVariableQuantityType == 1}else{FALSE}
      doubleComparisonThreshold <- if(!is.null(inputConstants$DoubleComparisonThreshold)){inputConstants$DoubleComparisonThreshold}else{0.0001}
      if(!is.null(inputVariables$FilterOutput)){
        index <- order(inputVariables$FilterOutput$TimeSeries$Dates)
        filterOutput <- inputVariables$FilterOutput$TimeSeries[index,]
        inputVariables <- inputVariables[!(names(inputVariables) %in% c('FilterOutput'))]
      }
      else{filterOutput <- NULL}
 '#===== Time axis ====='
      t <- if(useRegularAxis){GetRegularAxis(targetVariable, inputVariables)}else{GetInputAxis(inputVariables)}
      regularVariables <- SetDataOnCommonAxis(t, inputVariables, useVariableQuantityType)
 '#===== Formula ====='
      result <- data.frame(Dates = t$t, Values = 1 * 1 * regularVariables$s459562v2298198 + -1 * 1 * regularVariables$s459562v2298199 + 1 * 1 * regularVariables$s459564v2298204)
 '#===== Output ====='
      result <- FilterNaOutput(result)
      if(!is.null(filterOutput)){result <- FilterDuplicateOutput(result, filterOutput, doubleComparisonThreshold)}
      if(dim(result)[1] != 0){list(TimeSeries = result)}else{list(Errors = c('Calculation returned no output'))}
    },
    error = function(x){list(Errors = c(paste(x$message,paste(x$call, collapse = ' '), sep = ':')))},
    warning = function(x){list(Errors = c(paste(x$message,paste(x$call, collapse = ' '), sep = ':')))}
  )
}