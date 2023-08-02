load('c:/dev/R/Calculated/georgetown/sampleData.rdata')

library('dplyr')
library('lubridate')

get_date_timestamp_in_local_time <- function(utc_date) {
  # Could be a loooot simpler to write but some mysterious behaviours force to follow this way
  utc_timestamp <- as.integer(utc_date) * 86400
  utc_forced_date <- as.POSIXct(utc_timestamp,
                                origin='1970-01-01',
                                tz='UTC')
  local_forced_date <- force_tz(utc_forced_date, tz='America/Chicago')
  as.integer(local_forced_date)
}


get_last_violation_level <- function() {
  violation_levels <- inputVariables$self$TimeSeries
  if (nrow(violation_levels) ==0) {
    data.frame(Dates=numeric(0), Values=numeric(0))
  } else {
    violation_levels[nrow(violation_levels), ]
  }
}


filter_violations <- function() {
  violations <- inputVariables$violation$TimeSeries
  # We only look at violations after a fixed start date
  violations <- violations[violations$Dates >= start_violation_calculations, ]

  if (nrow(last_violation_level) > 0) {
    violations <- violations[violations$Dates > last_violation_level[1, 'Dates'], ]
  }

  # We clean the violations during a certificate period
  if(is.null(inputForms$certificateStart)) {
    if(!is.null(inputForms$certificateEnd)) {
      end <- get_date_timestamp_in_local_time(inputForms$certificateEnd)
      violations <- violations[violations$Dates > end, ]
    }
  } else {
    start <- get_date_timestamp_in_local_time(inputForms$certificateStart)
    if(is.null(inputForms$certificateEnd)) {
      violations <- violations[violations$Dates < start, ]
    } else {
      end <-get_date_timestamp_in_local_time(inputForms$certificateEnd)
      violations <- violations[(violations$Dates < start) | (violations$Dates > end), ]
    }

  }

  violations
}

get_owners <- function() {
  # We transform the owner variable to only keep ownership changes
  owners <- inputVariables$owner$TimeSeries
  # We skip the days where the owner finish a contract and starts a new one
  # The end dates are stored one minute before midnight
  owners[owners$Values < 0, 'Dates'] <- owners[owners$Values < 0, 'Dates'] + 60
  owners <- owners %>%
    group_by(Dates) %>%
    summarise(Values = sum(Values))
  # If we have the same signature (once positive, once negative)
  # on a day between two contracts, the value will be around 0
  # This means that we have a contract renewal
  owners[abs(owners$Values) >= 0.5, ]
}


add_courtesy_level <- function(violation_date) {
  last_violation_level <<- data.frame(Dates=violation_date, Values=0)
  result <<- rbind(result, last_violation_level)
  violations <<- violations[violations$Dates > violation_date, ]
}

manage_recent_existing_violation_level_for_owner <- function(violation_date) {
  freeze_period_name <- paste0('frozen', last_violation_level$Values)
  freeze_period <- if (is.null(inputForms[[freeze_period_name]])) {10} else {inputForms[[freeze_period_name]]}
  min_next_level_date <- last_violation_level$Dates + freeze_period * 86400
  if (violation_date < min_next_level_date) {
    # Violation is not eligible. We look at next iteration only for eligible dates
    next_level_date <- min_next_level_date
    # But we need to take a change of owner into account
    next_owners <- owners[owners$Dates > violation_date, ]
    if (nrow(next_owners) > 0) {
      # Bypass min issues in Datahub R
      next_owners <- next_owners[order(next_owners$Dates), ]
      if (next_level_date > next_owners[1, 'Dates']) {
        next_level_date <- next_owners[1, 'Dates']
      }
    }
    violations <<- violations[violations$Dates >= next_level_date, ]
  } else {
    # Naughty, naughty boy
    last_violation_level <<- data.frame(Dates=violation_date, Values=last_violation_level$Values + 1)
    result <<- rbind(result, last_violation_level)
    violations <<- violations[violations$Dates > violation_date, ]
  }
}

manage_existing_violation_level_for_owner <- function(violation_date) {
  if ((violation_date - last_violation_level$Dates) > reset_period) {
    # Last violation level is too old
    add_courtesy_level(violation_date)
  } else {
    manage_recent_existing_violation_level_for_owner(violation_date)
  }
}

manage_existing_violation_level <- function(violation_date) {
  # We get the contract start for one unique owner
  previous_owners <- owners[owners$Dates <= violation_date, 'Dates']
  if (nrow(previous_owners) == 0) {
    # This should never happen but we consider a "ghost" owner
    owner_start_date <- -Inf
  } else {
    # Bypass max issues in Datahub R
    previous_owners <- previous_owners[order(-previous_owners$Dates), ]
    # We sorted descending
    owner_start_date <- previous_owners[1, 'Dates']
  }
  if (last_violation_level$Dates < owner_start_date) {
    # This is the first violation for this owner
    add_courtesy_level(violation_date)
  } else {
    manage_existing_violation_level_for_owner(violation_date)
  }
}

main <- function() {
  while (nrow(violations) > 0) {
    first_violation_date <- violations[1, 'Dates']
    if (nrow(last_violation_level) == 0){
      # We consider this is the first violation
      add_courtesy_level(first_violation_date)
    } else {
      manage_existing_violation_level(first_violation_date)
    }
  }
}


start_violation_calculations <- get_date_timestamp_in_local_time(inputForms$startViolations)
reset_period <- inputForms$reset * 86400
last_violation_level <- get_last_violation_level()
violations <- filter_violations()
owners <- get_owners()
result <- data.frame(Dates=numeric(0), Values=numeric(0))
main()
list(TimeSeries=result)



