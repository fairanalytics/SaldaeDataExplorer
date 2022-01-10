#' Saldae time step detector
#' @description detect based on time vector the time step of my data (hourly, daily, weekly, monthly,...)
#' @author Farid Azouaou
#' @param time_vect time vector
#' @param n_new_dates detect only time step or generate new data using that time step
#' @param include_start_date whether to includelast date value
#' @return time stepoutput (hours, seconds, days , ... )
#' @export
detect_date_auto <- function(time_vect = NULL, n_new_dates = 1, include_start_date = FALSE) {

  time_vect <- tail(unique(time_vect), 2000)
  time_jump <- round(abs(diff(as.POSIXct(time_vect))), 0)
  time_unit <- attributes(time_jump)$units
  # .
  # time_unit <- time_vect%>%timetk::tk_get_timeseries_signature()%>%
  #   dplyr::select(year,half,quarter,month,day,hour,minute,second)%>%
  #   janitor::remove_constant()%>%colnames()%>%tail(1)%>%paste0("s")
  # return(time_unit)

  time_jump <- head(plyr::count(time_jump)%>%dplyr::arrange(desc(freq)),2)$x

  if (365 %in% time_jump & time_unit == "days") {
    time_unit <- "years"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq.Date(from = as.Date(tail(time_vect, 1)), length.out = n_new_dates + 1, by = "years")
  }
  if ((TRUE %in% sapply(time_jump, function(x) x %in% c(31, 30))) & time_unit == "days") {
    # ..........
    time_unit <- "months"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq.Date(from = as.Date(tail(time_vect, 1)), length.out = n_new_dates + 1, by = "months")
    # ..........
  }
  if ((TRUE %in% sapply(time_jump, function(x) x %in% c(7))) & time_unit == "days") {
    # ..........
    time_unit <- "weeks"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq.Date(from = as.Date(tail(time_vect, 1)), length.out = n_new_dates + 1, by = "7 days")
    # ..........
  }
  if (TRUE %in% (time_jump == 1) & time_unit == "days" || TRUE %in% (time_jump == 24) & time_unit == "hours") {
    # ..........
    time_unit <- "days"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq.Date(from = as.Date(tail(time_vect, 1)), length.out = n_new_dates + 1, by = "days")
    # ..........
  }
  if (TRUE %in% (time_jump %in% c(90,91,92)) & time_unit == "days") {
    # ..........
    time_unit <- "quarter"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq.Date(from = as.Date(tail(time_vect, 1)), length.out = n_new_dates + 1, by = "quarter")
    # ..........
  }
  if (TRUE %in% (time_jump == 60) & time_unit == "mins" | TRUE %in% (time_jump == 1) & time_unit == "hours") {
    # ..........
    time_unit <- "hours"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq(from = as.POSIXct(tail(time_vect, 1), tz = "CET"), length.out = n_new_dates + 1, by = "hours")
    # ..........
  }
  if (TRUE %in% (time_jump== 30) & time_unit == "mins") {
    # ..........
    time_unit <- "1/2 hours"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq(from = as.POSIXct(tail(time_vect, 1), tz = "CET"), length.out = n_new_dates + 1, by = "30 minutes")
    # ..........
  }

  if (TRUE %in% (time_jump == 15) & time_unit == "mins") {
    # ..........
    time_unit <- "1/4 hours"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    new_dates <- seq(from = as.POSIXct(tail(time_vect, 1), tz = "CET"), length.out = n_new_dates + 1, by = "15 minutes")
    # ..........
  }
  if (TRUE %in% (time_jump == 60) & time_unit == "secs" | TRUE %in% (time_jump == 1) & time_unit == "mins") {
    # ..........
    time_unit <- "minutes"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    period <- minutes(n_new_dates + 1)
    start_date <- as.POSIXct(tail(time_vect, 1), tz = "CET")
    stop_date <- start_date + period
    new_dates <- seq(start_date, stop_date, length.out = (n_new_dates + 1))
  }
  if (TRUE %in% (time_jump == 1) & time_unit == "secs") {
    # ..........
    time_unit <- "seconds"
    if (is.null(n_new_dates)) {
      return(time_unit)
    }
    period <- seconds(n_new_dates + 1)
    start_date <- as.POSIXct(tail(time_vect, 1), tz = "CET")
    stop_date <- start_date + period
    new_dates <- seq(start_date, stop_date, length.out = (n_new_dates + 1))
  }
  if (include_start_date == FALSE) {
    new_dates <- new_dates[-1]
  }

  # ........................
  return(as.POSIXct(new_dates, tz = "CET"))
  # ........................
}



#' Saldae possible time units
#' @description based on time we derive all possible time units on which the data can be aggregated
#' @author Farid Azouaou
#' @param time_vect time vector
#' @return a vector with all possible time unit valid for aggregation (hours ==> days, weeks, months)
#' @export
possible_units_for_summary <- function(time_vect = NULL) {
  # ..............
  time_unit <- detect_date_auto(time_vect = time_vect, n_new_dates = NULL)
  # ..............
  if (time_unit == "seconds") time_units_sum <- c("seconds", "minutes", "hours")
  if (time_unit == "minutes") time_units_sum <- c("minutes", "hours", "days")
  if (time_unit == "1/2 hours") time_units_sum <- c("1/2 hours", "hours", "days")
  if (time_unit == "1/4 hours") time_units_sum <- c("1/4 hours", "hours", "days")
  if (time_unit == "hours") time_units_sum <- c("hours", "days", "weeks")
  if (time_unit == "days") time_units_sum <- c("days", "weeks", "months")
  if (time_unit == "weeks") time_units_sum <- c("weeks", "months", "quarters")
  if (time_unit == "months") time_units_sum <- c("months", "quarters", "years")
  if (time_unit == "quarter") time_units_sum <- c("quarters", "years")
  if (time_unit == "years") time_units_sum <- "years"
  # ..............
  return(time_units_sum)
}
#' Saldae basis units
#' @description calculate how many basis time units in one given unit
#' @author Farid Azouaou
#' @param upper_unit time unit (weeks ,seconds , days , months ,quarters)
#' @param basis_unit  basis time unit
#' @param last_date ur zri-gh ara
#' @return a scalar number ( 24 hours in one day)
#' @export

detect_number_basis_units_in_upper_unit <- function(upper_unit = NULL, basis_unit = NULL, last_date = NULL) {
  #-------------------
  if (upper_unit == basis_unit) {
    return(1)
  }
  if (upper_unit == "years" & basis_unit == "quarters") {
    return(4)
  }
  if (upper_unit == "years" & basis_unit == "months") {
    return(12)
  }
  if (upper_unit == "years" & basis_unit == "weeks") {
    return(52)
  }
  if (upper_unit == "years" & basis_unit == "days") {
    return(as.numeric(as.Date(zoo::as.yearmon(lubridate::year(last_date)) + 1) - as.Date(zoo::as.yearmon(lubridate::year(last_date)))))
  }
  if (upper_unit == "quarters" & basis_unit == "months") {
    return(3)
  }
  if (upper_unit == "quarters" & basis_unit == "weeks") {
    last_date <- as.Date(paste(lubridate::year(last_date), lubridate::month(last_date), lubridate::days_in_month(last_date), sep = "-"))
    wk <- seq.Date(from = as.Date(last_date), length.out = 93, by = "-7 days")
    stop("ur tefriw ara")
    return(length(which(lubridate::quarter(wk) == lubridate::quarter(last_date))))
  }
  if (upper_unit == "quarters" & basis_unit == "days") {
    agguren_deg_useggas <- seq.Date(from = as.Date(paste0(lubridate::year(last_date), "-01-01")), length.out = 12, by = "months")
    ussan_deg_aggur <- days_in_month(agguren_deg_useggas)
    return(sum(ussan_deg_aggur[which(lubridate::quarter(agguren_deg_useggas) == lubridate::quarter(last_date))]))
  }
  if (upper_unit == "months" & basis_unit == "days") {
    return(lubridate::days_in_month(last_date))
  }
  if (upper_unit == "months" & basis_unit == "weeks") {
    return(4)
  }
  if (upper_unit == "weeks" & basis_unit == "days") {
    return(7)
  }
  if (upper_unit == "weeks" & basis_unit == "hours") {
    return(7 * 24)
  }
  if (upper_unit == "days" & basis_unit == "hours") {
    return(24)
  }
  if (upper_unit == "hours" & basis_unit == "1/2 hours") {
    return(2)
  }
  if (upper_unit == "days" & basis_unit == "1/2 hours") {
    return(2*24)
  }
  if (upper_unit == "hours" & basis_unit == "1/4 hours") {
    return(4)
  }
  if (upper_unit == "days" & basis_unit == "1/4 hours") {
    return(4*24)
  }
  if (upper_unit == "weeks" & basis_unit == "hours") {
    return(7 * 24)
  }
  if (upper_unit == "days" & basis_unit == "minutes") {
    return(3600)
  }
  if (upper_unit == "hours" & basis_unit == "minutes") {
    return(60)
  }
  if (upper_unit == "hours" & basis_unit == "seconds") {
    return(4)
  }
  if (upper_unit == "minutes" & basis_unit == "seconds") {
    return(60)
  }
  if (upper_unit == "hours" & basis_unit == "seconds") {
    return(3600)
  }
}


