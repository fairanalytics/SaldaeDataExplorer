#############################################
#
#  FUNCTION: interpolate missing value
#
##############################################


# ............ module 1 : using "TSimpute"

#' Saldae missing data interpolation
#' @description use an interpolation logic based on imputeTS package to complete values
#' @author Farid Azouaou
#' @param ts_x time serie
#' @param interp_mode  interpolation mode linear , spline , ..
#' @param logic interpolation logic
#' @return a vector containing a complete time series without NAs
#' @export
interp_na_value <- function(ts_x = NULL, interp_mode = "spline", logic = "na_seadec") {

  if (all(!is.na(ts_x)) == TRUE)return(ts_x)
  # start_point <- length(ts_x) - length(zoo::na.trim(ts_x, sides = "left")) + 1
  # end_point <- length(zoo::na.trim(ts_x, sides = "right"))

  # . possible modes : "interpolation","locf","mq","mean","kalman","random"
  if (logic == "seasplit") {
    if (interp_mode %in% c("linear", "spline", "stine")) {
      ts_x <- na.seasplit(x = ts_x, algorithm = "interpolation", option = interp_mode)
    } else {
      ts_x <- na.seasplit(x = ts_x, algorithm = mode)
    }
  } else if (logic == "na_seadec") {
    # . Seasonally Decomposed Missing Value Imputation
    if (interp_mode %in% c("linear", "spline", "stine")) {
      ts_x <- imputeTS::na_seadec(x = ts_x, algorithm = "interpolation", option = interp_mode)
    } else {
      ts_x <- imputeTS::na_seadec(x = ts_x, algorithm = interp_mode,find_frequency=TRUE)
    }
  }
  # ts_orig[start_point:end_point] <- ts_x
  return(ts_x)
}


# ...........................................;
interpolate_missing_values <- function(ts_original = NULL, interpolation_mode = "linear") {
  ts_original <- data.frame(ts_original, check.names = FALSE)
  # . apply only on numerical time series
  non_numeric_ts <- apply(ts_original, 2, function(x) non_numeric_vector(x))


  # .
  ts_cleaned <- ts_original[, !non_numeric_ts, drop = F]

  # . take out non informartive or and contant vector

  # .
  contains_NAs <- apply(ts_cleaned, 2, function(x) NA %in% zoo::na.trim(x, sides = "both"))
  if (TRUE %in% contains_NAs) {
    ts_interpolated <- ts_cleaned[, contains_NAs, drop = F]
    ts_interpolated <- apply(ts_interpolated, MARGIN = 2, function(x) interp_na_value(ts_x = x, mode = interpolation_mode))
    ts_original[, colnames(ts_interpolated)] <- ts_interpolated
  }
  # .
  print("Interpolation is successfully done.....")
  # .
  return(ts_original)
  # .
}
