##########################
# .
# . use tsouliers package : but make sure to don't change the trajectory of ts
# .
# . phase 01: include only Additive outliers otherwise the trajectory of original time series maybe disrupted

# . discard non numeric vector(column)
non_numeric_vector <- function(x) {
  # .
  l <- length(which(!is.na(as.numeric(na.omit(x))^2))) != length(na.omit(x))
  return(l)
  # .
}
#-------------- outliers detection without errors --------

outlier_detect_final <- function(x, outlier_type = "AO") {
  # tso(y =as.ts(x),type=outlier_type)$yadj
  return(tryCatch(tso(y = as.ts(x), type = outlier_type)$yadj, error = function(e) x, finally = print("Outlier detection  was not successful")))
}

#' Saldae Anomaly Detection
#' @description anomaly detection module for one single time serie
#' @author Farid Azouaou
#' @param tisefka  tibble with two variable date and target variable
#' @return tibble with anomaly information
#' @export

anomaly_detection_yiwen <- function(tisefka = NULL,anomaly_mode = "auto"){
  if (anomaly_mode == "auto") {
    if (nrow(tisefka) <= 2000) anomaly_mode <- "anomalize"
    if (nrow(tisefka) > 2000) anomaly_mode <- "twitter"
  }

  target_variable <- colnames(tisefka)[colnames(tisefka)!="date"]

  if (anomaly_mode == "anomalize"){
    tisefka <- tisefka%>%anomalize::time_decompose(!!target_variable, method = "stl") %>%
      anomalize::anomalize(remainder)%>%
      anomalize::clean_anomalies()
    Anomalies <- purrr::imap(.x = 1:nrow(tisefka), ~ base::ifelse(tisefka$anomaly[.x] == "Yes", tisefka$observed[.x], NA))

    tisefka<- tisefka%>%dplyr::mutate(Anomalies = as.numeric(Anomalies),upper_bound = trend+season+remainder_l2,lower_bound = trend+season+remainder_l1)%>%
      dplyr::select(date,observed,anomaly,Anomalies,observed_cleaned,upper_bound,lower_bound)

    # # if ("Yes" %in% tisefka_tazedgant$anomaly) {
    #   tisefka <- tisefka %>% dplyr::mutate(Anomalies = as.numeric(Anomalies),Corrected = tisefka_tazedgant$observed_cleaned)
    # # }
    # Function to clean & repair anomalous data
    # anomalize::clean_anomalies()
  }
  if (anomaly_mode == "twitter") {
    anomaly_ts <- AnomalyDetection::AnomalyDetectionTs(x = tisefka[, c("date", target_ts)], max_anoms = 0.02, direction = "both", plot = FALSE)
    if (dim(anomaly_ts$anoms)[1] != 0) {
      anomaly_ts <- anomaly_ts$anoms
      anomaly_ts <- data.frame(date = as.POSIXct(anomaly_ts$timestamp, tz = "CET"), Anomalies = anomaly_ts$anoms)
      tisefka <- tisefka %>% dplyr::left_join(anomaly_ts, by = "date")
    }
  }
  return(tisefka)
}

#' Saldae Anomaly Detection wrapper
#' @description anomaly detection module for for multiple data
#' @author Farid Azouaou
#' @param tisefka  tible including target variable and date information
#' @return tibble with anomaly information
#' @export

anomaly_detection_nnegh <- function(tisefka = NULL, anomaly_mode = "auto", target_ts = NULL) {
  tisefka[,target_ts] <-purrr::map_df(tisefka[,target_ts],~interp_na_value(ts_x = .x,interp_mode = "spline"))
  tisefka <- tisefka[,c("date",target_ts)]
  tisefka <- target_ts%>%purrr::map( ~anomaly_detection_yiwen(tisefka = tisefka[,c("date",.x)],anomaly_mode = anomaly_mode))%>%
    stats::setNames(target_ts)
  return(tisefka)
}
outliers_detection_function <- function(ts_original = NULL) {
  # .
  ts_original <- data.frame(ts_original, check.names = FALSE)
  # . apply only on numerical time series
  non_numeric_ts <- apply(ts_original, 2, function(x) non_numeric_vector(x))
  # .
  ts_cleaned <- ts_original[, !non_numeric_ts, drop = F]

  # . take out non informartive or and contant vector
  poor <- apply(ts_cleaned, 2, function(x) poor.data(ts_x = x, content_percentage = 10, outl_detection = TRUE))
  # .
  ts_cleaned <- data.frame(ts_cleaned[, !poor], check.names = FALSE)
  # .
  ts_cleaned[, 1] <- apply(ts_cleaned[, 1, drop = F], MARGIN = 2, function(x) outlier_detect_final(x, outlier_type = "AO"))

  ts_original[, colnames(ts_cleaned)] <- ts_cleaned
  # .
  print("outliers detection is successfully done.....")
  # .
  return(ts_original)
}
