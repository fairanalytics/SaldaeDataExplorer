
#' basic growth rate
#' @author Farid Azouaou
#' @param x numeric vector
#' @param asurif differentiation lag
#' @return numeric vector containing differentiated values
basic_growth_rate <- function(x, asurif = 1) {
  linear_Develop <- x - dplyr::lag(x, asurif)
  linear_growth_rate <- round((x - dplyr::lag(x, asurif)) / dplyr::lag(x, asurif) * 100, 2)
  log_growth_rate <- round((log(x) - log(dplyr::lag(x, asurif))) * 100, 2)
  gr <- dplyr::tbl_df(data.frame(linear_Develop, linear_growth_rate, log_growth_rate))
  return(gr)
}
#'Saldae Detect possible growth rates
#' @description based on time unit detect possible growth rates
#' @author Farid Azouaou
#' @param base_unit time unit
#' @param gemmu_iswi target growth rate
#' @return possible growth rates
#' @export

gemmu_detect_frequency <- function(base_unit = NULL, gemmu_iswi = NULL) {
  if (base_unit == "minutes" & gemmu_iswi == "Hourly") {
    return("minute")
  }
  if (base_unit == "1/4 hours" & gemmu_iswi == "Daily") {
    return("15 minutes")
  }
  if (base_unit == "1/2 hours" & gemmu_iswi == "Daily") {
    return("30 minutes")
  }
  if (base_unit == "hours" & gemmu_iswi == "Daily") {
    return("hour")
  }
  if (base_unit == "days" & gemmu_iswi == "Weekly") {
    return("wday")
  }
  if (base_unit == "days" & gemmu_iswi == "Monthly") {
    return("mday")
  }
  if (base_unit == "days" & gemmu_iswi == "Yearly") {
    return("yday")
  }
  if (base_unit == "months" & gemmu_iswi == "Quarterly") {
    return("mquarter")
  }
  if (base_unit == "months" & gemmu_iswi == "Yearly") {
    return("month")
  }
  if (base_unit == "quarters" & gemmu_iswi == "Yearly") {
    return("quarter")
  }
}

#' Detect all possible growth rates based on Time Unit
#' @author Farid Azouaou
#' @param base_unit time unit
#' @return a vector containing possible growth rates.
#' @export
gemmu_yellan_f <- function(base_unit = NULL) {
  if (base_unit == "minutes") {
    return(c("Minutes", "Hourly"))
  }
  if (base_unit == "minutes") {
    return(c("Minutes", "Hourly"))
  }
  if (base_unit == "hours") {
    return(c("Hourly", "Daily"))
  }
  if (base_unit == "1/2 hours") {
    return(c("Hourly", "Daily"))
  }
  if (base_unit == "1/4 hours") {
    return(c("Hourly", "Daily"))
  }
  if (base_unit == "days") {
    return(c("Daily", "Weekly", "Monthly"))
  }
  if (base_unit == "weeks") {
    return(c("Weekly", "Monthly", "Quarterly"))
  }
  if (base_unit == "months") {
    return(c("Monthly", "Quarterly", "Yearly"))
  }
  if (base_unit == "quarters") {
    return(c("Quarterly", "Yearly"))
  }
  if (base_unit == "years") {
    return(c("Yearly"))
  }
}
tezmer_i_gemmu <- function(x) {
  return(!any(na.omit(x) <= 0))
}

#' Saldae Growth Rate one variable
#' @description calculate 3 different growth rates for one single variable
#' @author Farid Azouaou
#' @param tisefka_report Data exploration report
#' @param gemmu_iswi target growth rate
#' @param d_tirni exhaustive or grouped by
#' @return  data frame containing growth rate
Saldae_growth_rate_yiwen <- function(gemmu = NULL,tisefka = NULL , asurif  = NULL,target_variable = NULL){
  gemmu <- apply(gemmu, 2, function(x) basic_growth_rate(x, asurif = asurif))
  gemmu_yellan <- colnames(gemmu[[1]])
  gemmu <- dplyr::tbl_df(do.call(cbind, gemmu))
  gemmu <- purrr::map(.x =  gemmu_yellan,~rowSums(gemmu[, grepl(.x, colnames(gemmu))], na.rm = TRUE))%>%stats::setNames(gemmu_yellan)
  gemmu <- dplyr::tbl_df(data.frame(gemmu,check.names = FALSE))
  gemmu <- dplyr::bind_cols(tisefka, gemmu)
  gemmu <- gemmu %>% dplyr::filter(!is.na(target_variable))
  gemmu <- gemmu[, c("date", gemmu_yellan)]
  return(gemmu)
}

#' Saldae Growth Rate
#' @author Farid Azouaou
#' @param tisefka_report Data exploration report
#' @param gemmu_iswi target growth rate
#' @param d_tirni exhaustive or grouped by
#' @return  data frame containing growth rate
#' @export

Saldae_rate_n_gemmu_f <- function(tisefka = NULL,base_unit = NULL,target_ts= NULL ,gemmu_iswi = NULL, d_tirni = NULL) {
  # tisefka <- tisefka_report$tisefka


  # base_unit <- tisefka_report$time_unit
  gemmu <- unlist(purrr::map(tisefka[, target_ts],  ~tezmer_i_gemmu(.x)))
  target_ts <- target_ts[gemmu]
  if(length(target_ts)==0)return(NULL)
  akka_ukuden <- c("Seconds", "Minutes", "Hourly","1/2 Hourly","1/4 Hourly", "Daily", "Weekly", "Monthly", "Quarterly", "Yearly")
  names(akka_ukuden) <- c("seconds", "minutes", "hours","+30 minute","+15 minute" ,"days", "weeks", "months", "quarters", "years")

  ukud_asurif <- c("sec", "min", "hour","30 min","15 min" ,"day", "week", "month", "quarter", "year")
  ukud_isem   <- c("seconds", "minutes", "hours","1/2 hour","1/4 hour" ,"days", "weeks", "months", "quarters", "years")
  names(ukud_asurif) <- ukud_isem

  full_dates <- dplyr::tbl_df(data.frame(date = seq(from = min(tisefka$date), to = max(tisefka$date), by = ukud_asurif[base_unit])))

  tisefka <- full_dates %>% dplyr::left_join(tisefka, by = "date")


  if (akka_ukuden[base_unit] != gemmu_iswi) {
      gemmu_ig_zemren <- c("minute", "hour", "day", "wday", "mday", "month", "quarter", "mquarter", "year")
      gemmu <- dplyr::tbl_df(timetk::tk_get_timeseries_signature(tisefka$date))
      gemmu <- gemmu %>% dplyr::mutate(mquarter = (month %% 4) + 1)
      gemmu <- gemmu[, gemmu_ig_zemren]
      gemmu <- gemmu %>% dplyr::select_if(~ length(unique(.)) > 1)
      tisefka <- purrr::map(.x = target_ts,~dplyr::bind_cols(tisefka[,c("date",.x)], gemmu))%>%
                        stats::setNames(target_ts)
      rm(gemmu)
      #------------------------
      zuzer_s <- gemmu_detect_frequency(base_unit = base_unit, gemmu_iswi = gemmu_iswi)

      asurif <- length(unique(tisefka[[1]] %>% dplyr::pull(!!zuzer_s)))
      gemmu   <- purrr::map(.x = target_ts, ~tisefka[[.x]]%>%tidyr::spread(!!zuzer_s, !!.x))

      gemmu <- purrr::map(.x =gemmu ,~ .x%>% dplyr::select(c((ncol(.x) - asurif + 1):ncol(.x))))%>%
        stats::setNames(target_ts)
      gemmu <- purrr::map(.x = target_ts, ~Saldae_growth_rate_yiwen(gemmu = gemmu[[.x]],tisefka = tisefka[[.x]],target_variable = .x,asurif = asurif))%>%
              stats::setNames(target_ts)


  } else {
        # tisefka_n_gemmu <- tisefka%>%
    #   complete(date = seq(min(date), max(date), by=base_unit))
    gemmu <- purrr::map(.x = tisefka[,target_ts], ~basic_growth_rate(.x ))%>%stats::setNames(target_ts)
    gemmu_yellan <- colnames(gemmu[[1]])
    gemmu <- purrr::map(.x = target_ts, ~dplyr::bind_cols(tisefka[,c("date",.x)], gemmu[[.x]])%>%dplyr::filter(!is.na(.x)))
    gemmu <- purrr::map(.x = gemmu,~.x[,c("date",gemmu_yellan)])%>%stats::setNames(target_ts)
  }
  return(gemmu)
}
#' Display Growth rate results in chart.
#' @author Farid Azouaou
#' @param gemu_tisefka data frame containing growth rate
#' @param target_variable growth rate type linear_Develop, linear_growth_rate, log_growth_rate
#' @param plot_type whether it's a linear or bar chart
#' @return plotly object
#' @export

sekned_gemmu_f <- function(gemmu_tisefka = NULL,target_variable = "linear_growth_rate",plot_type="bar") {

  status_f <- function(x){
    if(is.na(x))return(NA)
    if(sign(x)>0)return("increase")
    if(sign(x)<0)return("decrease")
    return("stable")
  }

  plot_colors_f <- function(x){
    if(x=="increase")return("seagreen")
    if(x=="decrease")return("orange")
    if(x=="stable")return("grey")
    return(NULL)
  }
  status <- sapply(dplyr::pull(gemmu_tisefka,!!target_variable),status_f)
  plot_colors <- sapply(na.omit(unique(status)),plot_colors_f)
  gemmu_tisefka <- gemmu_tisefka%>%dplyr::select(date,!!target_variable)%>%
    mutate(status = status)
  #----------------------------
  plot_mode <- "lines"
  if(plot_type=="bar")plot_mode<-NULL

  p <- gemmu_tisefka %>%
    plotly::plot_ly(x = ~date, y = ~base::get(target_variable), type = plot_type, mode = plot_mode,
                    color =~status,colors = plot_colors) %>%
    plotly::layout(
      xaxis = base::list(title = paste("Time"), tickangle = -45),
      yaxis = base::list(title = "Growth Rate %"),
      margin = base::list(b = 100),
      barmode = "stack", legend = list(orientation = "h", x = 0.35, y = 0)
    )
  p <- p %>% plotly::config(displaylogo = F)
  return(p)
  #----------------------------
}
