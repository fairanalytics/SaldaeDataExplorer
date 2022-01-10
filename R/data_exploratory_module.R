
#' Data Aggregation based on a given time unit and grouping pattern
#' @author Farid Azouaou
#' @param  tisefka  data frame including date variable
#' @param  sdukkel group by a discrete variable
#' @param  base_unit time unit of the raw data
#' @param  time_unit time unit of the aggregated data
#' @param  aggregation_metric aggregation metric (sum mean max min)
#' @return list containing aggregated data

data_aggregation_f <- function(tisefka = NULL, time_unit = NULL, base_unit = NULL, target_ts = NULL, sdukkel = NULL) {
  report_output <- base::list()
  if (time_unit != base_unit) {
    #-----------------------------------------------
    if (time_unit == "seconds") {
      #--------- unit which helps to determine on which frequency to summarize
      unit_format <- "%Y-%m-%d %H:%M:%S"
      #----------tasetta is when the upper unit does not have enough data for the last point (10 months in the last year)
    }
    if (time_unit == "minutes") {
      #--------- unit which helps to determine on which frequency to summarize
      unit_format <- "%Y-%m-%d %H:%M"
    }
    if (time_unit == "hours") {
      unit_format <- "%Y-%m-%d %H"
    }
    if (time_unit == "days") {
      unit_format <- "%Y-%m-%d"
    }
    if (time_unit == "months") {
      unit_format <- "%Y-%m"
    }
    if (time_unit == "years") {
      unit_format <- "%Y"
    }
    #--------------------- average
    if (!time_unit %in% c("weeks", "quarters")) {
      time_vect_group <- format(as.POSIXct(tisefka$date, tz = "CET"), unit_format)
    } else if (time_unit == "quarters") {
      time_vect_group <- paste(format(as.POSIXct(tisefka$date, tz = "CET"), "%Y"), lubridate::quarter(tisefka$date))
    } else if (time_unit == "weeks") {
      time_vect_group <- paste(format(as.POSIXct(tisefka$date, tz = "CET"), "%Y"), lubridate::week(tisefka$date))
    } else {
      stop("time unit not found")
    }
    tasetta <- detect_number_basis_units_in_upper_unit(upper_unit = time_unit, basis_unit = base_unit, last_date = tail(as.POSIXct(tisefka$date, tz = "CET"), 1))

    if (length(unique(tail(time_vect_group, tasetta))) > 1) {
      tasetta_dat <- tail(time_vect_group, tasetta)
      incomplete <- length(which(tasetta_dat == tail(tasetta_dat, 1)))
      time_vect_group <- head(time_vect_group, -incomplete)
      tisefka <- head(tisefka, -incomplete)
    }

    #-----------------------------
    aqerru <- detect_number_basis_units_in_upper_unit(upper_unit = time_unit, basis_unit = base_unit, last_date = head(as.POSIXct(tisefka$date, tz = "CET"), 1))
    if (length(unique(head(time_vect_group, aqerru))) > 1) {
      aqerru_dat <- head(time_vect_group, aqerru)
      incomplete <- length(which(aqerru_dat == head(aqerru_dat, 1)))
      time_vect_group <- tail(time_vect_group, -incomplete)
      tisefka <- head(tisefka, -incomplete)
    }
    #------------------------------
    date_by_unit <- data.frame(time_vect_group, date = tisefka$date) %>% dplyr::distinct(time_vect_group, .keep_all = TRUE)

    target_dat <- tisefka[,target_ts]
    # colnames(target_dat) <- "target_variable"

    # if (!is.null(sdukkel)) {
    #   target_dat$sdukkel <- tisefka[, sdukkel]
    # } else {
    #   target_dat$sdukkel <- target_ts
    #   sdukkel <- target_ts
    # }

    upper_summary_mean <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit) %>% # group by the day column
      dplyr::summarise_at(dplyr::vars(!!target_ts),mean, na.rm = TRUE)%>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))
    #--------------------- Sum --------------
    upper_summary_sum <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit) %>% # group by the day column
      dplyr::summarise_at(dplyr::vars(!!target_ts),sum, na.rm = TRUE)%>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))

    #--------------------- maximum
    upper_summary_max <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit) %>% # group by the day column
      dplyr::summarise_at(dplyr::vars(!!target_ts),max, na.rm = TRUE)%>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))

    #--------------------- min
    upper_summary_min <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit) %>% # group by the day column
      dplyr::summarise_at(dplyr::vars(!!target_ts),min, na.rm = TRUE)%>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))

    upper_summary_median <- target_dat %>%
      dplyr::mutate(upper_unit = time_vect_group) %>%
      dplyr::group_by(upper_unit) %>% # group by the day column
      dplyr::summarise_at(dplyr::vars(!!target_ts),median, na.rm = TRUE)%>%
      dplyr::mutate(date = as.POSIXct(date_by_unit$date, tz = "CET"))


    report_output[["ts_aggregated_by_sum"]] <- upper_summary_sum
    report_output[["ts_aggregated_by_average"]] <- upper_summary_mean
    report_output[["ts_aggregated_by_max"]] <- upper_summary_max
    report_output[["ts_aggregated_by_min"]] <- upper_summary_min
    report_output[["ts_aggregated_by_median"]] <- upper_summary_median
  } else {
    # tisefka[, "date"] <- as.POSIXct(rownames(tisefka))
    if (!is.null(sdukkel)) {
      target_dat <- tisefka[, c(target_ts, "date")]
      # colnames(target_dat) <- c("target_variable", "date")
      target_dat$sdukkel <- tisefka[, sdukkel]
      target_dat <- dplyr::tbl_df(target_dat)
      report_output$tisefka_ticeqfin <- target_dat %>%
        tidyr::spread(unique(sdukkel), !!target_variable)
    }
    report_output$tisefka <- tisefka[, c(target_ts, "date")]
  }
  return(report_output)
}
#---------------------------------------


#' Data Exploration Main wrapper function
#' @author Farid Azouaou
#' @param  tisefka  data frame including date variable
#' @param  tisefka_report exploratory report of raw data
#' @param  sdukkel group by a discrete variable
#' @param  sdukkel_ih logical variable stating whether to group or no
#' @param  base_unit time unit of the raw data
#' @param  time_unit time unit of the aggregated data
#' @param  aggregation_metric aggregation metric (sum mean max min)
#' @return list containing aggregated data with seasonally adjusted and anomaly detection infrmation
#' @export
data_exloration_aqerru <- function(tisefka = NULL, sdukkel = NULL,
                                   sdukkel_ih = NULL, aggregation_metric = NULL, time_unit = NULL, base_unit = NULL, target_ts = NULL, ts_actions = NULL) {
  if (is.null(tisefka)) return(NULL)
  report_output <- base::list()
  #----------------------
  if (is.null(sdukkel_ih)) {
    sdukkel <- NULL
  } else if (sdukkel_ih == FALSE) {
    sdukkel <- NULL
  }

  if (time_unit == base_unit) aggregation_metric <- NULL
  report_output_data <- data_aggregation_f(
    tisefka = tisefka,
    sdukkel = sdukkel,
    time_unit = time_unit,
    base_unit = base_unit, target_ts = target_ts
  )

  #----------------------------------------------
  if (is.null(aggregation_metric)) {
    if (!is.null(sdukkel)) tisefka <- report_output_data$tisefka_ticeqfin
    if (is.null(sdukkel)) tisefka <- report_output_data$tisefka
  } else {
    tisefka <- report_output_data[[paste0("ts_aggregated_by_", tolower(aggregation_metric))]]
  }
  #----------------------seasonal adjustement
  season_adjust <- FALSE
  if (is.null(sdukkel)) {
    report_output$tisefka <- season_adjust_f(tisefka = tisefka[, target_ts], time_unit = time_unit)
  }
  if (nrow(tisefka) < 50000) anomaly_detection <- TRUE
  if (anomaly_detection == TRUE & is.null(sdukkel)) {
    anomaly_output <- anomaly_detection_nnegh(tisefka = tisefka, anomaly_mode = "anomalize", target_ts = target_ts)
  }
#----------------------------------
  report_output$tisefka <-  purrr::map(.x = names(report_output$tisefka),~dplyr::bind_cols(report_output$tisefka[[.x]][,-1],anomaly_output[[.x]]) )%>%
    stats::setNames(names(report_output$tisefka))
  report_output$time_unit <- time_unit
  report_output$target_ts <- target_ts
  report_output$aggregation_metric <- aggregation_metric
#----------------------------------
  return(report_output)
}

#---------------------------- data exploration (boxplot)-----------------

#' Display box plot
#' @author Farid Azouaou
#' @param  tisefka  data frame including date variable
#' @param  x explaining variable
#' @param  y explained variable
#' @param  aggregation_metric aggregation metric (sum mean max min)
#' @return a boxplot plotly object


data_exloration_box <- function(tisefka = NULL, x = NULL, y = NULL, aggregation_metric = "Sum") {
  #-------------------------------
  if (is.null(x)) {
    p <- plotly::plot_ly(x = ~ tisefka[, y], type = "box", text = y, name = y)
    p <- p %>% plotly::layout(title = paste(y, "Statistics Overview"), xaxis = base::list(title = ""), yaxis = base::list(title = "y"))
  } else {
    tisefka$x_temporary <- as.factor(tisefka[, x])
    tisefka$y_temporary <- tisefka[, y]

    p <- plotly::plot_ly(tisefka, y = ~y_temporary, color = ~x_temporary, type = "box")
    p <- p %>%
      plotly::layout(
        title = paste(y, "statistics overview  group by", x),
        yaxis = base::list(title = y)
      )
  }

  return(p)
}

#' Time based Seasonality frequencies
#' @description detect based on time unit possible frequencies
#' @author Farid Azouaou
#' @param  time_unit time unit used to detect frequency
#' @return a vector of existing frequencies for that time unit

time_based_seasonality <- function(time_unit = NULL) {
  #----------------------
  if (time_unit == "seconds") {
    return(c(10, 60))
  }
  if (time_unit == "minutes") {
    return(c(10, 60))
  }
  if (time_unit == "hours") {
    return(c(4, 6, 8, 12, 24))
  }
  if (time_unit == "days") {
    return(c(5, 7, 30))
  }
  if (time_unit == "weeks") {
    return(c(4))
  }
  if (time_unit == "months") {
    return(c(3, 6, 12))
  }
  if (time_unit == "quarters") {
    return(4)
  }
  if (time_unit == "years") {
    return(10)
  }
  #----------------------
}
#' Seasonal Adjustement
#' @description extract from time series the seasonal , trend and remainder components based on a time frequency that we extract from time unit
#' @author Farid Azouaou
#' @param  ts_x  target time series
#' @param tuzyat possible frequencies
#' @return seasonally adjusted time serie and statement about frequency
tes3a_ddurt_f <- function(ts_x = NULL, tuzyat = NULL){
  for (tuzya in tuzyat[,1,drop=T]) {
    tes3a_ddurt <- FALSE
    if (tuzya > 1 & length(ts_x) / tuzya > 3) {
      tes3a_ddurt <- seastests::isSeasonal(ts(ts_x, frequency = tuzya), test = "wo", freq = NA)
      ts_temp <- forecast::seasadj(stats::stl(x = ts(ts_x, frequency = tuzya), s.window = 2, s.degree = 1))
      tuzya <- paste0("Seaso_adju_",tuzya)
      ts_x <- ts_x%>%dplyr::mutate(tuzya := as.numeric(ts_temp))
    }
  }
return(ts_x)
}


#' Seasonal Adjustement
#' @description extract from time series the seasonal , trend and remainder components based on a time frequency that we extract from time unit
#' @author Farid Azouaou
#' @param  ts_x  target time series
#' @param  time_unit time unit used to detect frequency
#' @return a list containing frequency and seasonally adjusted time serie

season_adjust_f <- function(tisefka = NULL, time_unit = NULL) {
  #-----------------------------
  output_season <- base::list()
  tisefka <- purrr::map_df(.x= tisefka,.f = interp_na_value, interp_mode = "spline")
  for(k in colnames(tisefka)){
    a<-interp_na_value(tisefka[,k],interp_mode = "spline")
  }
  tuzyat_ukud <- time_based_seasonality(time_unit = time_unit)
  tuzyat_ukud <- rep(tuzyat_ukud,times = ncol(tisefka))
  #-------------------
  tuzyat <- purrr::map_df(.x = tisefka,.f = forecast::findfrequency)
  tuzyat <- rbind(tuzyat,tuzyat_ukud)

  output_season <- purrr::map(.x = colnames(tisefka),~tes3a_ddurt_f(ts_x =tisefka[,.x] ,tuzyat = unique(tuzyat[,.x])))%>%
    stats::setNames(colnames(tisefka))
  #----------------------------
  return(output_season)
  #-------------------
}
#'Display raw data including additional insights such seasonally adjusted , outliers(anomalies) and quadratic fit
#' @author Farid Azouaou
#' @param tisefka_report list containing exploratory report
#' @param ts_actions I don't remember
#' @param sekned_settings display settings
#' @param plot_settings  settings to use for plotting such as color palette
#' @param graph_type Graph type whether a "marker" , "line" , "marker+line", "historgram", "density"
#' @return plolty reactive object
#' @export

sekned_tisefka_aggregator <- function(tisefka = NULL,time_unit = NULL ,aggregation = NULL,ts_actions = NULL, graph_type = "Lines",plot_settings= NULL) {
  # Base plot with date axis

  target_variable <- colnames(tisefka)[2]
  if (is.null(tisefka)) return(NULL)
  # tisefka <- tisefka_report$tisefka
  # color_by <- tisefka_report$group_by
  # time_unit <- tisefka_report$time_unit

  # aggregation <- tisefka_report$aggregation_metric
  color_by<- NULL
  season_adjust <- include_smoothing <- svegned_anomaly <- FALSE

  if (length(ts_actions) > 0) {
    include_smoothing <- "Smoothing" %in% ts_actions
    season_adjust <- ("Season. Adjust" %in% ts_actions) & ("Seaso_adju_"%in%colnames(tisefka))
    svegned_anomaly <- ("Anomaly" %in% ts_actions) & ("Anomalies"%in%colnames(tisefka))
  }

  x_axis <- paste("Time in ", time_unit)
  graph_type <- base::tolower(graph_type)
  if (is.null(color_by)) {
    graph_fill <- bar_type <- graph_mode <- NULL

    x_ax <- "date"
    y_ax <- target_variable

    if(is.null(plot_settings)){
      RAW_col      <- "#00AFBB"
      SA_col       <- "brown"
      Anomaly_col  <- "orange"
    }else{
      RAW_col      <- plot_settings$colors_inu[1]
      SA_col       <- plot_settings$colors_inu[2]
      Anomaly_col  <- plot_settings$colors_inu[3]
    }

    if (graph_type %in% c("markers")) {
      graph_type <- "scatter"
      graph_mode <- "markers"
      RAW_col <- NULL
    }
    if (graph_type %in% c("lines", "lines+markers")) {
      graph_mode <- graph_type
      graph_type <- "scatter"
    }
    if (graph_type %in% c("filled")) {
      graph_mode <- "lines"
      graph_type <- "scatter"
      graph_fill <- "tozeroy"
    }
    if (graph_type %in% c("bar1")) {
      graph_type <- "bar"
      bar_type <- NULL
    }

    if (graph_type %in% c("density")) {
      tisefka <- stats::density(tisefka[,target_variable, drop = T],na.rm = TRUE)
      tisefka <- data.frame(date = tisefka$x, target_variable = tisefka$y)
      graph_mode <- "lines"
      graph_type <- "scatter"
      graph_fill <- "tozeroy"
      x_ax <- list(title= target_variable)
      y_ax <- list(title = "probability")
      season_adjust <- include_smoothing <- svegned_anomaly <- FALSE
    }

    p <- plotly::plot_ly(tisefka) %>%
      plotly::add_trace(
        x = ~date, y = ~base::get(target_variable), type = graph_type, mode = graph_mode, fill = graph_fill, name = y_ax, yaxis = "y",
        line = base::list(color = RAW_col),
        text = ~ paste(y_ax)
      ) %>%
      plotly::layout(
        title = NULL,
        xaxis = base::list(title= x_ax),
        yaxis = base::list(title = y_ax, side = "left", showgrid = FALSE, zeroline = FALSE)
      )

    if (svegned_anomaly == TRUE) {
      p <- p %>%
        plotly::add_trace(x = ~date, y = ~Anomalies, type = "scatter", mode = "markers", name = "Anomalies", yaxis = "y"
                          )
    }
    if (season_adjust == TRUE) {
      SA_variable <- grepl("Seaso_adju_",colnames(tisefka))[1]
      p <- p %>%
        plotly::add_trace(
          x = ~date, y = ~get(SA_variable), type = "scatter", mode = "lines", name = "SA", yaxis = "y",
          line = base::list(color = SA_col)
        )
    }

    if (include_smoothing == TRUE) {
      xx <- 1:nrow(tisefka)
      smoothing <- stats::loess(as.formula(paste0(target_variable," ~ xx")), data = tisefka)
      smoothing.pred <- predict(smoothing, se = TRUE)
      ll.df <- data.frame(
        x = smoothing$x, fit = smoothing$fit,
        lb = smoothing$fit - (1.96 * smoothing$s),
        ub = smoothing$fit + (1.96 * smoothing$s)
      )

      p <- plotly::add_lines(p, x = ~date, y = smoothing.pred$fit, name = "Smoothing", line = base::list(color = "#E7B800", width = 2))
      p <- plotly::add_ribbons(p,
        x = ~date, ymin = ll.df$lb, ymax = ll.df$ub, name = "95% CI", line = base::list(opacity = 0.4, width = 0, color = "#E7B800"),
        fillcolor = "rgba(7, 164, 181, 0.2)"
      )
      p <- plotly::layout(p, y_axis = y_ax)
    }
    p <- plotly::layout(p, legend = list(orientation = "h", x = 0.35, y = 100))%>%plotly::config(displaylogo = F)
    return(p)
  }
  if (graph_type == "density") {
    date_den <- tisefka %>%
      dplyr::select(-date) %>%
      dplyr::select(1)
    date_den <- stats::density(date_den[, 1, drop = T], na.rm = TRUE)$x
    tisefka <- apply(tisefka %>% dplyr::select(-date), 2, function(x) stats::density(x, na.rm = TRUE)$y)
    tisefka <- data.frame(date = date_den, tisefka, check.names = FALSE)
  }
  tisefka <- dplyr::tbl_df(reshape2::melt(tisefka, id = "date"))
  if (graph_type == "lines") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "scatter", mode = "lines", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100),
        barmode = "stack"
      )
  }

  if (graph_type == "stack") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "bar", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100),
        barmode = "stack"
      )
  }
  if (graph_type == "bar") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "bar", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100),
        barmode = "group"
      )
  }
  if (graph_type == "scatter") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "scatter", color = ~variable) %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100)
      )
  }
  if (graph_type == "density") {
    p <- tisefka %>%
      plotly::plot_ly(x = ~date, y = ~value, type = "scatter", color = ~variable, fill = "tozeroy", mode = "line") %>%
      plotly::layout(
        xaxis = base::list(title = paste("Time in", time_unit), tickangle = -45),
        yaxis = base::list(title = target_ts),
        margin = base::list(b = 100)
      )
  }
  p <- plotly::layout(p, title = NULL, legend = list(orientation = "h", x = 0.35, y = 100))
  return(p)
}

#'Saldae countries flags
#' @author Farid Azouaou
#' @export

annayen_n_tumura_f <- function() {
  flags <- c(
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/au.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/jp.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/se.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fr.svg",
    "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/us.svg"
  )
  names(flags) <- c("AUS", "GER", "UK", "JAP", "SWD", "FRA", "USA")
  warning("wer3ad tfuk tagi n w annayen!!!")
  return(flags)
}

#'Saldae Pie Chart
#' @author Farid Azouaou
#' @param tisefka data with date as row names
#' @param x x-axis variable
#' @param y y-axis variable
#' @param aggregation sum mean max
#' @param bgrnd_color background color
#' @return pie chart plotly object
#' @export
data_exploration_pie <- function(tisefka = NULL, x = NULL, y = NULL, aggregation_metric = "sum", bgrnd_color = "black") {

  # Add label position
  if (is.null(x)) {
    return(NULL)
  }

  tisefka$x_temporary <- as.factor(tisefka[, x])
  tisefka$y_temporary <- tisefka[, y]
  tisefka <- dplyr::tbl_df(tisefka)
  tisefka <- tisefka %>%
    dplyr::group_by(x_temporary) %>%
    dplyr::summarise(count = n())

  p <- tisefka %>%
    plotly::plot_ly(labels = ~x_temporary, values = ~count) %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(
      title = paste("Categorical variables overview:", x), showlegend = T,
      paper_bgcolor = bgrnd_color,
      plot_bgcolor = bgrnd_color,
      xaxis = base::list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = base::list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    ) %>%
    plotly::config(displaylogo = F)

  return(p)
}


#' rearrange into Data frame to read it from DT functions
sekned_tisefka_DT <- function(tisefka = NULL, gzem = 3) {
  rownames(tisefka) <- tisefka$date
  tisefka$date <- NULL
  tisefka <- round(tisefka, gzem)
  return(tisefka)
}
#' Reactive Data Exploration including statistics data quality and graphic output.
#' @param tisefka Raw data(row names as date)
#' @author Farid Azouaou
#' @param numeric_variables the numerical variables
#' @param tisefka_report Data report including outliers , statistics and quality information
#' @return  datahandontable object including start end date , statistics outliers information and charts(boxplot,distribution and raw plot)
#' @export

Handson_exploration <- function(tisefka = NULL, tisefka_report = NULL, numeric_variables = NULL) {

  numeric_variables <-dlookr::get_class(tisefka)%>%dplyr::filter(class=="numeric")%>%dplyr::pull(variable)%>%paste()
   date_vector<- tisefka$date

  tisefka <- tisefka[, numeric_variables, drop = F]

  tisefka_density <- apply(tisefka, 2, function(x) stats::density(x,na.rm = TRUE)$y)
  relevant_variables <- c("variables", "outliers_cnt")
  DF <- tisefka_report$outliers[, relevant_variables]
  DF <- cbind(DF, ukud_tilisa_f(tisefka = tisefka,date_vector = date_vector))
  DF_stat <- tisefka_report$beschreibung[, c("n", "na", "mean", "sd")]
  DF <- DF%>%dplyr::bind_cols(DF_stat)

  # DF <- DF[numeric_variables, ]
  DF$Chart <- sapply(
    1:ncol(tisefka),
    function(x) {
      if(!is.numeric(tisefka_density[, x]))return(NULL)
      jsonlite::toJSON(list(
        values = tisefka%>%dplyr::pull(!!x),
        options = list(type = "line",col="green")
      ),na="null")
    }
  )

  DF$Density <- sapply(
    1:ncol(tisefka),
    function(x) {
      if(!is.numeric(tisefka_density[, x]))return(NULL)
      jsonlite::toJSON(list(
        values = tisefka_density[, x],
        options = list(type = "line")
      ))
    }
  )
  DF$Box <- sapply(
    1:ncol(tisefka),
    function(x) {
      if(!is.numeric(tisefka_density[, x]))return(NULL)
      jsonlite::toJSON(list(
        values = tisefka%>%dplyr::pull(!!x),
        options = list(type = "box")
      ),na="null")
    }
  )

  rh_plot <- rhandsontable::rhandsontable(DF, rowHeaders = NULL, width = 1000, height = 300) %>%
    rhandsontable::hot_col("Chart", renderer = htmlwidgets::JS("renderSparkline")) %>%
    rhandsontable::hot_col("Density", renderer = htmlwidgets::JS("renderSparkline")) %>%
    rhandsontable::hot_col("Box", renderer = htmlwidgets::JS("renderSparkline"))
  # rhandsontable(DF, rowHeaders = NULL, width = 550, height = 300) %>%
  #   rhandsontable::hot_col("chart", renderer = htmlwidgets::JS("renderSparkline"))
  return(rh_plot)
}
