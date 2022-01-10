

#-------------------
#' Saldae Time-based grouping
#' @description possible grouping time-based variables
#' @author Farid Azouaou
#' @param ukud_unit time unit
#' @return time-based features(month name, days of the week , workingday or week end)
#' @export

ukud_acu_asdukkel <- function(ukud_unit = NULL) {
  #-------------------------------------
  if (ukud_unit == "seconds") {
    return(NULL)
  }
  if (ukud_unit == "minutes") {
    return(NULL)
  }
  if (ukud_unit == "hours") {
    return(c("am.pm"))
  }
  if (ukud_unit == "days") {
    return(c("am.pm"))
  }
  if (ukud_unit == "week") {
    return(c("wday.lbl"))
  }
  if (ukud_unit == "months") {
    return(c("wday.lbl", "mweek"))
  }
  if (ukud_unit == "quarters") {
    return(c("wday.lbl", "mweek"))
  }
  if (ukud_unit == "years") {
    return(c("quarter", "half"))
  }
  #--------------------------------------
}

#-------------------
# create html file. file name is EDA_Report.htmleda_report(carseats, "US", output_format = "html")
#' Saldae : data diagnosis
#' @description draw a diagnosis on data  from quality and statitistics perspectives.
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param categoricals_ukud time_based gruping variables
#' @param ukud_unit time unit
#' @return list containing detailed diagnos (quality , comleteness outliers,...)
#' @export

data_diagnosis_f <- function(tisefka = NULL, categoricals_ukud = NULL, ukud_unit = NULL) {
  output_data_quality <- list()

  output_data_quality[["diagnosis"]]    <- dlookr::diagnose(.data = tisefka)
  output_data_quality[["beschreibung"]] <- dlookr::describe(.data = tisefka)
  output_data_quality[["outliers"]]     <- dlookr::diagnose_outlier(.data = tisefka)

  # rownames(output_data_quality[["beschreibung"]]) <- rownames(output_data_quality[["outliers"]]) <- output_data_quality[["beschreibung"]]$variable

  #----------------------------- afe-d ukuden yeddan akk d ukud_unit


  output_data_quality[["categoricals"]] <- output_data_quality$diagnosis%>%dplyr::filter(types %in%c("character","factor", "POSIXct")|unique_count < 100)%>%dplyr::pull(variables)
  output_data_quality[["categoricals_unique_values"]] <- tisefka%>%dplyr::select(!!output_data_quality[["categoricals"]])%>%purrr::map(~unique(.x))
  #-----------------------------------------------------------------
  return(output_data_quality)
}
#------------------------------------------
#' Saldae : data diagnosis display
#' @author Farid Azouaou
#' @param tisekfa data diagnosis information
#' @param target what to display (diagnosis or outliers)
#' @return DT interactive object
#' @export
data_quality_DT <- function(tisefka = NULL, target = "diagnosis") {
  if(target == "diagnosis") {
    tisefka$availablity_percent <- round(100 - tisefka$missing_percent, 0)
    tisefka <- tisefka[, c("variables", "types", "unique_rate", "availablity_percent")]

    tisefka$missing_percent <- NULL
    tisefka["zero_rate", c("unique_rate", "availablity_percent")] <- 0
    tisefka$unique_rate <- round(tisefka$unique_rate, 2)
    diagnosis_DT <- DT::datatable(tisefka,
      options = list(
        searching = FALSE,
        pageLength = nrow(tisefka),
        dom = "t"
      ),
      rownames = FALSE,
      selection = "none"
    ) %>%
      DT::formatStyle("unique_rate",
        background = DT::styleColorBar(tisefka$unique_rate, "#9ACD32"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      DT::formatStyle("availablity_percent",
        background = DT::styleColorBar(tisefka$availablity_percent, "#228B22"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
    #---------- take out the Zero_rate benchmark element
    diagnosis_DT$x$data <- head(diagnosis_DT$x$data, -1)
  }
  if (target == "outliers") {
    tisefka$outliers_ratio <- round(tisefka$outliers_ratio, 2)
    tisefka["lower", c("outliers_ratio")] <- 0
    tisefka["upper", c("outliers_ratio")] <- 100
    tisefka <- tisefka[, c("variables", "outliers_cnt", "outliers_ratio")]
    colnames(tisefka) <- c("variables", "Nr. outliers", "Outliers rate")
    diagnosis_DT <- DT::datatable(tisefka,
      options = list(
        searching = FALSE,
        pageLength = nrow(tisefka),
        dom = "t"
      ),
      rownames = FALSE,
      selection = "none"
    ) %>%
      DT::formatStyle("Outliers rate",
        background = DT::styleColorBar(tisefka[["Outliers rate"]], "orange"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
    diagnosis_DT$x$data <- head(diagnosis_DT$x$data, -2)
  }

  return(diagnosis_DT)
}
