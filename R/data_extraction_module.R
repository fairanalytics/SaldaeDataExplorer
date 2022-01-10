#' Read data from inputfile (CSV)
#' @author Farid Azouaou
#' @param input_file input file
#' @return tbl_df object

ghred_tisefka_csv <- function(input_file = NULL) {
  tisefka_csv <- readr::read_csv(
    file = input_file$datapath,
    # header = TRUE,
    # delim = ",",
    na = c("", "NA","ND", " ", "-", ".", "#")
  )
  tisefka_csv <- data.frame(tisefka_csv, check.names = FALSE)
  return(tisefka_csv)
}


#' Read data from inputfile (Excel)
#' @author Farid Azouaou
#' @param input_file inputfile object
#' @param tawriqt excel sheet name
#' @return tbl_df object

ghred_tisefka_excel <- function(input_file = NULL, tawriqt = NULL) {
  tisefka_excel <- readxl::read_excel(
    path = input_file$datapath,
    # header = TRUE,
    # delim = ",",
    sheet = tawriqt,
    na = c("", "NA","ND", " ", "-", ".", "#")
  )
  tisefka_excel <- data.frame(tisefka_excel, check.names = FALSE)
  return(tisefka_excel)
}

#' Read data main function
#' @author Farid Azouaou
#' @param input_file input file
#' @param tala data source (CSV/Excel/Database)
#' @param tawriqt excel sheet name
#' @return tbl_df object
#' @export

ghred_tisefka_aqerru <- function(input_file = NULL, tala = NULL, tawriqt = NULL) {
  if (tala == "csv") {
    tisefka <- ghred_tisefka_csv(input_file = input_file)
  }
  if (tala == "xlsx") {
    tisefka <- ghred_tisefka_excel(input_file = input_file, tawriqt = tawriqt)
  }
  tisefka <- tisefka%>%janitor::remove_empty(which = c("cols"))%>%
              janitor::remove_constant()
  return(tisefka)
}


#' possible date variables
#' @description Check if the intput element is a date based on a given date format
#' @author Farid Azouaou
#' @param tisefka a character containing date information
#' @return idk
#' @export
detect_possible_date_var <- function(tisefka = NULL){
    date_vars <- unlist(tisefka%>%purrr::map(~length(IsDate(gsub("\\/|_|\\.","-",head(.x,2000))))>0))
    return(names(date_vars)[date_vars])
}


as_POSIXct_without_error <- function(x,SA_date_format,tz="CET"){
  tryCatch(as.POSIXct(x,tz="CET",SA_date_format),error = NA)
}



#' Saldae : is Date
#' @description Check if the intput element is a date based on a given date format
#' @author Farid Azouaou
#' @param mydate a character containing date information
#' @param SA_date_format date format (YYYY/mm/dd , YY-dd-mm)
#' @return idk
#' @export

IsDate <- function(mydate, SA_date_format) {
  mydate<- na.omit(mydate)
  if(class(mydate[1])=="Date")return(TRUE)

  date_format_yellan <- c(
    "%Y-%m-%d", "%Y%m%d","%m-%d-%Y","%d-%m-%Y","%Y-%B-%d","%Y-%m","%Y%m","%m-%Y","%m%Y","%m%d","%d%m","%m-%d","%d-%m"
  )
  date_format_yellan <- c(date_format_yellan,gsub("%Y","%y",date_format_yellan))
  # date_format_yellan <- c(date_format_yellan,"%Y")

  time_extension <- stringr::str_count(mydate[1], pattern = ":")

  if(time_extension==1)date_format_yellan <- paste(date_format_yellan,"%H:%M")
  if(time_extension==2)date_format_yellan <- paste(date_format_yellan,"%H:%M:%S")
    names(date_format_yellan)<- date_format_yellan
    mydate <- clean_date_vect(mydate)
    valid_date_format <- unlist(date_format_yellan%>%purrr::map(~!any(is.na(as_POSIXct_without_error(mydate,SA_date_format = .x,tz="CET")))))
    valid_date_format<- date_format_yellan[valid_date_format]
    return(head(valid_date_format,1))
}

if_is_date <- function(x = NULL){
  x <- gsub("\\/|_","-",x)
  date_formats <- c("ymd","dmy","mdy","my","md","dm")
  valid_format <- date_formats%>%purrr::map(~ !any(is.na(lubridate::parse_date_time(na.omit(x),.x))))
  return(date_formats[unlist(valid_format)])
}


#' Saldae duplicated dates in raw data
#' @description check if there are duplicated dates in raw data and if possible create new variables based on that
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param date_index variable to use as a date
#' @return logical statement
#' @export

tisefka_spread_yella <- function(tisefka = NULL, date_variable = NULL, SA_date_format=NULL,upper_bound = 101) {

  if(is.null(tisefka))return(NULL)

  SA_date_format <- IsDate(mydate = head(dplyr::pull(tisefka,date_variable),1000))

  if(length(SA_date_format)==0)return(NULL)
  tisefka <- tisefka%>%dplyr::select(-!!date_variable)

  tisefka_class <- dlookr::get_class(tisefka)%>%dplyr::filter(class != "numeric")%>%
    dplyr::pull(variable)%>%paste()
  return(tisefka_class)
  date_index <- which(date_variable == colnames(tisefka))

  tisefka_diagnosis <- dlookr::diagnose(.data = tisefka)
  tisefka_diagnosis <- data.frame(tisefka_diagnosis, check.names = FALSE)
  rownames(tisefka_diagnosis) <- tisefka_diagnosis$variables
  if (tisefka_diagnosis[date_index, "unique_rate"] == 1) {
    return(NULL)
  }
  tisefka_diagnosis <- tisefka_diagnosis[-date_index, ]
  if (min(tisefka_diagnosis[, "unique_count"]) > upper_bound) {
    return(NULL)
  }
  tisefka_diagnosis <- tisefka_diagnosis[tisefka_diagnosis[, "unique_count"] <= upper_bound, ]
  if (max(tisefka_diagnosis[, "unique_count"]) == 1) {
    return(NULL)
  }
  myspread_variable <- tisefka_diagnosis[tisefka_diagnosis[, "unique_count"] <= upper_bound, "variables"]
  myspread_variable <- myspread_variable[myspread_variable != date_variable]
  return(myspread_variable)
}
#' Saldae aggregation function
#' @author Farid Azouaou
#' @param aggregation_metric Maximum, Sum, Average, Minimum , Median
aggregation_fun<- function(aggregation_metric= NULL){
  if(aggregation_metric=="Sum"){
    return(sum)
  }
  if(aggregation_metric=="Average"){
    return(mean)
  }
  if(aggregation_metric=="Maximum"){
    return(max)
  }
  if(aggregation_metric=="Minimum"){
    return(min)
  }
  if(aggregation_metric=="Median"){
    return(median)
  }
}

clean_date_vect <- function(date_vect= NULL){
  date_vect <- gsub("\\/|\\.|_","-",date_vect)
  return(date_vect)
}

#' Saldae prepare data
#' @description Prepare raw data using date_variable as row names
#' @author Farid Azouaou
#' @param tisefka raw data(uploaded from inputfile)
#' @param date_variable variable to use as date
#' @param SA_date_format date format
#' @param spread_value value to spread data
#' @param spread_key   group category to use to spread data
#' @param aggregation_metric used metric to aggregate Sum, Average, Maximum,...
#' @return raw data ready to explore and to analyze
#' @export

sbed_tisefka <- function(tisefka = NULL, date_variable = NULL, SA_date_format = "YYYY-MM-DD",aggregation_metric=NULL, spread_value = NULL, spread_key = NULL) {

  if(is.null(tisefka))return(NULL)

  SA_date_format <- IsDate(mydate = head(dplyr::pull(tisefka,date_variable),1000))

  if(length(SA_date_format)==0)return(NULL)
    date_index <- which(date_variable==colnames(tisefka))
    colnames(tisefka)[date_index] <- "date"
    if(class(tisefka$date)[1]!="Date"){
      tisefka <- tisefka%>%mutate(date = as.POSIXct(clean_date_vect(date),format = SA_date_format))
    }
  if(!is.null(aggregation_metric)){
    my_aggregation_fun <- aggregation_fun(aggregation_metric = aggregation_metric)
    tisefka <- tisefka%>%dplyr::group_by(date)%>%
      dplyr::summarise_if(is.numeric, my_aggregation_fun, na.rm = TRUE)
  }

  if (!is.null(spread_value) & !is.null(spread_key)) {
    tisefka <- zuzer_tisefka(tisefka = tisefka , anwa = spread_value,f_anwa = spread_key)
  }


  tisefka <- tisefka %>% dplyr::filter(!is.na(date))

  return(tisefka)
}

#' Saldae available date format
#' @author Farid Azouaou
#' @param ukud raw data
#' @return vector of date format
#' @export
SA_date_format_yellan <- function(ukud = NULL) {
  date_format_yellan <- c(
    "YYYY-MM-DD", "YYYY/MM/DD", "YYYY.MM.DD", "YYYYMMDD","MM-DD-YYYY","MM/DD/YYYY",
    "DD-MM-YYYY", "DD/MM/YYYY", "DD.MM.YYYY","YYYY-MON-DD","YYYY/MONTH/DD"
  )
  return(date_format_yellan)
}
#' Saldae convert into R-based date format
#' @param my_date_format
#' @return R-based date format

SA_date_format_convert <- function(my_date_format = NULL){
  date_format_yellan <- gsub("YYYY","%Y",my_date_format)
  date_format_yellan <- gsub("YY","%y",date_format_yellan)
  date_format_yellan <- gsub("MM","%m",date_format_yellan)
  date_format_yellan <- gsub("DD","%d",date_format_yellan)
  date_format_yellan <- gsub("DD","%d",date_format_yellan)
  date_format_yellan <- gsub("MON","%b",date_format_yellan)
  date_format_yellan <- gsub("MONTH","%B",date_format_yellan)
return(gsub("\\/","-",date_format_yellan))
}
#' Saldae arrange  data
#' @author Farid Azouaou
#' @param tifeka raw data
#' @return tbl_df object
fren_tisefka <- function(tifeka = NULL) {
  # tisefka    <- tisefka %>% dplyr::distinct(date, .keep_all = TRUE)
  tisefka <- tisefka %>% dplyr::arrange(as.POSIXct(date, tz = "CET"), .by_group = TRUE)
}


#' Time interval
#' @author Farid Azouaou
#' @param x time vector

ukud_aciwen <- function(x = NULL) {
  anda_tebda <- length(x) + 1 - length(zoo::na.trim(x, sides = "left"))
  anda_tfuk <- length(zoo::na.trim(x, sides = "right"))
  return(c(anda_tebda, anda_tfuk))
}


#' Saldae  Time interval
#' @author Farid Azouaou
#' @param tisefka data frame with dates as row names
#' @return time intervals
#' @export

ukud_tilisa_f <- function(tisefka = NULL,date_vector= NULL) {
  ukud_tilisa <- t(apply(tisefka, 2, function(x) ukud_aciwen(x)))
  ukud_tilisa <- data.frame("Start Date" = date_vector[ukud_tilisa[, 1]], "End Date" = date_vector[ukud_tilisa[, 2]], check.names = FALSE)
  # rownames(ukud_tilisa) <- colnames(tisefka)
  return(ukud_tilisa)
}
