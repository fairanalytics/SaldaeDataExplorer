





mded_ukud_s_tsefka <- function(tisefka = NULL,ukud_variables = NULL,ukud_format = NULL){
  tisefka <- dplyr::bind_cols(tisefka%>%dplyr::select(-ukud_variables),tisefka[,ukud_variables]%>%tidyr::unite(date, sep = "-", remove = TRUE, na.rm = FALSE))
  tisefka$date <- ukud_format_complete(ukud_format = ukud_format,ukud_vector = tisefka$date)
  return(tisefka)
}

ukud_format_complete <- function(ukud_format = NULL,ukud_vector = NULL){
  if(ukud_format=="%B-%Y" || ukud_format=="%B-%y" || ukud_format=="%b-%Y" || ukud_format=="%b-%y"){
    ukud_vector <- paste0(ukud_vector,"-01")
    ukud_format <- paste0(ukud_format,"-%d")
  }else if(ukud_format=="%Y" || ukud_format=="%y"){
    ukud_vector <- paste0(ukud_vector,"-01-01")
    ukud_format <- paste0(ukud_format,"-%m-%d")
  }
 return(as.POSIXct(ukud_vector,format = ukud_format))
}

# tisefka <- room_occupancy_tax_by_month_statewide_1
# mded_ukud_s_tsefka(tisefka = tisefka ,ukud_variables = ukud_variables,ukud_format = "%B-%Y")

aggregate_lists <- function(y = NULL){
  unlist(y%>%purrr::map(~sum(.x,na.rm = TRUE)))
}

#' Saldae prepare data: spread
#' @description spread Data into multiple rows and have one uniqute date vector
#' @author Farid Azouaou
#' @param tisefka raw data(uploaded from inputfile)
#' @param anwa value to spread data
#' @param f_anwa   group category to use to spread data
#' @return raw data ready to explore and to analyze
#' @export

zuzer_tisefka <- function(tisefka = NULL,anwa= NULL,f_anwa = NULL){
  tisefka <- tisefka%>%tidyr::pivot_wider(id_cols= date,
                                          names_from = f_anwa,
                                          values_from = anwa,
                                          values_fill = list(NA)
                                          )
  if("list"%in% dlookr::get_class(tisefka)$class){
    tisefka<- tisefka%>%purrr::map_if(is.list,~aggregate_lists(.x))
  }
  tisefka <- tisefka%>%tibble::as.tibble()
  return(tisefka)
}
