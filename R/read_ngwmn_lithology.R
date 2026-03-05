#' Get NGWMN Lithology Observations
#' 
#' @description `r get_description("lithologyObs", base = "NGWMN")`
#' 
#' @export
#' @param monitoring_location_id 
#' `r get_ogc_params("lithologyObs", base = "NGWMN")$monitoring_location_id$description`
#' @param monitoring_location_obs_number 
#' `r get_ogc_params("lithologyObs", base = "NGWMN")$monitoring_location_obs_number$description`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("lithologyObs", base = "NGWMN")`.
#' The default (`NA`) will return all columns of the data.

#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution. 
#' 
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "AKDNR-535134236016630"
#' ngwmn_lith <- read_ngwmn_lithology(monitoring_location_id = site)
#' 
#' ngwmn_lith_sub <- read_ngwmn_lithology(monitoring_location_id = site,
#'                      monitoring_location_obs_number = 2)
#'                      
#' sites <- c("ISWS-P428197", "ISWS-P428197",
#'            "AKDNR-535143966816631", "AKDNR-535134236016630")
#' ngwml_lith_sites <- read_ngwmn_lithology(monitoring_location_id = sites)
#'                  
#' }
read_ngwmn_lithology <- function(monitoring_location_id = NA_character_,
                                 monitoring_location_obs_number = NA_character_,
                                 properties = NA_character_,
                                 limit = NA,
                                 no_paging = FALSE){
  
  service <- "lithologyObs"
  
  args <- mget(names(formals()))
  
  return_list <- get_ngwmn_data(args, service)
  
  return_list <- deal_with_empty(return_list, 
                                 properties, 
                                 service,
                                 skipGeometry = TRUE, 
                                 convertType = TRUE,
                                 no_paging = no_paging,
                                 base = "NGWMN")
  
  
  return_list <- sf::st_drop_geometry(return_list)
  
  return(return_list)
}
