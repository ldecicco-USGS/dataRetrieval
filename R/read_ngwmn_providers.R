#' Get NGWMN Provider Data
#' 
#' @description `r get_description("providers", base = "NGWMN")`
#' 
#' @export
#' @param state 
#' `r get_ogc_params("providers", base = "NGWMN")$state$description`
#' @param agency_code 
#' `r get_ogc_params("providers", base = "NGWMN")$agency_code$description`
#' @param organization_type 
#' `r get_ogc_params("providers", base = "NGWMN")$organization_type$description`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("providers", base = "NGWMN")`.
#' The default (`NA`) will return all columns of the data.

#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' 
#' ngwmn_providers <- read_ngwmn_providers(state = "WI")
#' 
#' ngwml_providers2 <- read_ngwmn_providers(state = c("WI", "MN"))
#' 
#' org_type <- read_ngwmn_providers(organization_type = "NWIS")
#'                  
#' }
read_ngwmn_providers <- function(state = NA_character_,
                                 agency_code = NA_character_,
                                 organization_type = NA_character_,
                                 properties = NA_character_,
                                 limit = NA){
  
  service <- "providers"
  
  args <- mget(names(formals()))
  
  return_list <- get_ngwmn_data(args, service)
  
  return_list <- deal_with_empty(return_list, 
                                 properties, 
                                 service,
                                 skipGeometry = TRUE, 
                                 convertType = TRUE,
                                 no_paging = FALSE,
                                 base = "NGWMN")
  
  
  return_list <- sf::st_drop_geometry(return_list)
  
  return(return_list)
}
