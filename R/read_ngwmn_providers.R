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
#' @inheritParams check_arguments_non_api
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
                                 ...,
                                 convertType = getOption("dataRetrieval.convertType"),
                                 no_paging = getOption("dataRetrieval.no_paging"),
                                 chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
                                 limit = getOption("dataRetrieval.limit"),
                                 attach_request = getOption("dataRetrieval.attach_request")){
  
  service <- "providers"
  rlang::check_dots_empty()
  args <- mget(names(formals()))
  
  return_list <- get_ogc_data(
    args = args,
    output_id = "id",
    service = service,
    base = "NGWMN"
  )
  
  
  return_list <- sf::st_drop_geometry(return_list)
  
  return(return_list)
}
