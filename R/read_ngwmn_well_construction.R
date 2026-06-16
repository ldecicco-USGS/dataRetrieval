#' Get NGWMN Well Construction Data
#'
#' @description `r get_description("constructionObs", base = "NGWMN")`
#'
#' @export
#' @param monitoring_location_id
#' `r get_ogc_params("constructionObs", base = "NGWMN")$monitoring_location_id$description`
#' @param monitoring_location_obs_number
#' `r get_ogc_params("constructionObs", base = "NGWMN")$monitoring_location_obs_number$description`
#' @param material
#' `r get_ogc_params("constructionObs", base = "NGWMN")$material$description`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("constructionObs", base = "NGWMN")`.
#' The default (`NA`) will return all columns of the data.
#' @inheritParams check_arguments_non_api
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "USGS-272838082142201"
#' ngwmn_well <- read_ngwmn_well_construction(monitoring_location_id = site)
#'
#' ngwmn_well_sub <- read_ngwmn_well_construction(monitoring_location_id = site,
#'                      monitoring_location_obs_number = 2)
#'
#' sites <- c("USGS-272838082142201", "USGS-404159100494601",
#'            "USGS-401216080362703", "MBMG-702934")
#' ngwml_well_sites <- read_ngwmn_well_construction(monitoring_location_id = sites)
#'
#' }
read_ngwmn_well_construction <- function(
  monitoring_location_id = NA_character_,
  monitoring_location_obs_number = NA_character_,
  material = NA_character_,
  properties = NA_character_,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "constructionObs"
  # Check for mandatory arguments:
  if(is.na(monitoring_location_id)){
    stop("monitoring_location_id is a mandatory argument.")
  }
  
  rlang::check_dots_empty()
  args <- mget(names(formals()))

  return_list <- get_ogc_data(
    args = args,
    output_id = "id",
    service = service,
    base = "NGWMN"
  )

  return(return_list)
}
