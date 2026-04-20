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

#' @param limit The optional limit parameter is used to control the subset of the
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution.
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
  limit = NA,
  no_paging = FALSE
) {
  service <- "constructionObs"

  args <- mget(names(formals()))

  return_list <- get_ngwmn_data(args, service)

  return(return_list)
}
