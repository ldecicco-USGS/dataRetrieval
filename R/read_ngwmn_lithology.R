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
#' @param \dots Not used. Included to help differentiate official NGWMN API arguments
#' from more seldom used, optional dataRetrieval-specific arguments.
#' @inheritParams check_arguments_non_api
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "AKDNR-535134236016630"
#' ngwmn_lith <- read_ngwmn_lithology(monitoring_location_id = site)
#'
#'
#' sites <- c("ISWS-P428197",
#'            "AKDNR-535143966816631",
#'            "AKDNR-535134236016630")
#' ngwml_lith_sites <- read_ngwmn_lithology(monitoring_location_id = sites)
#'
#' }
read_ngwmn_lithology <- function(
  monitoring_location_id = NA_character_,
  monitoring_location_obs_number = NA_character_,
  properties = NA_character_,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "lithologyObs"

  # Check for mandatory arguments:
  if (all(is.na(monitoring_location_id))) {
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
