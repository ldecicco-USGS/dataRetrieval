#' Get Latest USGS field Data
#'
#' @description `r get_description("latest-field-measurements")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("latest-field-measurements")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("latest-field-measurements")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param statistic_id `r get_ogc_params("latest-field-measurements")$statistic_id`
#' Multiple statistic_ids can be requested as a character vector.
#' @param time `r get_ogc_params("latest-field-measurements")$time`
#'
#' See also Details below for more information.
#' @param value `r get_ogc_params("latest-field-measurements")$value`
#' @param unit_of_measure `r get_ogc_params("latest-field-measurements")$unit_of_measure`
#' @param approval_status `r get_ogc_params("latest-field-measurements")$approval_status`
#' @param last_modified `r get_ogc_params("latest-field-measurements")$last_modified`
#'
#' See also Details below for more information.
#' @param time_series_id `r get_ogc_params("latest-field-measurements")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param qualifier `r get_ogc_params("latest-field-measurements")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("latest-field-measurements", "latest_field_id")`.
#' The default (`NA`) will return all columns of the data.
#'
#' @inheritParams check_arguments_api
#' @inheritParams check_arguments_non_api
#' @inherit read_waterdata_continuous details
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "USGS-01435000"
#'
#' field_data_sf <- read_waterdata_latest_field(monitoring_location_id = site)
#'
#' dv_data_trim <- read_waterdata_latest_field(monitoring_location_id = site,
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"))
#'
#' field_data <- read_waterdata_latest_field(monitoring_location_id = site,
#'                            skipGeometry = TRUE)
#'
#' multi_site <- read_waterdata_latest_field(monitoring_location_id =  c("USGS-01435000",
#'                                                                       "USGS-14202650"))
#'
#' }
read_waterdata_latest_field <- function(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  statistic_id = NA_character_,
  properties = NA_character_,
  time_series_id = NA_character_,
  approval_status = NA_character_,
  unit_of_measure = NA_character_,
  qualifier = NA_character_,
  value = NA,
  last_modified = NA_character_,
  skipGeometry = NA,
  time = NA_character_,
  bbox = NA,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  limit = getOption("dataRetrieval.limit"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_meta"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "latest-field-measurements"
  output_id <- "latest_field_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
