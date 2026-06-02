#' Get NGWMN Water Level Data
#'
#' @description `r dataRetrieval:::get_description("waterLevelObs", base = "NGWMN")`
#'
#' @export
#' @param monitoring_location_id
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$monitoring_location_id$description`
#' @param monitoring_location_obs_number
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$monitoring_location_obs_number$description`
#' @param sample_time
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$sample_time$description`
#' See also Details below for more information.
#'
#' @param data_provided_by
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$data_provided_by$description`
#' @param water_depth_below_land_surface_ft
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$water_depth_below_land_surface_ft$description`
#' @param water_level_above_site_datum_ft
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$water_level_above_site_datum_ft$description`
#' @param monitoring_location_vertical_datum
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$monitoring_location_vertical_datum$description`
#' @param water_level_above_navd88_ft
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$water_level_above_navd88_ft$description`
#'
#' @param datetime
#' `r get_ogc_params("waterLevelObs", base = "NGWMN")$sample_time$descriptiond`
#' Multiple time_series_ids can be requested as a character vector.
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("waterLevelObs", base = "NGWMN")`.
#' The default (`NA`) will return all columns of the data.
#'
#' @inheritParams check_arguments_non_api
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "USGS-272838082142201"
#' ngwmn_wl <- read_ngwmn_water_level(monitoring_location_id = site)
#'
#' ngwmn_wl_sub <- read_ngwmn_water_level(monitoring_location_id = site,
#'                      monitoring_location_obs_number = 1:10)
#'
#' ngwml_wl_time2 <- read_ngwmn_water_level(monitoring_location_id = site,
#'                      datetime = c("2022-01-01", "2024-01-01"))
#'
#' sites <- c("USGS-272838082142201", "USGS-404159100494601",
#'            "USGS-401216080362703", "MBMG-702934")
#' ngwml_wl_sites <- read_ngwmn_water_level(monitoring_location_id = sites)
#'
#' }
read_ngwmn_water_level <- function(
  monitoring_location_id = NA_character_,
  monitoring_location_obs_number = NA_character_,
  sample_time = NA_character_,
  data_provided_by = NA_character_,
  water_depth_below_land_surface_ft = NA_character_,
  water_level_above_site_datum_ft = NA_character_,
  monitoring_location_vertical_datum = NA_character_,
  water_level_above_navd88_ft = NA_character_,
  properties = NA_character_,
  datetime = NA_character_,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "waterLevelObs"
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
