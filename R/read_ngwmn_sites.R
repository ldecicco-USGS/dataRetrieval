#' Get NGWMN Site Data
#'
#' @description `r get_description("sites", base = "NGWMN")`
#'
#' @export
#' @param monitoring_location_id
#' `r get_ogc_params("sites", base = "NGWMN")$monitoring_location_id$description`
#' @param agency_code
#' `r get_ogc_params("sites", base = "NGWMN")$agency_code$description`
#' @param monitoring_location_number
#' `r get_ogc_params("sites", base = "NGWMN")$monitoring_location_number$description`
#' @param altitude
#' `r get_ogc_params("sites", base = "NGWMN")$altitude$description`
#' @param national_aquifer_code
#' `r get_ogc_params("sites", base = "NGWMN")$national_aquifer_code$description`
#' @param national_aquifer_description
#' `r get_ogc_params("sites", base = "NGWMN")$national_aquifer_description$description`
#' @param country_code
#' `r get_ogc_params("sites", base = "NGWMN")$country_code$description`
#' @param country_name
#' `r get_ogc_params("sites", base = "NGWMN")$country_name$description`
#' @param state_name
#' `r get_ogc_params("sites", base = "NGWMN")$state_name$description`
#' @param county_name
#' `r get_ogc_params("sites", base = "NGWMN")$county_name$description`
#' @param aquifer_name
#' `r get_ogc_params("sites", base = "NGWMN")$aquifer_name$description`
#' @param site_type
#' `r get_ogc_params("sites", base = "NGWMN")$site_type$description`
#' @param aquifer_type_code
#' `r get_ogc_params("sites", base = "NGWMN")$aquifer_type_code$description`
#' @param qw_sys_name
#' `r get_ogc_params("sites", base = "NGWMN")$qw_sys_name$description`
#' @param qw_sn_flag
#' `r get_ogc_params("sites", base = "NGWMN")$qw_sn_flag$description`
#' @param qw_baseline_flag
#' `r get_ogc_params("sites", base = "NGWMN")$qw_baseline_flag$description`
#' @param qw_well_chars
#' `r get_ogc_params("sites", base = "NGWMN")$qw_well_chars$description`
#' @param qw_well_type
#' `r get_ogc_params("sites", base = "NGWMN")$qw_well_type$description`
#' @param qw_well_purpose
#' `r get_ogc_params("sites", base = "NGWMN")$qw_well_purpose$description`
#' @param wl_sys_name
#' `r get_ogc_params("sites", base = "NGWMN")$wl_sys_name$description`
#' @param wl_sn_flag
#' `r get_ogc_params("sites", base = "NGWMN")$wl_sn_flag$description`
#' @param wl_baseline_flag
#' `r get_ogc_params("sites", base = "NGWMN")$wl_baseline_flag$description`
#' @param wl_well_chars
#' `r get_ogc_params("sites", base = "NGWMN")$wl_well_chars$description`
#' @param wl_well_type
#' `r get_ogc_params("sites", base = "NGWMN")$wl_well_type$description`
#' @param wl_well_purpose
#' `r get_ogc_params("sites", base = "NGWMN")$wl_well_purpose$description`
#'
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("sites", base = "NGWMN")`.
#' The default (`NA`) will return all columns of the data.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param limit The optional limit parameter is used to control the subset of the
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution.
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' ngwmn_sites <- read_ngwmn_sites(state_name = "Wisconsin")
#'
#' ngwml_providers2 <- read_ngwmn_sites(state = c("WI", "MN"))
#'
#' org_type <- read_ngwmn_sites(organization_type = "NWIS")
#'
#' }
read_ngwmn_sites <- function(
  monitoring_location_id = NA_character_,
  agency_code = NA_character_,
  monitoring_location_number = NA_character_,
  altitude = NA_character_,
  national_aquifer_code = NA_character_,
  national_aquifer_description = NA_character_,
  country_code = NA_character_,
  country_name = NA_character_,
  state_name = NA_character_,
  county_name = NA_character_,
  aquifer_name = NA_character_,
  site_type = NA_character_,
  aquifer_type_code = NA_character_,
  qw_sys_name = NA_character_,
  qw_sn_flag = NA_character_,
  qw_baseline_flag = NA_character_,
  qw_well_chars = NA_character_,
  qw_well_type = NA_character_,
  qw_well_purpose = NA_character_,
  wl_sys_name = NA_character_,
  wl_sn_flag = NA_character_,
  wl_baseline_flag = NA_character_,
  wl_well_chars = NA_character_,
  wl_well_type = NA_character_,
  wl_well_purpose = NA_character_,
  bbox = NA,
  properties = NA_character_,
  skipGeometry = FALSE,
  limit = NA,
  no_paging = FALSE
) {
  service <- "sites"
  rlang::check_dots_empty()
  args <- mget(names(formals()))

  return_list <- get_ngwmn_data(args, service)

  return(return_list)
}
