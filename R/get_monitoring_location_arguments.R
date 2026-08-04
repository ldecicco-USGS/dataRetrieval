#' Get Monitoring Location Arguments
#'
#' Many read_waterdata functions have a long list of arguments that can be used
#' to find sites that have data. Users can use this function to create a list
#' of possible arguments that can be used as input to the `monitoring_location_arguments`
#' argument (found in many of the read_waterdata functions). This function
#' also is used to check that user supplied available parameters to their queries
#' when using the `monitoring_location_arguments` argument.
#'
#' @param service Endpoint to check arguments against. Possible values are
#' "daily", "latest-continuous", "field-measurements", "latest-daily",
#' "latest-field-measurements", "continuous", "peaks".
#' @param agency_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$agency_code$description`
#' @param agency_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$agency_name$description`
#' @param monitoring_location_number `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$monitoring_location_number$description`
#' @param monitoring_location_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$monitoring_location_name$description`
#' @param district_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$district_code$description`
#' @param country_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$country_code$description`
#' @param country_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$country_name$description`
#' @param state_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$state_code$description`
#' @param state_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$state_name$description`
#' @param county_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$county_code$description`
#' @param county_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$county_name$description`
#' @param minor_civil_division_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$minor_civil_division_code$description`
#' @param site_type_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$site_type_code$description`
#' @param site_type `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$site_type$description`
#' @param hydrologic_unit_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$hydrologic_unit_code$description`
#' @param basin_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$basin_code$description`
#' @param altitude `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$altitude$description`
#' @param altitude_accuracy `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$altitude_accuracy$description`
#' @param altitude_method_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$altitude_method_code$description`
#' @param altitude_method_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$altitude_method_name$description`
#' @param vertical_datum `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$vertical_datum$description`
#' @param vertical_datum_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$vertical_datum_name$description`
#' @param horizontal_positional_accuracy_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$horizontal_positional_accuracy_code$description`
#' @param horizontal_positional_accuracy `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$agency_code$description`
#' @param horizontal_position_method_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$horizontal_position_method_code$description`
#' @param horizontal_position_method_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$horizontal_position_method_name$description`
#' @param original_horizontal_datum `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$original_horizontal_datum$description`
#' @param original_horizontal_datum_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$original_horizontal_datum_name$description`
#' @param drainage_area `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$drainage_area$description`
#' @param contributing_drainage_area `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$contributing_drainage_area$description`
#' @param time_zone_abbreviation `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$time_zone_abbreviation$description`
#' @param uses_daylight_savings `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$agency_code$description`
#' @param construction_date `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$construction_date$description`
#' @param aquifer_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$aquifer_code$description`
#' @param national_aquifer_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$national_aquifer_code$description`
#' @param aquifer_type_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$agency_code$description`
#' @param well_constructed_depth `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$well_constructed_depth$description`
#' @param hole_constructed_depth `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$hole_constructed_depth$description`
#' @param depth_source_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$depth_source_code$description`
#' @param data_gap_interval `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$data_gap_interval$description`
#' @param \dots Not used. Used to make sure the user isn't passing in arguments that aren't available.
#' @param check_arguments Logical. Used to decide if the arguments passed in are available or not. The
#' default is `FALSE`. Using `TRUE` will make two calls to the API, so if you are concerned
#' with minimizing API calls, keep this value as `FALSE`. If you are concerned with
#' making sure your calls are accepted by the service, set to `TRUE`.
#' @export
#' @examples
#'
#' get_monitoring_location_arguments(service = "daily", agency_name = "USGS")
#' get_monitoring_location_arguments(service = "field-measurements",
#'                                    state_name = c("Ohio", "Wisconsin"))
get_monitoring_location_arguments <- function(
  service = "daily",
  agency_code = NA_character_,
  agency_name = NA_character_,
  monitoring_location_number = NA_character_,
  monitoring_location_name = NA_character_,
  district_code = NA_character_,
  country_code = NA_character_,
  country_name = NA_character_,
  state_code = NA_character_,
  state_name = NA_character_,
  county_code = NA_character_,
  county_name = NA_character_,
  minor_civil_division_code = NA_character_,
  site_type_code = NA_character_,
  site_type = NA_character_,
  hydrologic_unit_code = NA_character_,
  basin_code = NA_character_,
  altitude = NA_character_,
  altitude_accuracy = NA_character_,
  altitude_method_code = NA_character_,
  altitude_method_name = NA_character_,
  vertical_datum = NA_character_,
  vertical_datum_name = NA_character_,
  horizontal_positional_accuracy_code = NA_character_,
  horizontal_positional_accuracy = NA_character_,
  horizontal_position_method_code = NA_character_,
  horizontal_position_method_name = NA_character_,
  original_horizontal_datum = NA_character_,
  original_horizontal_datum_name = NA_character_,
  drainage_area = NA_character_,
  contributing_drainage_area = NA_character_,
  time_zone_abbreviation = NA_character_,
  uses_daylight_savings = NA_character_,
  construction_date = NA_character_,
  aquifer_code = NA_character_,
  national_aquifer_code = NA_character_,
  aquifer_type_code = NA_character_,
  well_constructed_depth = NA_character_,
  hole_constructed_depth = NA_character_,
  depth_source_code = NA_character_,
  data_gap_interval = NA_character_,
  ...,
  check_arguments = FALSE
) {
  # Make sure no one passes in an argument that's not suppose to be there
  rlang::check_dots_empty()
  service_with_extra_queries <- c(
    "daily",
    "latest-continuous",
    "field-measurements",
    "latest-daily",
    "latest-field-measurements",
    "continuous",
    "peaks"
  )
  match.arg(service, choices = service_with_extra_queries, several.ok = FALSE)

  args <- mget(names(formals()))
  args[["..."]] <- NULL
  args[["service"]] <- NULL
  args[["check_arguments"]] <- NULL

  n_args <- names(args)

  lapply(n_args, function(x) {
    check_character(args[[x]], x)
  })

  if (check_arguments) {
    # think about if we want this as an option
    # don't want to waste a lot of hits to the API if people are
    # running into their token limits
    properties <- dataRetrieval::get_ogc_params(service)
    queryables <- check_OGC_requests(endpoint = service, type = "queryables")
    non_returned <- queryables$properties[
      !names(queryables$properties) %in% names(properties)
    ]

    args_not_available <- args[!names(args) %in% names(non_returned)]
    if (length(args_not_available) > 0) {
      message(
        "The ",
        service,
        " service doesn't accept: ",
        paste0(names(args_not_available), collapse = ", "),
        "."
      )
      message("Those arguments will be ignored.")
    }

    args <- args[names(args) %in% names(non_returned)]
  }

  return(args)
}

cleanup_arguments <- function(args, monitoring_location_arguments, service) {
  query_args <- do.call(
    get_monitoring_location_arguments,
    c(monitoring_location_arguments, service = service)
  )

  args[["monitoring_location_arguments"]] <- NULL
  args[["..."]] <- NULL
  args <- c(args, query_args)

  return(args)
}

check_character <- function(x, name) {
  if (!is.null(x)) {
    if (all(!is.na(x) & !is.character(x))) {
      stop(paste(name, "should be a character"))
    }
  }
}

check_numeric <- function(x, name) {
  if (!is.null(x)) {
    if (all(!is.na(x) & !is.numeric(x))) {
      stop(paste(name, "should be a numeric"))
    }
  }
}

check_integer <- function(x, name) {
  if (!is.null(x)) {
    if (all(!is.na(x) & !is.numeric(x))) {
      stop(paste(name, "should be a integer"))
    }
  }
}

check_logical <- function(x, name) {
  if (!is.null(x)) {
    if (all(!is.na(x) & !is.logical(x))) {
      stop(paste(name, "should be a logical (TRUE/FALSE)"))
    }
  }
}
