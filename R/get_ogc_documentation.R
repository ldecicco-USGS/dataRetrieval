#' Create service descriptions dynamically
#'
#' This function populates the parameter descriptions.
#'
#' @param service Character, can be any of the endpoints
#' @param base Character, can be "OGC" or "NGWMN"
#' @return list
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' ml_desc <- dataRetrieval:::get_description("monitoring-locations")
#' ml_desc
#'
#' wl_desc <- dataRetrieval:::get_description("waterLevelObs", base = "NGWMN")
#' wl_desc
#' }
#'
get_description <- function(service, base = "OGC") {
  query_ret <- get_collection(base)

  tags <- query_ret[["tags"]]

  service_index <- which(sapply(tags, function(x) {
    x$name == service
  }))

  tags[[service_index]][["description"]]
}

#' Get collection response
#'
#'
#' @return httr2 response
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' collection <- dataRetrieval:::get_collection()
#' collection
#' }
#'
get_collection <- function(base = "OGC") {
  check_collections <- base_url(base = base) |>
    httr2::req_url_path_append("openapi") |>
    httr2::req_url_query(f = "html#/server/getCollections")

  check_endpoints_req <- basic_request(check_collections)

  query_ret <- httr2::req_perform(check_endpoints_req) |>
    httr2::resp_body_json()

  return(query_ret)
}

#' Get parameter descriptions
#'
#' This function returns a list of properties available from an endpoint. When
#' available, it will also contain a description.
#'
#' @param service Character, can be any of the USGS Waterdata API endpoints or collections.
#' @param base Either "OGC" for waterdata, or "NGWMN" for National Groundwater Monitoring Network.
#' @return list
#' @export
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' ml <- dataRetrieval::get_ogc_params("monitoring-locations")
#' ml$national_aquifer_code
#' }
#'
get_ogc_params <- function(service, base = "OGC") {
  check_queryables_req <- base_url(base) |>
    httr2::req_url_path_append("collections") |>
    httr2::req_url_path_append(service) |>
    httr2::req_url_path_append("schema") |>
    basic_request()

  query_ret <- httr2::req_perform(check_queryables_req) |>
    httr2::resp_body_json()

  if (base == "OGC") {
    params <- sapply(query_ret$properties, function(x) x[["description"]])
  } else if (base == "NGWMN") {
    params <- query_ret$properties
  }

  params
}


#' Get property list
#'
#' This function gets a list of available properties, and
#' renames the id column to what is used in dataRetrieval.
#'
#' @param service Character, can be any of the endpoints
#' @param output_id Character, dataRetrieval output name
#' @param base Character, either "OGC" or "NGWMN"
#' @return list
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' dataRetrieval:::get_properties_for_docs("monitoring-locations",
#'                                         "monitoring_location_id")
#'
#' dataRetrieval:::get_properties_for_docs("waterLevelObs",
#'                                         base = "NGWMN")
#'
#' }
#'
get_properties_for_docs <- function(service, output_id = NA, base = "OGC") {
  schema <- check_OGC_requests(endpoint = service, type = "schema", base = base)

  properties <- names(schema$properties)
  if (!is.na(output_id)) {
    properties[properties == "id"] <- output_id
  }

  return(paste(properties, collapse = ", "))
}

#' Check OGC requests
#'
#' @param endpoint Character, can be any existing collection
#' @param type Character, can be "queryables", "schema"
#' @param base Character, can be "OGC" or "NGWMN"
#' @export
#' @keywords internal
#' @return list
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' dv_queryables <- check_OGC_requests(endpoint = "daily",
#'                                 type = "queryables")
#' dv_schema <- check_OGC_requests(endpoint = "daily",
#'                             type = "schema")
#' ts_meta_queryables <- check_OGC_requests(endpoint = "time-series-metadata",
#'                                 type = "queryables")
#' ts_meta_schema <- check_OGC_requests(endpoint = "time-series-metadata",
#'                                 type = "schema")
#'
#' ngwml <- check_OGC_requests(endpoint = "waterLevelObs",
#'                             type = "schema",
#'                             base = "NGWMN")
#' }
check_OGC_requests <- function(
  endpoint = "daily",
  type = "queryables",
  base = "OGC"
) {
  match.arg(type, c("queryables", "schema"))
  match.arg(base, c("OGC", "NGWMN"))

  if (base == "OGC") {
    match.arg(endpoint, c(pkg.env$api_endpoints, pkg.env$metadata))
  }

  req <- base_url(base) |>
    httr2::req_url_path_append("collections") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_path_append(type) |>
    basic_request()

  query_ret <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  return(query_ret)
}
