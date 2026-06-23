#' Generalized NGWMN API retrieval function
#'
#' Function that allows complex CQL queries on National Groundwater
#' Monitoring Network API.
#' See <https://api.waterdata.usgs.gov/docs/ogcapi/complex-queries/>
#' for more information.
#'
#' @export
#' @param service character, can be any existing collection. Can be:
#' "providers", "constructionObs", "waterLevelObs", "sites", or "lithologyObs".
#' @param CQL A string in a Common Query Language format.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @param \dots Additional arguments to send to the request.
#' @inheritParams check_arguments_non_api
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' cql <- '{
#' "op": "and",
#' "args": [
#'   {
#'     "op": "between",
#'     "args": [
#'       { "property": "water_level_above_navd88_ft" },
#'       [ "30.00", "80.00" ]
#'     ]
#'   },
#'  {
#'     "op": "in",
#'     "args": [
#'       { "property": "monitoring_location_id" },
#'       [ "USGS-272838082142201", "USGS-404159100494601", "USGS-401216080362703" ]
#'     ]
#'   }
#' ]
#' }'
#'
#' wl_data <- read_ngwmn(service = "waterLevelObs",
#'                       CQL = cql)
#'
#'
#' }
read_ngwmn <- function(
    service,
    CQL,
    ...,
    convertType = getOption("dataRetrieval.convertType"),
    limit = getOption("dataRetrieval.limit"),
    attach_request = getOption("dataRetrieval.attach_request")
) {
  match.arg(service, c("providers", "constructionObs",
                       "waterLevelObs", "sites",
                       "lithologyObs"))
  
  args <- list(...)
  
  args[["convertType"]] <- convertType
  args[["limit"]] <- limit
  args[["attach_request"]] <- attach_request
  args[["bbox"]] <- NA
  args[["no_paging"]] <- FALSE # drops id if TRUE
  args[["chunk_size"]] <- NA # Chunking doesn't make sense.
  
  if (!"properties" %in% names(args)) {
    args[["properties"]] <- NA_character_
  }
  
  args[["output_id"]] <- "id"
  args[["base"]] <- "NGWMN"
  args[["service"]] <- service
  
  data_req <- suppressWarnings(do.call(construct_api_requests, args))
  
  data_req <- data_req |>
    httr2::req_headers(`Content-Type` = "application/query-cql-json") |>
    httr2::req_body_raw(CQL)
  
  return_list <- walk_pages(data_req)
  
  return_list <- deal_with_empty(return_list = return_list,
                                 properties = args[["properties"]],
                                 service = service,
                                 skipGeometry = isTRUE(args[["skipGeometry"]]),
                                 convertType = args[["convertType"]],
                                 no_paging = FALSE, 
                                 base = "NGWMN"
  )
    
  return_list <- rejigger_cols(
    return_list,
    args[["properties"]],
    args[["output_id"]]
  )
  
  if (convertType) {
    return_list <- cleanup_cols(return_list, service)
    return_list <- order_results(return_list)
    return_list <- move_id_col(return_list, args[["output_id"]])
  }
  
  return(return_list)
}
