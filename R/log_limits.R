#' Check STAC limits
#'
#' If using a STAC_USGS_PAT token, retrieve the remaining requests for the hour.
#'
#' @export
#' @return message on how many requests are for this hour.
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' log_stac_limit()
#'
#' }
log_stac_limit <- function() {
  resp <- httr2::request("https://api.waterdata.usgs.gov/stac") |>
    httr2::req_url_path_append(getOption("dataRetrieval.api_version_stac")) |>
    httr2::req_url_path_append("search") |>
    httr2::req_url_query(limit = 1) |>
    basic_request(token = "STAC") |>
    httr2::req_perform()

  header_info <- httr2::resp_headers(resp)
  if ("x-ratelimit-remaining" %in% names(header_info)) {
    message(
      "Remaining STAC search requests this hour:",
      header_info$`x-ratelimit-remaining`
    )
  }

  features <- httr2::resp_body_json(resp)[["features"]]

  resp2 <- httr2::request(features[[1]][["assets"]][["data"]][["href"]]) |>
    basic_request(token = "STAC") |>
    httr2::req_perform()

  header2_info <- httr2::resp_headers(resp2)
  if ("x-ratelimit-remaining" %in% names(header2_info)) {
    message(
      "Remaining STAC files requests this hour:",
      header2_info$`x-ratelimit-remaining`
    )
  }
  invisible(NULL)
}
