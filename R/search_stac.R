#' Search STAC services
#'
#' The USGS Water Data STAC API provides access to USGS water data using the
#' SpatioTemporal Asset Catalog (STAC) family of standards. These APIs will
#' provide access to a wide array of USGS water data stored in native formats.
#' That means this API will provide you with links to where the data you're
#' interested in is stored, rather than providing you the data directly as a
#'  result of your query.
#'
#' @param \dots filter arguments to filter on
#' @param collections Could be "daily", "continuous", "ratings".
#' @param limit description
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth).
#' @param custom_filter Custom STAC filter. Default is NA. Uses CQL2 formatting.
#' @return list of features available in the STAC service.
#' @param chunk_size Number of monitoring_location_ids to chunk requests into.
#' The default for functions is 50.
#' @export
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' monitoring_location_id <- c("USGS-01435000", "USGS-05406360")
#'
#' features_2_sites <- search_stac(
#'     monitoring_location_id = monitoring_location_id,
#'     collections = c("daily", "continuous"))
#'
#' features_county <- search_stac(
#'     county_name = "Dane County",
#'     state_name = "Wisconsin",
#'     site_type = "Stream",
#'     #start_datetime = c(NA, "2000-01-01"), # starts before 2000
#'     end_datetime = c("2026-01-01", NA), # ends after 2026
#'     collections = "daily"
#' )
#'
#' # HUCs return all HUCs that start with the entered value
#' features_huc <- search_stac(
#'   hydrologic_unit_code = "07090002"
#' )
#'
#' unlist(unique(sapply(features_huc, function(x)
#'     x[["properties"]]["hydrologic_unit_code"])))
#'
#' features_huc2 <- search_stac(
#'   hydrologic_unit_code = c("0709000203", "0709000204")
#' )
#'
#' unlist(unique(sapply(features_huc2, function(x)
#'     x[["properties"]]["hydrologic_unit_code"])))
#'
#' # Paging example:
#' features_OH <- search_stac(
#'   state_name = "Ohio",
#'   limit = 500
#' )
#'
#'}
search_stac <- function(
  ...,
  collections = c("daily", "continuous", "ratings"),
  bbox = NA,
  limit = getOption("dataRetrieval.limit"),
  custom_filter = NA_character_,
  chunk_size = 50
) {
  match.arg(
    collections,
    choices = c("daily", "continuous", "ratings"),
    several.ok = TRUE
  )

  filter_args <- list(...)

  if (
    !is.na(chunk_size) &
      length(filter_args[["monitoring_location_id"]]) > chunk_size
  ) {
    ml_splits <- split(
      filter_args[["monitoring_location_id"]],
      ceiling(seq_along(filter_args[["monitoring_location_id"]]) / chunk_size)
    )

    rl <- lapply(ml_splits, function(x) {
      do.call(
        search_stac,
        list(
          monitoring_location_id = x,
          collections = collections,
          bbox = bbox,
          limit = limit,
          custom_filter = custom_filter,
          chunk_size = chunk_size
        )
      )
    })

    rl_filtered <- rl[sapply(rl, length) > 0]

    return_list <- unlist(rl_filtered, recursive = FALSE)
    return(return_list)
  } else {
    request <- httr2::request("https://api.waterdata.usgs.gov/stac") |>
      httr2::req_url_path_append(getOption("dataRetrieval.api_version_stac")) |>
      httr2::req_url_path_append("search")

    if (!all(is.na(collections))) {
      request <- request |>
        httr2::req_url_query(collections = collections, .multi = "comma")
    }

    dt_index <- grep("datetime|updated", names(filter_args))
    like_index <- grep("hydrologic_unit_code", names(filter_args))

    if (length(dt_index) > 0) {
      in_filters <- filter_args[-dt_index]
    } else {
      in_filters <- filter_args
    }

    if (length(like_index) > 0) {
      in_filters <- in_filters[-grep("hydrologic_unit_code", names(in_filters))]
    } else {
      in_filters <- in_filters
    }

    in_filters <- do.call(add_in_filter, args = in_filters)
    date_filters <- do.call(add_datetime_filter, args = filter_args[dt_index])
    like_filters <- do.call(add_like_filter, args = filter_args[like_index])

    if (!all(is.na(date_filters))) {
      request <- httr2::req_url_query(
        request,
        !!!date_filters
      )
    }

    all_filters <- c(in_filters, custom_filter)

    if (all(!is.na(bbox))) {
      request <- httr2::req_url_query(
        request,
        bbox = as.numeric(bbox),
        .multi = "comma"
      )
    }

    if (!all(is.na(all_filters))) {
      all_filters <- all_filters[!is.na(all_filters)]
      filters <- paste0(all_filters, collapse = " AND ")

      if (all(like_filters != "")) {
        filters <- paste0(filters, " OR ", like_filters)
      }

      request <- request |>
        httr2::req_url_query(filter = filters) |>
        httr2::req_url_query(`filter-lang` = "cql2-text")
    } else {
      if (length(like_filters) > 0) {
        filters <- like_filters
      }

      request <- request |>
        httr2::req_url_query(filter = filters) |>
        httr2::req_url_query(`filter-lang` = "cql2-text")
    }

    request <- request |>
      httr2::req_url_query(limit = limit) |>
      basic_request(token = "STAC")

    message("Requesting:\n", request$url)

    resps <- httr2::req_perform_iterative(
      request,
      next_req = next_req_url,
      max_reqs = Inf,
      on_error = "stop"
    )

    failures <- resps |> httr2::resps_failures()

    features <- resps |>
      httr2::resps_successes() |>
      httr2::resps_data(\(resp) get_stac_features(resp))

    return(features)
  }
}


get_stac_features <- function(resp) {
  features <- httr2::resp_body_json(resp)[["features"]]
  return(features)
}


add_datetime_filter <- function(...) {
  args <- list(...)
  out <- c()
  for (i in seq_along(args)) {
    if (any(grepl("P", args[[i]]))) {
      stop(
        "Periods are not supported in datetime argument in the STAC service."
      )
    }
    datetime <- format_api_dates(args[[i]], date = FALSE)
    out <- c(out, datetime)
  }
  names(out) <- names(args)
  return(out)
}

add_in_filter <- function(...) {
  args <- list(...)
  out <- c()
  for (i in seq_along(args)) {
    if (length(args[[i]]) > 1) {
      vals <- paste0(args[[i]], collapse = "', '")

      out <- c(out, sprintf("%s IN ('%s')", names(args[i]), vals))
    } else {
      vals <- args[[i]]
      out <- c(out, sprintf("%s = '%s'", names(args[i]), vals))
    }
  }

  return(out)
}

add_like_filter <- function(...) {
  args <- list(...)
  out <- c()
  for (i in seq_along(args)) {
    if (length(args[[i]]) > 1) {
      for (j in seq_along(args[[i]])) {
        vals <- args[[i]][j]
        like_single <- sprintf("%s LIKE '%s%%'", names(args[i]), vals)
        out <- c(out, like_single)
      }
    } else {
      vals <- args[[i]]
      out <- c(out, sprintf("%s LIKE '%s%%'", names(args[i]), vals))
    }
  }

  out <- paste0(out, collapse = " OR ")
  return(out)
}
